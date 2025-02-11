package com.scala.axidraw

import com.scala.axidraw.Hershey.{Font, Glyph, RenderingOptions}
import io.circe.generic.semiauto._
import io.circe.{Decoder, parser}

import java.io.FileNotFoundException
import scala.annotation.tailrec
import scala.io.Source
import scala.util.{Failure, Success, Try}
import scala.xml.{Node, XML}

/**
  * Клас Hershey являє собою публічну обгортку для завантаженого шрифту.
  * Він дозволяє рендерити текст як векторні шляхи.
  *
  * @param font Внутрішнє представлення шрифту.
  */
class Hershey(val font: Font) {

  /**
    * Рендерить заданий текст у вигляді векторних шляхів.
    *
    * Метод послідовно обробляє кожний символ тексту, знаходить відповідний гліф у шрифті,
    * застосовує до нього трансформації згідно з переданими параметрами та об'єднує результуючі шляхи.
    *
    * @param text    Текст, який необхідно відобразити.
    * @param options Опції рендерингу, що включають масштаб, відступ між символами та початкову точку.
    * @return Paths – векторні шляхи, що представляють рендерений текст.
    */
  def renderText(text: String, options: RenderingOptions = RenderingOptions()): Paths =
    text
      .foldLeft((Paths.empty, options.origin)) {
        case ((acc, cursor), char) =>
          // Спроба знайти гліф для символу; якщо гліф відсутній, використовується спеціальний "missing" гліф.
          val glyphOpt = font.glyphs.get(char.toString).orElse(font.glyphs.get("missing"))
          glyphOpt.fold((acc, cursor)) { glyph =>
            val transformed = transformGlyph(glyph, cursor, options)
            val newCursor = updateCursor(cursor, glyph, options)
            (acc.combine(transformed), newCursor)
          }
      }
      ._1

  /**
    * Виконує трансформацію гліфа шляхом переміщення та масштабування.
    *
    * @param glyph   Гліф, який необхідно трансформувати.
    * @param cursor  Початкова позиція, з якої починається рендеринг гліфа.
    * @param options Опції трансформації, що включають масштабування.
    * @return Paths – векторні шляхи після застосування трансформації.
    */
  private def transformGlyph(glyph: Glyph, cursor: Point, options: RenderingOptions): Paths =
    glyph.paths
      .translate(cursor.x, cursor.y)
      .scale(options.scale)

  /**
    * Оновлює позицію курсора після рендерингу гліфа.
    *
    * Після відображення гліфа позиція курсора коригується з урахуванням ширини символу та додаткового відступу.
    *
    * @param cursor  Поточна позиція курсора.
    * @param glyph   Гліф, що був відрендерений.
    * @param options Опції рендерингу, які містять масштаб та відступ між символами.
    * @return Point – нова позиція курсора.
    */
  private def updateCursor(cursor: Point, glyph: Glyph, options: RenderingOptions): Point =
    cursor.copy(
      x = cursor.x + (glyph.advanceWidth * options.scale) + options.charSpacing
    )
}

/**
  * Об'єкт Hershey забезпечує функціональність для роботи з векторними шрифтами Hershey у форматі SVG.
  * Він надає можливості для ініціалізації системи шрифтів, завантаження окремих шрифтів та рендерингу тексту
  * у вигляді векторних шляхів.
  */
object Hershey {

  /**
    * Модель шрифту, що містить векторні гліфи.
    *
    * @param fontId   Ідентифікатор шрифту.
    * @param fontFace Метрики шрифту, що визначають його основні характеристики.
    * @param glyphs   Відповідність між символами (ключ – Unicode символ або назва гліфа) та гліфами.
    */
  case class Font(
    fontId: String,
    fontFace: FontFace,
    glyphs: Map[String, Glyph]
  )

  /**
    * Метрики шрифту, що описують його основні параметри.
    *
    * @param familyName Назва сімейства шрифтів.
    * @param unitsPerEm Основна одиниця виміру розмірів шрифту.
    * @param ascent     Висота підйому шрифту.
    * @param descent    Глибина спуску шрифту.
    */
  case class FontFace(
    familyName: String,
    unitsPerEm: Int,
    ascent: Int,
    descent: Int
  )

  /**
    * Модель векторного гліфа.
    *
    * @param name         Назва гліфа.
    * @param unicode      Unicode символ, що відповідає гліфу (опціонально).
    * @param advanceWidth Ширина символу, що використовується для розрахунку позиції наступного символу.
    * @param paths        Векторні шляхи, які описують контур гліфа.
    */
  case class Glyph(
    name: String,
    unicode: Option[String],
    advanceWidth: Double,
    paths: Paths
  )

  /**
    * Внутрішня структура, що відображає поточний стан системи шрифтів.
    *
    * @param registry Реєстр шрифтів, який містить записи про доступні шрифти.
    * @param cache    Кеш завантажених шрифтів для прискорення доступу.
    */
  private case class FontState(
    registry: Map[String, FontEntry],
    cache: Map[String, Font]
  )

  /**
    * Запис про шрифт, що містить інформацію про розташування файлу та назву.
    *
    * @param file Ім'я файлу шрифту.
    * @param name Назва шрифту.
    */
  private case class FontEntry(file: String, name: String)

  /**
    * Декодер для автоматичного перетворення JSON-записів у об'єкти FontEntry.
    */
  implicit private val fontEntryDecoder: Decoder[FontEntry] = deriveDecoder

  /**
    * Початковий стан системи шрифтів, який є порожнім.
    */
  private val initialState: FontState = FontState(Map.empty, Map.empty)

  /**
    * Змінна, що містить поточний стан системи шрифтів. Використовується як implicit значення.
    */
  implicit private var implicitFontState: FontState = initialState

  /**
    * Ініціалізує систему шрифтів та оновлює внутрішній стан.
    *
    * Метод зчитує реєстр шрифтів з JSON-файлу за шляхом "/hershey/index.json",
    * виконує його парсинг та оновлює поточний стан системи шрифтів.
    *
    * @return Try[Unit] – успішне завершення ініціалізації або помилка, що виникла під час процесу.
    */
  def init(): Try[Unit] =
    for {
      indexJson <- readResource("/hershey/index.json")
      registry <- parseFontRegistry(indexJson)
    } yield {
      implicitFontState = FontState(registry, Map.empty)
    }

  /**
    * Фабричний метод для завантаження шрифту.
    *
    * @param fontName Ім'я шрифту для завантаження.
    * @return Try[Hershey] – успішно завантажений об'єкт Hershey або повідомлення про помилку.
    */
  def apply(fontName: String): Try[Hershey] =
    loadFont(fontName)(implicitFontState).flatMap {
      case (newState, Some(font)) =>
        implicitFontState = newState
        Success(new Hershey(font))
      case (newState, None) =>
        implicitFontState = newState
        Failure(new NoSuchElementException(s"Шрифт '$fontName' не знайдено"))
    }

  /**
    * Завантажує шрифт за вказаним ключем, використовуючи заданий стан системи шрифтів.
    *
    * @param fontKey Ключ шрифту, який необхідно завантажити.
    * @param state   Поточний стан системи шрифтів.
    * @return Try[(FontState, Option[Font])] – кортеж, що містить оновлений стан та опціонально завантажений шрифт.
    */
  private def loadFont(fontKey: String)(state: FontState): Try[(FontState, Option[Font])] =
    state.registry
      .get(fontKey)
      .fold[Try[(FontState, Option[Font])]](
        // Якщо шрифт не знайдено в реєстрі, повертаємо поточний стан та None.
        Success((state, None))
      ) { entry =>
        state.cache
          .get(fontKey)
          .fold(
            // Якщо шрифт відсутній у кеші, завантажуємо та виконуємо парсинг.
            loadAndParseFont(entry).map { font =>
              (state.copy(cache = state.cache + (fontKey -> font)), Some(font))
            }
          ) { font =>
            // Якщо шрифт уже присутній у кеші, повертаємо його.
            Success((state, Some(font)))
          }
      }

  /**
    * Зчитує вміст ресурсу за заданим шляхом.
    *
    * @param path Шлях до ресурсу.
    * @return Try[String] – вміст ресурсу або помилка, якщо ресурс не знайдено.
    */
  private def readResource(path: String): Try[String] = Try {
    val stream = getClass.getResourceAsStream(path)
    if (stream == null) throw new FileNotFoundException(path)
    Source.fromInputStream(stream).mkString
  }

  /**
    * Парсить JSON-рядок, що містить реєстр шрифтів.
    *
    * @param json Рядок у форматі JSON з описом доступних шрифтів.
    * @return Try[Map[String, FontEntry]] – мапа записів шрифтів або повідомлення про помилку парсингу.
    */
  private def parseFontRegistry(json: String): Try[Map[String, FontEntry]] =
    parser.parse(json).flatMap(_.as[Map[String, FontEntry]]).toTry

  /**
    * Завантажує шрифт з ресурсу та виконує його парсинг.
    *
    * @param entry Запис про шрифт, що містить шлях до файлу та назву.
    * @return Try[Font] – завантажений та розпарсений шрифт або повідомлення про помилку під час завантаження.
    */
  private def loadAndParseFont(entry: FontEntry): Try[Font] =
    for {
      content <- readResource(s"/hershey/${entry.file}")
      xml <- Try(XML.loadString(content))
      font <- parseFontXml(xml)
    } yield font

  /**
    * Парсить XML-документ шрифту та створює об'єкт Font.
    *
    * @param xml XML-документ, що містить інформацію про шрифт.
    * @return Try[Font] – розпарсений шрифт або повідомлення про помилку, якщо парсинг неможливий.
    */
  private def parseFontXml(xml: Node): Try[Font] =
    for {
      fontNode <- getSingleNode(xml, "font")
      fontFace <- parseFontFace(fontNode)
      glyphs <- parseGlyphs(fontNode, fontFace)
      missing <- parseMissingGlyph(fontNode, fontFace)
    } yield {
      val combined = glyphs ++ missing
      val allGlyphs = combined.withDefault { key =>
        combined.getOrElse(
          "missing",
          throw new NoSuchElementException(s"Гліф для ключа '$key' не знайдено, а missing glyph не визначено")
        )
      }
      Font(
        fontId = (fontNode \@ "id").trim,
        fontFace = fontFace,
        glyphs = allGlyphs
      )
    }

  /**
    * Знаходить єдиний XML-вузол за заданою назвою тегу.
    *
    * @param root Кореневий XML-вузол.
    * @param tag  Назва тегу, який необхідно знайти.
    * @return Try[Node] – знайдений вузол або повідомлення про помилку, якщо вузол відсутній.
    */
  private def getSingleNode(root: Node, tag: String): Try[Node] =
    (root \\ tag).headOption.fold[Try[Node]](
      Failure(new Exception(s"Відсутній елемент <$tag>"))
    )(Success(_))

  /**
    * Виконує парсинг метрик шрифту з XML-вузла.
    *
    * @param fontNode XML-вузол, що містить інформацію про шрифт.
    * @return Try[FontFace] – розпарсені метрики шрифту або повідомлення про помилку, якщо парсинг неможливий.
    */
  private def parseFontFace(fontNode: Node): Try[FontFace] = Try {
    val face = (fontNode \\ "font-face").head
    FontFace(
      familyName = face \@ "font-family",
      unitsPerEm = (face \@ "units-per-em").toInt,
      ascent = (face \@ "ascent").toInt,
      descent = (face \@ "descent").toInt
    )
  }

  /**
    * Парсить гліфи шрифту з XML-вузла.
    *
    * @param fontNode XML-вузол з інформацією про гліфи.
    * @param fontFace Метрики шрифту, необхідні для обчислення координат.
    * @return Try[Map[String, Glyph]] – мапа гліфів, де ключем є символ або назва гліфа, або повідомлення про помилку, якщо парсинг неможливий.
    */
  private def parseGlyphs(fontNode: Node, fontFace: FontFace): Try[Map[String, Glyph]] = Try {
    (fontNode \\ "glyph").collect {
      case g if (g \@ "glyph-name").nonEmpty =>
        val name = g \@ "glyph-name"
        val unicode = Option(g \@ "unicode").filter(_.nonEmpty)
        val advance = (g \@ "horiz-adv-x").toDoubleOption.getOrElse(
          (fontNode \@ "horiz-adv-x").toDouble
        )
        // Парсинг даних шляху, після чого виконується трансформація координат.
        val paths = parsePathData(g \@ "d")
          .translate(0, fontFace.unitsPerEm)
          .scale(1.0, -1.0)

        unicode.getOrElse(name) -> Glyph(name, unicode, advance, paths)
    }.toMap
  }

  /**
    * Парсить інформацію про відсутній гліф з XML-вузла.
    *
    * Якщо XML-документ містить елемент <missing-glyph>, створюється відповідний гліф,
    * який використовується для відображення символів, що не знайдені у основному наборі гліфів.
    *
    * @param fontNode XML-вузол, що містить інформацію про шрифт.
    * @param fontFace Метрики шрифту.
    * @return Try[Map[String, Glyph]] – мапа з гліфом для відсутніх символів або порожня мапа, якщо елемент відсутній.
    */
  private def parseMissingGlyph(fontNode: Node, fontFace: FontFace): Try[Map[String, Glyph]] = Try {
    (fontNode \\ "missing-glyph").headOption.map { g =>
      val advance = (g \@ "horiz-adv-x").toDoubleOption.getOrElse(
        (fontNode \@ "horiz-adv-x").toDouble
      )
      "missing" -> Glyph("missing", None, advance, Paths.empty)
    }.toMap
  }

  /**
    * Парсить рядок даних шляху у форматі SVG.
    *
    * Метод використовує рекурсію для перетворення послідовності токенів у об'єкт Paths, що містить
    * список векторних шляхів, кожен з яких формується з послідовності точок.
    *
    * @param d Рядок, що містить дані шляху у форматі SVG.
    * @return Paths – об'єкт, що містить розпарсені векторні шляхи.
    */
  private def parsePathData(d: String): Paths = {
    @tailrec
    def parse(tokens: List[String], acc: List[Path], current: Path): Paths =
      tokens match {
        case "M" :: x :: y :: rest =>
          // If the current path is non-empty, add it to the accumulator
          val newAcc = if (current.points.nonEmpty) acc :+ current else acc
          // Start a new path with the new "M" coordinate
          parse(rest, newAcc, Path(List(Point(x.toDouble, y.toDouble))))
        case "L" :: x :: y :: rest =>
          // Continue adding points to the current path
          parse(rest, acc, current.copy(points = current.points :+ Point(x.toDouble, y.toDouble)))
        case Nil =>
          // When tokens are exhausted, add the last current path (if non-empty)
          if (current.points.nonEmpty) Paths(acc :+ current) else Paths(acc)
        case _ =>
          // In case of unexpected tokens, return what we have so far
          Paths(acc)
      }

    val tokenPattern = "([A-Za-z])|(-?\\d+(\\.\\d+)?)".r
    val tokens = tokenPattern.findAllIn(d).toList
    parse(tokens, Nil, Path(Nil))
  }

  /**
    * Клас, що містить опції рендерингу тексту.
    *
    * @param scale       Масштаб, який застосовується до гліфів.
    * @param charSpacing Відступ між символами.
    * @param origin      Початкова точка рендерингу.
    */
  case class RenderingOptions(
    scale: Double = 1.0,
    charSpacing: Double = 0.0,
    origin: Point = Point.zero
  )
}

/**
  * Розширення для Hershey.
  */
object HersheyExtensions {

  /**
    * Розширення для об'єктів типу Try[Hershey] для зручної роботи зі шрифтами.
    *
    * @param self об'єкт типу Try[Hershey], що містить завантажений шрифт
    */
  implicit class RichHersheyFont(val self: Try[Hershey]) {

    /**
      * Рендерить заданий текст за допомогою завантаженого шрифту.
      *
      * @param text Текст, який потрібно відобразити.
      * @return Try[Paths] – спроба отримати векторні шляхи рендереного тексту.
      */
    def render(text: String): Try[Paths] = self.map(_.renderText(text))

    /**
      * Комбінує два об'єкти типу Try[Hershey] у кортеж.
      *
      * @param other Інший об'єкт типу Try[Hershey].
      * @return Try[(Hershey, Hershey)] – спроба отримати кортеж, що містить два завантажених шрифти.
      */
    def combine(other: Try[Hershey]): Try[(Hershey, Hershey)] =
      self.flatMap(a => other.map(b => (a, b)))
  }

}

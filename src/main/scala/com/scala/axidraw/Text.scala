package com.scala.axidraw

import com.scala.axidraw.Hershey.Font
import com.scala.axidraw.GeometryUtils._
import scala.util.chaining._
import scala.annotation.tailrec

/**
  * Головний клас для роботи з текстом, що підтримує векторне відображення.
  *
  * @param content   Текст для відображення
  * @param modifiers Налаштування зовнішнього вигляду тексту
  */
case class Text private (
  content: String,
  modifiers: Text.Modifiers = Text.Modifiers()
) {
  import Text._

  /**
    * Змінює основний стиль шрифту.
    *
    * @param design Обраний стиль (наприклад, Serif, Sans, Monospace)
    * @return Новий об'єкт Text з оновленими налаштуваннями шрифту
    */
  def fontDesign(design: FontDesign): Text =
    copy(modifiers = modifiers.copy(design = design, family = design.defaultFamily))
    
  /**
    * Встановлює конкретне сімейство шрифтів.
    *
    * @param family Сімейство шрифтів з доступних або користувацьких
    * @return Новий об'єкт Text з обраним сімейством шрифтів
    */
  def fontFamily(family: FontFamily): Text =
    copy(modifiers = modifiers.copy(family = family))

  /**
    * Змінює вагу шрифту (товщину).
    *
    * @param weight Бажана вага шрифту (наприклад, Regular, Bold)
    * @return Новий об'єкт Text з оновленою вагою шрифту
    */
  def fontWeight(weight: FontWeight): Text =
    copy(modifiers = modifiers.copy(weight = weight))

  /**
    * Встановлює розмір шрифту у пунктах.
    *
    * @param size Розмір шрифту у пунктах
    * @return Новий об'єкт Text з заданим розміром шрифту
    */
  def fontSize(size: Double): Text =
    copy(modifiers = modifiers.copy(size = size))

  /**
    * Вмикає або вимикає курсивне накреслення.
    *
    * @param enabled true – ввімкнути курсив, false – вимкнути
    * @return Новий об'єкт Text з оновленою опцією курсиву
    */
  def italic(enabled: Boolean = true): Text =
    copy(modifiers = modifiers.copy(italic = enabled))

  /**
    * Задає відстань між символами (трекінг).
    *
    * @param value Значення трекінгу
    * @return Новий об'єкт Text з оновленим інтервалом між символами
    */
  def tracking(value: Double): Text =
    copy(modifiers = modifiers.copy(tracking = value))

  /**
    * Фіксує максимальну ширину текстового блоку.
    *
    * @param width Максимальна ширина текстового блоку у пунктах
    * @return Новий об'єкт Text з обмеженням за шириною
    */
  def frame(width: Double): Text =
    copy(modifiers = modifiers.copy(frameWidth = Some(width)))

  /**
    * Обмежує кількість відображуваних рядків.
    *
    * @param limit Максимальна кількість рядків
    * @return Новий об'єкт Text з обмеженням кількості рядків
    */
  def lineLimit(limit: Int): Text =
    copy(modifiers = modifiers.copy(lineLimit = Some(limit)))

  /**
    * Встановлює вирівнювання тексту у межах блоку.
    *
    * @param align Вирівнювання тексту (Leading, Center, Trailing)
    * @return Новий об'єкт Text з обраним вирівнюванням
    */
  def alignment(align: TextAlignment): Text =
    copy(modifiers = modifiers.copy(alignment = align))

  /**
    * Генерує векторні шляхи для відображення тексту.
    *
    * @return Об'єкт Paths, який містить векторні контури тексту
    */
  def render(): Paths = {
    val effectiveFont = resolveEffectiveFont()
    val scale = modifiers.size / effectiveFont.fontFace.unitsPerEm
    val lineHeight = calculateLineHeight(effectiveFont, scale)
    
    processContent(effectiveFont, scale)
      .zipWithIndex
      .foldLeft(Paths.empty) { case (paths, (line, idx)) =>
        val lineY = idx * lineHeight
        renderLine(line, effectiveFont, scale)
          .translate(0, lineY)
          .pipe(applyDecorations(_, line, effectiveFont, scale))
          .pipe(applyAlignment(_, line, effectiveFont, scale))
          .combine(paths)
      }
  }

  /**
    * Визначає активний шрифт на основі налаштувань.
    *
    * @return Ефективний шрифт для відображення тексту
    */
  private def resolveEffectiveFont(): Font = {
    val baseFont = modifiers.family.getFont(modifiers.weight, modifiers.italic)
      .orElse(modifiers.design.defaultFamily.getFont(modifiers.weight, modifiers.italic))
    
    baseFont.getOrElse {
      println(s"Шрифт не знайдено. Використовується стандартний.")
      FontFamily.DefaultFont
    }
  }

  /**
    * Розраховує висоту рядка з урахуванням інтервалів.
    *
    * @param font  Використовуваний шрифт
    * @param scale Масштаб відображення
    * @return Висота рядка у пунктах
    */
  private def calculateLineHeight(font: Font, scale: Double): Double = 
    (font.fontFace.ascent - font.fontFace.descent) * scale + modifiers.lineSpacing

  /**
    * Обробляє текст для подальшого відображення.
    *
    * @param font  Використовуваний шрифт
    * @param scale Масштаб відображення
    * @return Список рядків тексту після обробки та перенесення
    */
  private def processContent(font: Font, scale: Double): List[String] = {
    val rawLines = modifiers.frameWidth match {
      case Some(width) => wrapText(content, width / scale, font)
      case None        => content.split('\n').toList
    }
    rawLines.take(modifiers.lineLimit.getOrElse(Int.MaxValue))
  }

  /**
    * Розбиває текст на рядки з перенесенням слів за заданою максимальною шириною.
    *
    * @param text     Вхідний текст
    * @param maxWidth Максимальна ширина рядка (у пунктах)
    * @param font     Використовуваний шрифт
    * @param acc      Акумулятор для зберігання результатів
    * @return Список рядків після перенесення
    */
  @tailrec
  private def wrapText(text: String, maxWidth: Double, font: Font, acc: List[String] = Nil): List[String] = {
    if (text.isEmpty) acc.reverse
    else {
      val (line, remaining) = findBreakPoint(text, maxWidth, font)
      wrapText(remaining, maxWidth, font, line :: acc)
    }
  }

  /**
    * Розбиває надто довге слово на дві частини.
    *
    * @param word     Слово, яке потрібно розбити
    * @param maxWidth Максимальна ширина (у пунктах)
    * @param font     Використовуваний шрифт
    * @return Кортеж, де перший елемент – перша частина слова з дефісом, а другий – залишок слова
    */
  private def splitLongWord(word: String, maxWidth: Double, font: Font): (String, String) = {
    @tailrec
    def loop(idx: Int, currentWidth: Double): (String, String) = {
      if (idx >= word.length) (word, "")
      else {
        val char = word(idx)
        val charWidth = font.glyphs.get(char.toString).map(_.advanceWidth).getOrElse(0.0)
        if (currentWidth + charWidth > maxWidth && idx > 0) {
          val part1 = word.substring(0, idx) + "-"
          val part2 = word.substring(idx)
          (part1, part2)
        } else {
          loop(idx + 1, currentWidth + charWidth)
        }
      }
    }
    loop(0, 0.0)
  }

  /**
    * Знаходить оптимальну точку розриву для перенесення тексту.
    *
    * Якщо перше слово перевищує максимальну ширину, воно розбивається.
    *
    * @param text     Оброблюваний текст
    * @param maxWidth Максимальна ширина рядка (у пунктах)
    * @param font     Використовуваний шрифт
    * @return Кортеж, де перший елемент – рядок для відображення, а другий – залишок тексту
    */
  private def findBreakPoint(text: String, maxWidth: Double, font: Font): (String, String) = {
    val words = text.split(" ").toList
    @tailrec
    def loop(acc: List[String], remaining: List[String]): (String, String) = remaining match {
      case Nil => (acc.mkString(" "), "")
      case word :: tail =>
        if (acc.isEmpty && measureLine(word, font) > maxWidth) {
          val (part1, part2) = splitLongWord(word, maxWidth, font)
          (part1, (if (part2.nonEmpty) part2 :: tail else tail).mkString(" "))
        } else {
          val testLine = (acc :+ word).mkString(" ")
          if (measureLine(testLine, font) > maxWidth && acc.nonEmpty)
            (acc.mkString(" "), (word :: tail).mkString(" "))
          else
            loop(acc :+ word, tail)
        }
    }
    loop(Nil, words)
  }

  /**
    * Генерує векторні шляхи для окремого рядка тексту.
    *
    * @param line  Рядок тексту
    * @param font  Використовуваний шрифт
    * @param scale Масштаб відображення
    * @return Об'єкт Paths, що містить векторні шляхи для рядка
    */
  private def renderLine(line: String, font: Font, scale: Double): Paths = {
    line.foldLeft((Paths.empty, 0.0)) { 
      case ((paths, x), char) =>
        font.glyphs.get(char.toString).fold((paths, x)) { glyph =>
          val charPath = glyph.paths
            .scale(scale)
            .translate(x, 0)
          val advance = (glyph.advanceWidth + modifiers.tracking) * scale
          (paths.combine(charPath), x + advance)
        }
    }._1
  }

  /**
    * Додає декоративні елементи (підкреслення та закреслення) до тексту.
    *
    * @param paths  Поточні векторні шляхи
    * @param line   Рядок тексту
    * @param font   Використовуваний шрифт
    * @param scale  Масштаб відображення
    * @return Об'єкт Paths з доданими декоративними елементами
    */
  private def applyDecorations(paths: Paths, line: String, font: Font, scale: Double): Paths = {
    val underlinePath = if (modifiers.underline) renderUnderline(line, font, scale) else Paths.empty
    val strikePath    = if (modifiers.strikethrough) renderStrikethrough(line, font, scale) else Paths.empty
    paths.combine(underlinePath).combine(strikePath)
  }

  /**
    * Створює шлях для підкреслення тексту.
    *
    * @param line  Рядок тексту
    * @param font  Використовуваний шрифт
    * @param scale Масштаб відображення
    * @return Об'єкт Paths, що містить контур підкреслення
    */
  private def renderUnderline(line: String, font: Font, scale: Double): Paths = {
    val lineWidth  = measureLine(line, font) * scale
    val yPosition  = (-font.fontFace.descent * 0.9) * scale // 90% нижче базової лінії
    Paths(Seq(Path(Seq(
      Point(0, yPosition),
      Point(lineWidth, yPosition)
    ))))
  }

  /**
    * Створює шлях для закреслення тексту.
    *
    * @param line  Рядок тексту
    * @param font  Використовуваний шрифт
    * @param scale Масштаб відображення
    * @return Об'єкт Paths, що містить контур закреслення
    */
  private def renderStrikethrough(line: String, font: Font, scale: Double): Paths = {
    val lineWidth  = measureLine(line, font) * scale
    val yPosition  = (font.fontFace.ascent * 0.45) * scale // 45% вище базової лінії
    Paths(Seq(Path(Seq(
      Point(0, yPosition),
      Point(lineWidth, yPosition)
    ))))
  }

  /**
    * Застосовує вирівнювання до векторних шляхів.
    *
    * @param paths  Поточні векторні шляхи
    * @param line   Рядок тексту
    * @param font   Використовуваний шрифт
    * @param scale  Масштаб відображення
    * @return Об'єкт Paths, що містить вирівняні векторні шляхи
    */
  private def applyAlignment(paths: Paths, line: String, font: Font, scale: Double): Paths = {
    val lineWidth  = measureLine(line, font) * scale
    val frameWidth = modifiers.frameWidth.getOrElse(lineWidth)
    val offset = modifiers.alignment match {
      case TextAlignment.Leading  => 0.0
      case TextAlignment.Center   => (frameWidth - lineWidth) / 2
      case TextAlignment.Trailing => frameWidth - lineWidth
    }
    paths.translate(offset, 0)
  }

  /**
    * Вимірює довжину рядка тексту у пунктах.
    *
    * @param line Рядок тексту
    * @param font Використовуваний шрифт
    * @return Довжина рядка у пунктах
    */
  private def measureLine(line: String, font: Font): Double =
    line.map(c => font.glyphs.get(c.toString).fold(0.0)(_.advanceWidth)).sum
}

/**
  * Супутній об'єкт з визначеннями типів та конфігурацій для роботи з текстом.
  */
object Text {

  /**
    * Трейт, що визначає вагу шрифту.
    */
  sealed trait FontWeight {
    /**
      * Товщина шрифту.
      */
    def thickness: Int
  }

  /**
    * Об'єкт, що містить стандартні значення ваги шрифту.
    */
  object FontWeight {
    case object UltraLight extends FontWeight { val thickness = 100 }
    case object Thin       extends FontWeight { val thickness = 200 }
    case object Light      extends FontWeight { val thickness = 300 }
    case object Regular    extends FontWeight { val thickness = 400 }
    case object Medium     extends FontWeight { val thickness = 500 }
    case object Semibold   extends FontWeight { val thickness = 600 }
    case object Bold       extends FontWeight { val thickness = 700 }
    case object Heavy      extends FontWeight { val thickness = 800 }
    case object Black      extends FontWeight { val thickness = 900 }
  }

  /**
    * Модель варіанту шрифту (вага + курсив).
    *
    * @param weight Вага шрифту
    * @param italic Прапорець, що вказує на курсивне накреслення (за замовчуванням false)
    */
  case class FontVariant(weight: FontWeight, italic: Boolean = false)

  /**
    * Сімейство шрифтів із наявними варіантами.
    *
    * @param name     Назва сімейства шрифтів
    * @param variants Список доступних варіантів у вигляді пар (FontVariant, Font)
    */
  case class FontFamily(
    name: String,
    variants: List[(FontVariant, Font)]
  ) {
    /**
      * Знаходить найближчу доступну вагу шрифту за різницею товщини.
      *
      * @param target Бажана вага шрифту
      * @return Найближча за товщиною вага
      */
    private def findClosestWeight(target: FontWeight): FontWeight =
      variants.map(_._1.weight).distinct.minBy(w => math.abs(w.thickness - target.thickness))

    /**
      * Пошук шрифту за заданими параметрами.
      *
      * Логіка пошуку:
      *   1. Шукається точна відповідність за вагою та курсивом.
      *   2. Якщо точного збігу немає, шукається найближча вага серед тих, що мають потрібний курсив.
      *   3. Якщо і цього не вдається, використовується найближча за товщиною вага серед усіх варіантів.
      *
      * @param weight Бажана вага шрифту
      * @param italic Чи потрібен курсив
      * @return Опціональний шрифт
      */
    def getFont(weight: FontWeight, italic: Boolean): Option[Font] = {
      val exactMatch = variants.find { case (v, _) => v.weight == weight && v.italic == italic }
      val italicMatch = variants.filter { case (v, _) => v.italic == italic } match {
        case Nil => None
        case italicVariants =>
          val closest = italicVariants.map(_._1.weight).distinct.minBy(w => math.abs(w.thickness - weight.thickness))
          variants.find { case (v, _) => v.weight == closest && v.italic == italic }
      }
      val fallback = {
        val closest = findClosestWeight(weight)
        variants.find { case (v, _) => v.weight == closest }
      }
      exactMatch.orElse(italicMatch).orElse(fallback).map(_._2)
    }
  }

  /**
    * Базовий стиль шрифту.
    */
  sealed trait FontDesign {
    /**
      * Сімейство шрифтів за замовчуванням для даного стилю.
      *
      * @return Сімейство шрифтів
      */
    def defaultFamily: FontFamily
  }

  /**
    * Об'єкт, що містить стандартні варіанти дизайну шрифту.
    */
  object FontDesign {
    /**
      * Стиль з зарубками (Serif).
      */
    case object Serif extends FontDesign {
      def defaultFamily = FontFamily.HersheySerif
    }
    /**
      * Стиль без зарубок (Sans).
      */
    case object Sans extends FontDesign {
      def defaultFamily = FontFamily.HersheySans
    }
    /**
      * Моноширинний стиль (Monospace).
      */
    case object Monospace extends FontDesign {
      def defaultFamily = FontFamily.EMSTech
    }
  }

  /**
    * Попередньо визначені сімейства шрифтів.
    */
  object FontFamily {
    /**
      * Завантажує шрифт через API Hershey.
      *
      * @param name Назва шрифту для завантаження
      * @return Завантажений шрифт
      * @throws FontLoadException Якщо шрифт не вдається завантажити
      */
    private def loadHersheyFont(name: String): Font =
      Hershey(name).fold(
        err => throw new FontLoadException(s"Помилка завантаження шрифту $name: $err"),
        _.font
      )

    /**
      * Стандартний шрифт за замовчуванням.
      */
    val DefaultFont: Font = loadHersheyFont("hershey_sans_1")

    /**
      * Серіфне сімейство шрифтів Hershey.
      */
    val HersheySerif = FontFamily(
      "Hershey Serif",
      List(
        (FontVariant(FontWeight.Regular), loadHersheyFont("hershey_serif")),
        (FontVariant(FontWeight.Medium),  loadHersheyFont("hershey_serif_med")),
        (FontVariant(FontWeight.Bold),    loadHersheyFont("hershey_serif_bold"))
      )
    )

    /**
      * Сімейство без зарубок Hershey.
      */
    val HersheySans = FontFamily(
      "Hershey Sans",
      List(
        (FontVariant(FontWeight.Regular), loadHersheyFont("hershey_sans_1")),
        (FontVariant(FontWeight.Medium),  loadHersheyFont("hershey_sans_med"))
      )
    )

    /**
      * Технічне моноширинне сімейство EMS.
      */
    val EMSTech = FontFamily(
      "EMS Tech",
      List(
        (FontVariant(FontWeight.Regular), loadHersheyFont("ems_tech"))
      )
    )
  }

  /**
    * Виняток, який виникає при невдалій спробі завантаження шрифту.
    *
    * @param message Повідомлення про помилку
    */
  class FontLoadException(message: String) extends RuntimeException(message)

  /**
    * Контейнер параметрів відображення тексту.
    *
    * @param design        Основний стиль шрифту
    * @param family        Вибране сімейство шрифтів
    * @param weight        Вага шрифту
    * @param size          Розмір шрифту у пунктах
    * @param italic        Прапорець, що вказує на курсивне накреслення
    * @param tracking      Відстань між символами
    * @param lineSpacing   Інтервал між рядками
    * @param underline     Прапорець, що вказує на підкреслення
    * @param strikethrough Прапорець, що вказує на закреслення
    * @param frameWidth    Максимальна ширина текстового блоку
    * @param lineLimit     Максимальна кількість рядків
    * @param alignment     Вирівнювання тексту
    */
  case class Modifiers(
    design: FontDesign = FontDesign.Serif,
    family: FontFamily = FontDesign.Serif.defaultFamily,
    weight: FontWeight = FontWeight.Regular,
    size: Double = 14.0,
    italic: Boolean = false,
    tracking: Double = 0.0,
    lineSpacing: Double = 4.0,
    underline: Boolean = false,
    strikethrough: Boolean = false,
    frameWidth: Option[Double] = None,
    lineLimit: Option[Int] = None,
    alignment: TextAlignment = TextAlignment.Leading
  )

  /**
    * Створення текстового об'єкта зі стандартними налаштуваннями.
    *
    * @param content Текст для відображення
    * @return Новий об'єкт Text
    */
  def apply(content: String): Text = new Text(content)
  
  /**
    * Створення текстового об'єкта з користувацьким шрифтом.
    *
    * @param content Текст для відображення
    * @param font    Користувацький шрифт
    * @return Новий об'єкт Text
    */
  def apply(content: String, font: Font): Text =
    new Text(content, Modifiers(family = FontFamily("Custom", List(
      (FontVariant(FontWeight.Regular), font),
      (FontVariant(FontWeight.Regular, italic = true), font)
    ))))

  /**
    * Тип вирівнювання тексту.
    */
  sealed trait TextAlignment
  /**
    * Об'єкт, що містить варіанти вирівнювання тексту.
    */
  object TextAlignment {
    /**
      * Вирівнювання за лівим краєм.
      */
    case object Leading extends TextAlignment
    /**
      * Центрування тексту.
      */
    case object Center extends TextAlignment
    /**
      * Вирівнювання за правим краєм.
      */
    case object Trailing extends TextAlignment
  }
}


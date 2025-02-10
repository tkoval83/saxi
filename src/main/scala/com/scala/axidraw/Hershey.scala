package com.scala.axidraw

import io.circe._
import io.circe.generic.semiauto._

import java.io.FileNotFoundException
import scala.io.Source
import scala.xml.XML

/**
  * Об'єкт для роботи з векторними шрифтами Hershey у форматі SVG.
  * Надає функції для завантаження шрифтів, отримання гліфів та керування кешем.
  */
object Hershey {

  /**
    * Представляє шрифт.
    *
    * @param fontId унікальний ідентифікатор шрифту (наприклад, "HersheySans1")
    * @param fontFace об'єкт, що містить метрики та властивості шрифту.
    * @param glyphs карта гліфів, де ключем є ім'я гліфа, а значення – тип [[Glyph]].
    *               Якщо шрифт не містить гліф для певного символу, його missing-гліф
    *               зберігається в цій мапі під ключем null.
    */
  final case class Font(
    fontId: String,
    fontFace: FontFace,
    glyphs: Map[String, Glyph]
  )

  /**
    * Містить метрики шрифту та інші властивості.
    *
    * @param familyName назва сімейства шрифту (наприклад, "Hershey Sans 1-stroke")
    * @param unitsPerEm кількість одиниць, що відповідають одному em (наприклад, 1000)
    * @param ascent значення підйому (наприклад, 800)
    * @param descent значення опускання (наприклад, -200)
    * @param capHeight висота великих літер (наприклад, 500)
    * @param xHeight висота малих літер (наприклад, 300)
    */
  final case class FontFace(
    familyName: String,
    unitsPerEm: Int,
    ascent: Int,
    descent: Int,
    capHeight: Int,
    xHeight: Int
  )

  /**
    * Представляє окремий гліф (символ) у шрифті.
    *
    * @param name ім'я гліфа.
    * @param unicode значення Unicode.
    * @param advanceWidth горизонтальна ширина для символу.
    * @param paths дані контуру, що описують контур символу.
    */
  final case class Glyph(
    name: String,
    unicode: Option[String],
    advanceWidth: Double,
    paths: Seq[Path]
  )

  /**
    * Представляє запис про шрифт у індексному файлі.
    *
    * @param file Назва файлу SVG з гліфами.
    * @param name Відображувана назва шрифту.
    */
  private case class FontEntry(file: String, name: String)

  // Неявний декодер JSON для FontEntry.
  implicit private val fontEntryDecoder: Decoder[FontEntry] = deriveDecoder[FontEntry]

  // Реєстр записів про шрифти (попередньо не завантажені об'єкти Font).
  private var fontRegistry: Map[String, FontEntry] = Map.empty

  // Кеш завантажених Font-об'єктів, де ключ – fontKey.
  private var fontCache: Map[String, Font] = Map.empty

  /**
    * Ініціалізує систему шрифтів, завантажуючи індексний файл.
    */
  def init(): Unit = {
    val indexJson = readResource("/hershey/index.json")
    fontRegistry = parseFontRegistry(indexJson)
  }

  /**
    * Зчитує ресурс з classpath як рядок.
    *
    * @param path Шлях до ресурсу.
    * @return Вміст ресурсу як рядок.
    * @throws FileNotFoundException Якщо ресурс не знайдено.
    */
  private def readResource(path: String): String = {
    val stream = getClass.getResourceAsStream(path)
    if (stream == null) throw new FileNotFoundException(path)
    Source.fromInputStream(stream).mkString
  }

  /**
    * Парсить JSON-індекс шрифтів.
    *
    * @param json JSON-рядок з індексом.
    * @return Мапа ідентифікаторів шрифтів до їх записів.
    */
  private def parseFontRegistry(json: String): Map[String, FontEntry] =
    io.circe.parser.parse(json) match {
      case Left(error) => throw new RuntimeException(s"Помилка парсингу JSON: $error")
      case Right(json) =>
        json.as[Map[String, FontEntry]] match {
          case Left(error)    => throw new RuntimeException(s"Помилка декодування JSON: $error")
          case Right(entries) => entries
        }
    }

  /**
    * Завантажує шрифт із SVG-файлу та перетворює його у Font.
    *
    * @param fontKey Ідентифікатор шрифту.
    * @return Опціональний об'єкт Font.
    */
  def loadFont(fontKey: String): Option[Font] =
    fontRegistry.get(fontKey).flatMap { fontEntry =>
      try {
        val svgContent = readResource(s"/hershey/${fontEntry.file}")
        val xml = XML.loadString(svgContent)

        // Знаходимо елемент <font>.
        val fontNode = (xml \\ "font").headOption.getOrElse {
          throw new Exception("Не знайдено елемент <font> у SVG")
        }
        val fontId = (fontNode \@ "id").trim
        // Значення horiz-adv-x, яке використовується як дефолтова горизонтальна ширина.
        val defaultHorizAdvX = (fontNode \@ "horiz-adv-x").trim.toInt

        // Парсимо <font-face>.
        val fontFaceNode = (fontNode \\ "font-face").headOption.getOrElse {
          throw new Exception("Не знайдено елемент <font-face> у SVG")
        }
        val fontFace = FontFace(
          familyName = (fontFaceNode \@ "font-family").trim,
          unitsPerEm = (fontFaceNode \@ "units-per-em").trim.toInt,
          ascent = (fontFaceNode \@ "ascent").trim.toInt,
          descent = (fontFaceNode \@ "descent").trim.toInt,
          capHeight = (fontFaceNode \@ "cap-height").trim.toInt,
          xHeight = (fontFaceNode \@ "x-height").trim.toInt
        )

        // Парсимо усі <glyph> елементи.
        val glyphs: Map[String, Glyph] = (fontNode \\ "glyph").flatMap { glyphNode =>
          val gName = (glyphNode \@ "glyph-name").trim
          if (gName.isEmpty) None
          else {
            val unicodeOpt =
              (glyphNode \@ "unicode") match {
                case s if s.nonEmpty => Some(s)
                case _               => None
              }
            val adv = (glyphNode \@ "horiz-adv-x").trim match {
              case s if s.nonEmpty => s.toDouble
              case _               => defaultHorizAdvX
            }
            val d = (glyphNode \@ "d").trim
            val paths: Seq[Path] = if (d.nonEmpty) parsePathData(d) else Seq.empty
            Some(gName -> Glyph(gName, unicodeOpt, adv, paths))
          }
        }.toMap

        // Парсимо <missing-glyph> якщо існує. Якщо знайдено, додаємо його у мапу під ключем null.
        val allGlyphs: Map[String, Glyph] = (fontNode \\ "missing-glyph").headOption match {
          case Some(node) =>
            val adv = (node \@ "horiz-adv-x").trim match {
              case s if s.nonEmpty => s.toInt
              case _               => defaultHorizAdvX
            }
            // Створюємо missing glyph з порожнім контуром.
            val missingGlyph = Glyph("missing-glyph", None, adv, Seq.empty)
            glyphs + (null.asInstanceOf[String] -> missingGlyph)
          case None =>
            // Якщо missing-glyph не знайдено, можна не додавати (або додати за замовчуванням).
            glyphs
        }

        val font = Font(fontId, fontFace, allGlyphs)
        fontCache += (fontKey -> font)
        Some(font)
      } catch {
        case e: Exception =>
          println(s"Помилка завантаження шрифту $fontKey: ${e.getMessage}")
          None
      }
    }

  /**
    * Парсить рядок з SVG-шляхами у послідовність об'єктів Path.
    *
    * @param d Рядок з даними шляху у форматі SVG.
    * @return Послідовність шляхів.
    */
  private def parsePathData(d: String): Seq[Path] = {
    // Вставляємо пробіл між числом і наступною командою
    val preprocessed = d.replaceAll("(?<=[0-9\\.])(?=[A-Za-z])", " ")
    preprocessed.split("M")
      .filter(_.nonEmpty)
      .map { subpath =>
        val tokens = subpath.trim.split("\\s+").toList
        tokens match {
          case x :: y :: rest =>
            val initialPoint = Point(parseToken(x), parseToken(y))
            val points = scala.collection.mutable.ListBuffer(initialPoint)
            var remainingTokens = rest
            while (remainingTokens.nonEmpty) {
              remainingTokens match {
                case head :: a :: b :: tail if head.forall(_.isLetter) =>
                  // Якщо перший токен — це команда (наприклад, "L"), то видаляємо її та обробляємо наступні токени
                  points += Point(parseToken(a), parseToken(b))
                  remainingTokens = tail
                case a :: b :: tail =>
                  // Якщо токени вже чисті, просто перетворюємо їх
                  points += Point(parseToken(a), parseToken(b))
                  remainingTokens = tail
                case _ =>
                  remainingTokens = Nil
              }
            }
            Path(points.toSeq)
          case _ =>
            throw new IllegalArgumentException("Невірний формат шляху")
        }
      }
      .toSeq
  }

  private def parseToken(token: String): Double = {
    // Видаляємо всі літери на початку рядка
    val cleaned = token.replaceAll("^[A-Za-z]+", "")
    cleaned.toDouble
  }

  /**
    * Отримує гліф для вказаного символу, шукаючи у полі Unicode.
    * Якщо відповідний гліф не знайдено, повертається missing glyph із мапи (з ключем null).
    * Якщо Unicode гліфа містить кілька символів, використовується перший.
    *
    * @param fontKey Ідентифікатор шрифту.
    * @param ch Символ, для якого шукаємо гліф.
    * @return Опціональний об'єкт Glyph.
    */
  def getGlyph(fontKey: String, ch: Char): Option[Glyph] =
    (fontCache.get(fontKey) orElse loadFont(fontKey)) match {
      case Some(font) =>
        font.glyphs.values.find { g =>
          g.unicode.exists { u =>
            u.nonEmpty && u.head == ch
          }
        } orElse font.glyphs.get(null)
      case None => None
    }

  /**
    * Повертає список доступних шрифтів.
    *
    * @return Мапа ідентифікаторів шрифтів до їх назв.
    */
  def listFonts(): Map[String, String] =
    fontRegistry.map { case (k, v) => k -> v.name }
}

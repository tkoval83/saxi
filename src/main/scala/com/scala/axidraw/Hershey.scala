package com.scala.axidraw

import io.circe._
import io.circe.generic.semiauto._

import java.io.FileNotFoundException
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.xml.XML

/**
  * Об'єкт для роботи з векторними шрифтами Hershey у форматі SVG.
  * Надає функції для завантаження шрифтів, отримання гліфів та керування кешем.
  */
object Hershey {

  /**
    * Представляє запис про шрифт у індексному файлі.
    *
    * @param file Назва файлу SVG з гліфами
    * @param name Відображувана назва шрифту
    */
  case class FontEntry(file: String, name: String)

  /**
    * Зберігає всі гліфи для конкретного шрифту.
    *
    * @param fontName Назва шрифту
    * @param glyphs   Мапа гліфів де ключ - символ, значення - послідовність шляхів
    */
  case class FontGlyphs(fontName: String, glyphs: Map[Char, Seq[Path]])

  // Неявний декодер JSON для FontEntry
  implicit private val fontEntryDecoder: Decoder[FontEntry] = deriveDecoder[FontEntry]

  // Реєстр завантажених шрифтів
  private var fontRegistry: Map[String, FontEntry] = Map.empty

  // Кеш гліфів для швидкого доступу
  private var glyphCache: Map[String, FontGlyphs] = Map.empty

  /**
    * Ініціалізує систему шрифтів, завантажуючи індексний файл.
    */
  def init(): Unit = {
    val indexJson = readResource("/index.json")
    fontRegistry = parseFontRegistry(indexJson)
  }

  /**
    * Зчитує ресурс з classpath як рядок.
    *
    * @param path Шлях до ресурсу
    * @return Вміст ресурсу як рядок
    * @throws FileNotFoundException Якщо ресурс не знайдено
    */
  private def readResource(path: String): String = {
    val stream = getClass.getResourceAsStream(path)
    if (stream == null) throw new FileNotFoundException(path)
    Source.fromInputStream(stream).mkString
  }

  /**
    * Парсить JSON-індекс шрифтів.
    *
    * @param json JSON-рядок з індексом
    * @return Мапа ідентифікаторів шрифтів до їх записів
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
    * Завантажує гліфи для конкретного шрифту.
    *
    * @param fontKey Ідентифікатор шрифту
    * @return Опціональний об'єкт FontGlyphs
    */
  private def loadFontGlyphs(fontKey: String): Option[FontGlyphs] =
    fontRegistry.get(fontKey).flatMap { fontEntry =>
      try {
        val svgContent = readResource(s"/hershey/${fontEntry.file}")
        val xml = XML.loadString(svgContent)

        val glyphs = (xml \\ "glyph").flatMap { glyph =>
          val unicode = (glyph \@ "unicode").trim
          val pathData = (glyph \@ "d").trim
          if (unicode.nonEmpty && pathData.nonEmpty) {
            Some(unicode.head -> parsePathData(pathData))
          } else None
        }.toMap

        val fontGlyphs = FontGlyphs(fontEntry.name, glyphs)
        glyphCache += fontKey -> fontGlyphs
        Some(fontGlyphs)
      } catch {
        case e: Exception =>
          println(s"Помилка завантаження шрифту $fontKey: ${e.getMessage}")
          None
      }
    }

  /**
    * Парсить рядок з SVG-шляхами в послідовність об'єктів Path.
    *
    * @param d Рядок з даними шляху у форматі SVG
    * @return Послідовність шляхів
    */
  private def parsePathData(d: String): Seq[Path] =
    d.split("M")
      .filter(_.nonEmpty)
      .map { subpath =>
        val tokens = subpath.trim.split("\\s+").toList
        val (initialPoint, remaining) = tokens match {
          case x :: y :: rest =>
            (Point(x.toDouble, y.toDouble), rest)
          case _ => throw new IllegalArgumentException("Невірний формат шляху")
        }

        val points = ListBuffer(initialPoint)
        var current = initialPoint
        var remainingTokens = remaining

        while (remainingTokens.nonEmpty) {
          remainingTokens match {
            case "L" :: x :: y :: tail =>
              current = Point(x.toDouble, y.toDouble)
              points += current
              remainingTokens = tail

            case x :: y :: tail =>
              current = Point(x.toDouble, y.toDouble)
              points += current
              remainingTokens = tail

            case _ =>
              remainingTokens = Nil
          }
        }

        Path(points.toSeq)
      }
      .toSeq

  /**
    * Отримує послідовність шляхів для вказаного символу.
    *
    * @param fontKey Ідентифікатор шрифту
    * @param char    Символ для отримання
    * @return Опціональна послідовність шляхів
    */
  def getGlyph(fontKey: String, char: Char): Option[Seq[Path]] =
    glyphCache.get(fontKey) match {
      case Some(font) => font.glyphs.get(char)
      case None       => loadFontGlyphs(fontKey).flatMap(_.glyphs.get(char))
    }

  /**
    * Повертає список доступних шрифтів.
    *
    * @return Мапа ідентифікаторів шрифтів до їх назв
    */
  def listFonts(): Map[String, String] =
    fontRegistry.map { case (k, v) => k -> v.name }
}

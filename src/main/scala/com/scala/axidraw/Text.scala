package com.scala.axidraw

import com.scala.axidraw.Hershey.Font

import scala.annotation.tailrec
import scala.util.Success
import scala.util.chaining._

/**
  * Головний клас для роботи з векторним текстом.
  * Дозволяє налаштовувати параметри відображення та генерувати векторні шляхи.
  *
  * @param content   Вхідний текст
  * @param modifiers Налаштування відображення
  * @param origin    Початкова точка малювання
  * @param paths     Акумульовані векторні шляхи
  */
case class Text private (
  content: String,
  modifiers: Text.Modifiers = Text.Modifiers(),
  origin: Point = Point.zero,
  paths: Paths = Paths.empty
) {
  import Text._

  /**
    * Змінює основний стиль шрифту (серіфний, без зарубок, моноширинний).
    *
    * @param design Обраний стиль з переліку FontDesign
    * @return Новий екземпляр Text
    */
  def fontDesign(design: FontDesign): Text =
    copy(modifiers = modifiers.copy(design = design))

  /**
    * Встановлює конкретне сімейство шрифтів за назвою.
    *
    * @param familyName Назва сімейства з реєстру FontBook
    * @return Новий екземпляр Text
    */
  def fontFamily(familyName: String): Text =
    copy(modifiers = modifiers.copy(familyName = familyName))

  /**
    * Змінює товщину шрифту.
    *
    * @param weight Вага з переліку FontWeight (100-900)
    * @return Новий екземпляр Text
    */
  def fontWeight(weight: FontWeight): Text =
    copy(modifiers = modifiers.copy(weight = weight))

  /**
    * Встановлює розмір шрифту в пунктах.
    *
    * @param size Розмір у типографічних пунктах
    * @return Новий екземпляр Text
    */
  def fontSize(size: Double): Text =
    copy(modifiers = modifiers.copy(size = size))

  /**
    * Керує курсивним накресленням.
    *
    * @param enabled true - увімкнути курсив, false - звичайний стиль
    * @return Новий екземпляр Text
    */
  def italic(enabled: Boolean = true): Text =
    copy(modifiers = modifiers.copy(italic = enabled))

  /**
    * Задає відстань між символами (трекінг).
    *
    * @param value Значення додаткової відстані
    * @return Новий екземпляр Text
    */
  def tracking(value: Double): Text =
    copy(modifiers = modifiers.copy(tracking = value))

  /**
    * Фіксує максимальну ширину текстового блоку.
    *
    * @param width Максимальна ширина в пунктах
    * @return Новий екземпляр Text
    */
  def frame(width: Double): Text =
    copy(modifiers = modifiers.copy(frameWidth = Some(width)))

  /**
    * Обмежує кількість відображуваних рядків.
    *
    * @param limit Максимальна кількість рядків
    * @return Новий екземпляр Text
    */
  def lineLimit(limit: Int): Text =
    copy(modifiers = modifiers.copy(lineLimit = Some(limit)))

  /**
    * Встановлює горизонтальне вирівнювання тексту.
    *
    * @param align Тип вирівнювання з TextAlignment
    * @return Новий екземпляр Text
    */
  def alignment(align: TextAlignment): Text =
    copy(modifiers = modifiers.copy(alignment = align))

  /**
    * Генерує векторні шляхи для візуалізації тексту.
    *
    * @return Об'єкт Paths з векторним представленням
    */
  def render(): Paths = {
    val effectiveFont = resolveEffectiveFont()
    val scale = modifiers.size / effectiveFont.fontFace.unitsPerEm
    val lineHeight = calculateLineHeight(effectiveFont, scale)
    
    processContent(effectiveFont, scale)
      .zipWithIndex
      .foldLeft(Paths.empty) { case (paths, (line, idx)) =>
        val lineY = origin.y + idx * lineHeight
        renderLine(line, effectiveFont, scale)
          .translate(origin.x, lineY)
          .pipe(applyDecorations(_, line, effectiveFont, scale))
          .pipe(applyAlignment(_, line, effectiveFont, scale))
          .combine(paths)
      }
  }

  /**
    * Визначає активний шрифт на основі налаштувань.
    *
    * @return Знайдений або резервний шрифт
    */
  private def resolveEffectiveFont(): Font = {
    val style = if (modifiers.italic) FontStyle.Italic else FontStyle.Normal
    
    FontBook.findFont(
      familyName = modifiers.familyName,
      weight = modifiers.weight,
      style = style
    ).getOrElse {
      println("Шрифт не знайдено. Використовується резервний.")
      FontBook.getFallbackFont
    }
  }

  /**
    * Розраховує висоту рядка з урахуванням інтервалів.
    */
  private def calculateLineHeight(font: Font, scale: Double): Double = 
    (font.fontFace.ascent - font.fontFace.descent) * scale + modifiers.lineSpacing

  /**
    * Обробляє текст: переносить слова, обрізає кількість рядків.
    */
  private def processContent(font: Font, scale: Double): List[String] = {
    val rawLines = modifiers.frameWidth match {
      case Some(width) => wrapText(content, width / scale, font)
      case None        => content.split('\n').toList
    }
    rawLines.take(modifiers.lineLimit.getOrElse(Int.MaxValue))
  }

  @tailrec
  private def wrapText(text: String, maxWidth: Double, font: Font, acc: List[String] = Nil): List[String] = {
    if (text.isEmpty) acc.reverse else {
      val (line, remaining) = findBreakPoint(text, maxWidth, font)
      wrapText(remaining, maxWidth, font, line :: acc)
    }
  }

  private def splitLongWord(word: String, maxWidth: Double, font: Font): (String, String) = {
    @tailrec
    def loop(idx: Int, currentWidth: Double): (String, String) = {
      if (idx >= word.length) (word, "") else {
        val charWidth = font.glyphs.get(word(idx).toString)
          .map(_.advanceWidth).getOrElse(0.0)
        
        if ((currentWidth + charWidth > maxWidth) && idx > 0)
          (word.substring(0, idx) + "-", word.substring(idx))
        else
          loop(idx + 1, currentWidth + charWidth)
      }
    }
    loop(0, 0.0)
  }

  private def findBreakPoint(text: String, maxWidth: Double, font: Font): (String, String) = {
    val words = text.split(" ").toList
    @tailrec
    def loop(acc: List[String], remaining: List[String]): (String, String) = remaining match {
      case Nil => (acc.mkString(" "), "")
      case word :: tail =>
        if (acc.isEmpty && measureLine(word, font) > maxWidth) {
          val (part1, part2) = splitLongWord(word, maxWidth, font)
          (part1, (part2 +: tail).mkString(" "))
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

  private def applyDecorations(paths: Paths, line: String, font: Font, scale: Double): Paths = {
    val underlinePath = if (modifiers.underline) renderUnderline(line, font, scale) else Paths.empty
    val strikePath = if (modifiers.strikethrough) renderStrikethrough(line, font, scale) else Paths.empty
    paths.combine(underlinePath).combine(strikePath)
  }

  private def renderUnderline(line: String, font: Font, scale: Double): Paths = {
    val lineWidth = measureLine(line, font) * scale
    val yPosition = (-font.fontFace.descent * 0.9) * scale
    Paths(Seq(Path(Seq(
      Point(0, yPosition),
      Point(lineWidth, yPosition)
    ))))
  }

  private def renderStrikethrough(line: String, font: Font, scale: Double): Paths = {
    val lineWidth = measureLine(line, font) * scale
    val yPosition = (font.fontFace.ascent * 0.45) * scale
    Paths(Seq(Path(Seq(
      Point(0, yPosition),
      Point(lineWidth, yPosition)
    ))))
  }

  private def applyAlignment(paths: Paths, line: String, font: Font, scale: Double): Paths = {
    val lineWidth = measureLine(line, font) * scale
    val frameWidth = modifiers.frameWidth.getOrElse(lineWidth)
    val offset = modifiers.alignment match {
      case TextAlignment.Leading  => 0.0
      case TextAlignment.Center   => (frameWidth - lineWidth) / 2
      case TextAlignment.Trailing => frameWidth - lineWidth
    }
    paths.translate(offset, 0)
  }

  private def measureLine(line: String, font: Font): Double =
    line.map(c => font.glyphs.get(c.toString).fold(0.0)(_.advanceWidth)).sum
}

object Text {
  /** Тип вирівнювання тексту */
  sealed trait TextAlignment
  
  object TextAlignment {
    /** Вирівнювання по лівому краю */
    case object Leading extends TextAlignment
    /** Центрування по горизонталі */
    case object Center extends TextAlignment
    /** Вирівнювання по правому краю */
    case object Trailing extends TextAlignment
  }

  /**
    * Контейнер параметрів відображення тексту.
    *
    * @param design        Стиль шрифту
    * @param familyName    Назва сімейства
    * @param weight        Товщина шрифту
    * @param size          Розмір у пунктах
    * @param italic        Курсивне накреслення
    * @param tracking      Відстань між символами
    * @param lineSpacing   Відстань між рядками
    * @param underline     Підкреслення
    * @param strikethrough Закреслення
    * @param frameWidth    Макс. ширина блоку
    * @param lineLimit     Макс. кількість рядків
    * @param alignment     Горизонтальне вирівнювання
    */
  case class Modifiers(
    design: FontDesign = FontDesign.Sans,
    familyName: String = "Hershey Sans",
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

  /** Фабричний метод для створення тексту зі стандартними налаштуваннями */
  def apply(content: String): Text = new Text(content)

  /**
    * Створює текст з користувацьким шрифтом.
    *
    * @param content Текст для відображення
    * @param font    Об'єкт шрифту
    * @param design  Базовий дизайн
    */
  def apply(content: String, font: Font, design: FontDesign): Text = {
    val familyName = s"Custom-${java.util.UUID.randomUUID().toString.take(8)}"
    FontBook.registerFamily(FontFamily(
      name = familyName,
      design = design,
      variants = List((FontVariant(FontWeight.Regular), Success(font)))
    ))
    new Text(content, Modifiers(design = design, familyName = familyName))
  }
}


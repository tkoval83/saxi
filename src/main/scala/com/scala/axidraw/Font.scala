package com.scala.axidraw

import com.scala.axidraw.Hershey.Font

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

/**
  * Виняток, що виникає при невдалому завантаженні шрифту.
  *
  * @param message Повідомлення про помилку
  */
class FontLoadException(message: String) extends RuntimeException(message)

/**
  * Стиль шрифту: нормальний, курсив або похилий.
  */
sealed trait FontStyle {
  /**
    * Обчислює відстань до іншого стилю.
    *
    * @param other Інший стиль
    * @return 0 для однакових стилів, 150 для різних
    */
  def distanceTo(other: FontStyle): Int
}

object FontStyle {
  /** Нормальний стиль. */
  case object Normal extends FontStyle {
    def distanceTo(other: FontStyle): Int = if (this == other) 0 else 150
  }
  
  /** Курсивний стиль. */
  case object Italic extends FontStyle {
    def distanceTo(other: FontStyle): Int = if (this == other) 0 else 150
  }
  
  /** Похилий стиль. */
  case object Oblique extends FontStyle {
    def distanceTo(other: FontStyle): Int = if (this == other) 0 else 150
  }
}

/**
  * Вага шрифту (товщина).
  */
sealed trait FontWeight {
  /** Чисельне значення товщини (100-900). */
  val thickness: Int
  
  /**
    * Обчислює відстань до іншої ваги.
    *
    * @param other Інша вага
    * @return Різниця в товщині
    */
  def distanceTo(other: FontWeight): Int = 
    Math.abs(this.thickness - other.thickness)
}

object FontWeight {
  /** Ультратонкий (100). */
  case object UltraLight extends FontWeight { val thickness = 100 }
  
  /** Тонкий (200). */
  case object Thin extends FontWeight { val thickness = 200 }
  
  /** Легкий (300). */
  case object Light extends FontWeight { val thickness = 300 }
  
  /** Звичайний (400). */
  case object Regular extends FontWeight { val thickness = 400 }
  
  /** Середній (500). */
  case object Medium extends FontWeight { val thickness = 500 }
  
  /** Напівжирний (600). */
  case object Semibold extends FontWeight { val thickness = 600 }
  
  /** Жирний (700). */
  case object Bold extends FontWeight { val thickness = 700 }
  
  /** Наджирний (800). */
  case object Heavy extends FontWeight { val thickness = 800 }
  
  /** Найтовстіший (900). */
  case object Black extends FontWeight { val thickness = 900 }
}

/**
  * Розтягування шрифту (ширина).
  */
sealed trait FontStretch {
  /** Коефіцієнт розтягування. */
  val ratio: Double
  
  /**
    * Обчислює відстань до іншого розтягування.
    *
    * @param other Інше розтягування
    * @return Відстань помножена на 50
    */
  def distanceTo(other: FontStretch): Double = 
    Math.abs(this.ratio - other.ratio) * 50
}

object FontStretch {
  /** Ультравузький (0.5). */
  case object UltraCondensed extends FontStretch { val ratio = 0.5 }
  
  /** Дуже вузький (0.625). */
  case object ExtraCondensed extends FontStretch { val ratio = 0.625 }
  
  /** Вузький (0.75). */
  case object Condensed extends FontStretch { val ratio = 0.75 }
  
  /** Напіввузький (0.875). */
  case object SemiCondensed extends FontStretch { val ratio = 0.875 }
  
  /** Нормальний (1.0). */
  case object Normal extends FontStretch { val ratio = 1.0 }
  
  /** Напівширокий (1.125). */
  case object SemiExpanded extends FontStretch { val ratio = 1.125 }
  
  /** Широкий (1.25). */
  case object Expanded extends FontStretch { val ratio = 1.25 }
  
  /** Дуже широкий (1.5). */
  case object ExtraExpanded extends FontStretch { val ratio = 1.5 }
  
  /** Ультраширокий (2.0). */
  case object UltraExpanded extends FontStretch { val ratio = 2.0 }
}

/**
  * Варіант шрифту, що комбінує вагу, стиль і розтягування.
  *
  * @param weight Вага
  * @param style Стиль
  * @param stretch Розтягування
  */
case class FontVariant(
  weight: FontWeight,
  style: FontStyle = FontStyle.Normal,
  stretch: FontStretch = FontStretch.Normal
) {
  /**
    * Обчислює відстань до іншого варіанту.
    *
    * @param target Інший варіант
    * @return Сумарна відстань
    */
  def distanceTo(target: FontVariant): Double =
    weight.distanceTo(target.weight) +
    style.distanceTo(target.style) +
    stretch.distanceTo(target.stretch)

  /**
    * Обчислює відстань до окремих параметрів.
    *
    * @param targetWeight Цільова вага
    * @param targetStyle Цільовий стиль
    * @param targetStretch Цільове розтягування
    * @return Сумарна відстань
    */
  def distanceTo(
    targetWeight: FontWeight,
    targetStyle: FontStyle,
    targetStretch: FontStretch
  ): Double =
    weight.distanceTo(targetWeight) +
    style.distanceTo(targetStyle) +
    stretch.distanceTo(targetStretch)
}

/**
  * Дизайн шрифту (серіфний, без зарубок, моноширинний).
  */
sealed abstract class FontDesign(val defaultFamilyName: String)

object FontDesign {
  /** Серіфи (зарубки на кінцях літер). */
  case object Serif extends FontDesign("serif")
  
  /** Без зарубок. */
  case object Sans extends FontDesign("sans")
  
  /** Моноширинний (фіксована ширина). */
  case object Monospace extends FontDesign("monospace")
}

/**
  * Сімейство шрифтів зі списком варіантів.
  *
  * @param name Назва сімейства
  * @param design Дизайн шрифту
  * @param variants Список варіантів
  */
case class FontFamily(
  name: String,
  design: FontDesign,
  variants: List[(FontVariant, Try[Font])]
) {
  /**
    * Знаходить найближчий варіант шрифту.
    *
    * @param weight Бажана вага
    * @param style Бажаний стиль
    * @param stretch Бажане розтягування
    * @return Результат з найближчим шрифтом
    */
  def getFont(
    weight: FontWeight = FontWeight.Regular,
    style: FontStyle = FontStyle.Normal,
    stretch: FontStretch = FontStretch.Normal
  ): Try[Font] = variants match {
    case Nil => Failure(new FontLoadException(s"Відсутні варіанти в сімействі '$name'"))
    case vs => vs.minBy { case (v, _) => 
        v.distanceTo(weight, style, stretch)
      }._2
  }

  /**
    * Знаходить найближчий варіант до цільового.
    *
    * @param target Цільовий варіант
    * @return Результат з найближчим шрифтом
    */
  def findClosestVariant(target: FontVariant): Try[Font] =
    variants.minBy { case (v, _) => v.distanceTo(target) }._2
}

/**
  * Реєстр сімейств шрифтів з можливістю пошуку.
  */
object FontBook {
  private val families = ListBuffer.empty[FontFamily]

  /**
    * Реєструє нове сімейство шрифтів.
    *
    * @param family Сімейство для додавання
    */
  def registerFamily(family: FontFamily): Unit = families += family

  /**
    * Шукає шрифт за заданими параметрами з обов’язковою назвою сімейства.
    *
    * @param familyName Назва сімейства шрифтів (регістр не враховується)
    * @param weight     Бажана вага (за замовчуванням Regular)
    * @param style      Бажаний стиль (за замовчуванням Normal)
    * @param stretch    Бажане розтягування (за замовчуванням Normal)
    * @return Спроба отримати відповідний шрифт
    */
  def findFont(
    familyName: String,
    weight: FontWeight = FontWeight.Regular,
    style: FontStyle = FontStyle.Normal,
    stretch: FontStretch = FontStretch.Normal
  ): Try[Font] = {
    @tailrec
    def fallback(remaining: List[FontFamily]): Try[Font] = remaining match {
      case Nil =>
        Failure(new FontLoadException(s"Не знайдено шрифтів для сімейства '$familyName'"))
      case family :: rest =>
        family.getFont(weight, style, stretch) match {
          case Success(font) => Success(font)
          case Failure(_)    => fallback(rest)
        }
    }

    val candidates = families
      .filter(f => f.name.equalsIgnoreCase(familyName))
      .sortBy(_.name)
      .toList

    if (candidates.isEmpty) Try(getFallbackFont)
    else fallback(candidates)
  }

  /** Завантажує резервний шрифт. */
  def getFallbackFont: Font =
    Hershey("hershey_sans_1").fold(
      err => throw new FontLoadException(s"Помилка завантаження резервного шрифту: $err"),
      _.font
    )

  // Ініціалізація стандартних сімейств
  registerFamily(FontFamily(
    "Hershey Serif",
    FontDesign.Serif,
    List(
      (FontVariant(FontWeight.Medium), Hershey("hershey_serif_med").map(_.font)),
      (FontVariant(FontWeight.Bold, FontStyle.Italic), Hershey("hershey_serif_bold").map(_.font))
    )
  ))

  registerFamily(FontFamily(
    "Hershey Sans",
    FontDesign.Sans,
    List(
      (FontVariant(FontWeight.Regular), Hershey("hershey_sans_1").map(_.font)),
      (FontVariant(FontWeight.Medium), Hershey("hershey_sans_med").map(_.font))
    )
  ))

  registerFamily(FontFamily(
    "EMS Tech",
    FontDesign.Monospace,
    List(
      (FontVariant(FontWeight.Regular), Hershey("ems_tech").map(_.font))
    )
  ))
}


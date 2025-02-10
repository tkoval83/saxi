package com.scala.axidraw

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.util.{Failure, Success}

class HersheySpec extends AnyWordSpec with Matchers {

  "Система шрифтів Hershey" should {

    "ініціалізуватися та завантажити всі шрифти з index.json" in {
      // Ініціалізуємо систему шрифтів
      Hershey.init() match {
        case Failure(ex) => fail(s"Ініціалізація не вдалася: $ex")
        case Success(_)  => // ініціалізація пройшла успішно
      }

      // Очікувані ключі з index.json
      val expectedKeys = Set(
        "ems_allure",
        "ems_elfin",
        "ems_felix",
        "ems_nixish",
        "ems_nixish_italic",
        "ems_osmotron",
        "ems_readability",
        "ems_readability_italic",
        "ems_tech",
        "hershey_goth_english",
        "hershey_sans_1",
        "hershey_sans_med",
        "hershey_script_1",
        "hershey_script_med",
        "hershey_serif_bold",
        "hershey_serif_bold_italic",
        "hershey_serif_med",
        "hershey_serif_med_italic"
      )

      // Для кожного ключа перевіряємо, що можна створити Hershey instance
      expectedKeys.foreach { key =>
        withClue(s"Ключ '$key': ") {
          Hershey(key) match {
            case Failure(ex) =>
              fail(s"Завантаження шрифту для ключа '$key' не вдалося: $ex")
            case Success(_) =>
            // Успішне завантаження
          }
        }
      }
    }

    "повертати Failure при завантаженні невідомого ключа шрифту" in {
      Hershey.init() match {
        case Failure(ex) => fail(s"Ініціалізація не вдалася: $ex")
        case Success(_)  => // ініціалізація успішна
      }

      val unknownKey = "невідомий_шрифт"
      Hershey(unknownKey) match {
        case Success(_) =>
          fail(s"Шрифт '$unknownKey' повинен бути невідомим, але було завантажено")
        case Failure(_) =>
          succeed // Очікувано – шрифт не знайдено
      }
    }

    "містити гліф 'A' у шрифті HersheySans1" in {
      Hershey.init() match {
        case Failure(exception) =>
          fail(s"Ініціалізація не вдалася: $exception")
        case Success(_) =>
      }

      val fontKey = "hershey_sans_1"
      Hershey(fontKey) match {
        case Failure(exception) =>
          fail(s"Завантаження шрифту '$fontKey' не вдалося: $exception")
        case Success(hersheyInstance) =>
          // Перевіряємо, що шрифт містить гліф для символу "A"
          val font = hersheyInstance.font
          font.glyphs should contain key ("A")

          val glyphA = font.glyphs("A")
          glyphA.name shouldBe "A"
          // Очікувана ширина символу згідно з атрибутом horiz-adv-x (567)
          glyphA.advanceWidth shouldBe 567.0
          // Переконуємося, що векторні дані не порожні
          glyphA.paths.paths should not be empty
      }
    }

    "повертати Paths, що відповідають missing glyph для неіснуючого символу" in {
      Hershey.init() match {
        case Failure(ex) => fail(s"Ініціалізація не вдалася: $ex")
        case Success(_)  =>
      }

      val fontKey = "hershey_sans_1"
      Hershey(fontKey) match {
        case Failure(exception) =>
          fail(s"Завантаження шрифту '$fontKey' не вдалося: $exception")
        case Success(hersheyInstance) =>
          // Використовуємо символ, якого немає у шрифті (наприклад, "🚀")
          val nonExistingSymbol = "🚀"
          val options = Hershey.RenderingOptions(scale = 1.0, charSpacing = 0.0, origin = Point.zero)
          val renderedPaths = hersheyInstance.renderText(nonExistingSymbol, options)

          // Оскільки для неіснуючого символу використовується missing glyph,
          // а missing glyph має порожні векторні шляхи, очікуємо, що результат також порожній
          renderedPaths.paths shouldBe empty
      }
    }

    "повертати missing glyph при запиті неіснуючого символу" in {
      Hershey.init() match {
        case Failure(ex) => fail(s"Ініціалізація не вдалася: $ex")
        case Success(_)  =>
      }

      val fontKey = "hershey_sans_1"
      Hershey(fontKey) match {
        case Failure(ex) =>
          fail(s"Завантаження шрифту '$fontKey' не вдалося: $ex")
        case Success(hersheyInstance) =>
          val font = hersheyInstance.font
          // Запитуємо гліф для неіснуючого символу (наприклад, "nonexistent")
          val nonExistingKey = "nonexistent"
          // Завдяки fallback‑логіці (через withDefault) виклик повертає гліф "missing"
          val fallbackGlyph = font.glyphs(nonExistingKey)
          fallbackGlyph.name shouldEqual "missing"
      }
    }
  }
}

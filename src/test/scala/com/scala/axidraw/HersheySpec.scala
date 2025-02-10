package com.scala.axidraw

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class HersheySpec extends AnyWordSpec with Matchers {

  "Система шрифтів Hershey" should {

    "ініціалізуватися та повернути список доступних шрифтів з index.json" in {
      // Ініціалізуємо систему шрифтів (index.json має бути у main/resources)
      Hershey.init()
      val fonts = Hershey.listFonts()
      fonts should not be empty

      // Перевіряємо, що деякі ключі з наданого index.json присутні
      fonts should contain key ("ems_allure")
      fonts should contain key ("hershey_sans_1")
      println(s"Доступні шрифти: $fonts")
    }

    "завантажувати шрифт hershey_sans_1 та містити missing-glyph (з ключем null)" in {
      Hershey.init()
      val fontKey = "hershey_sans_1" // Переконайтеся, що цей ключ збігається з index.json
      val fontOpt = Hershey.loadFont(fontKey)
      fontOpt should not be empty
      val font = fontOpt.get

      // Перевіряємо, що у мапі glyphs є запис з ключем null, який відповідає missing-glyph
      font.glyphs should contain key (null)
      val missingGlyph = font.glyphs(null)
      missingGlyph.advanceWidth should be > 0.0
    }

    "повернути гліф для символу 'A' для шрифту hershey_sans_1" in {
      Hershey.init()
      val fontKey = "hershey_sans_1"
      // Отримуємо гліф для символу 'A'
      val glyphOpt = Hershey.getGlyph(fontKey, 'A')
      glyphOpt should not be empty
      val glyph = glyphOpt.get

      // Перевіряємо, що Unicode значення гліфа містить символ 'A'
      glyph.unicode match {
        case Some(u) => u.head shouldEqual 'A'
        case None    => fail("Гліф для 'A' не містить Unicode значення")
      }
    }
  }

}

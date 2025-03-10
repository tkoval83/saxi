package com.scala.axidraw

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CanvasSpec extends AnyWordSpec with Matchers {

  "Canvas" should {
    "генерувати коректний SVG для простого тексту Hello World" in {
      // Ініціалізація шрифтів Hershey (припускаємо, що Hershey.init() успішно завантажує шрифти)
      Hershey.init().get

      val helloPaths = Text("Hello World")
        .fontSize(12)
        .alignment(Text.TextAlignment.Center)
        .render()

      val canvas = Canvas(
        paths = helloPaths,
        deviceWidth = 210,
        deviceHeight = 297,
        padding = 10,
        orientation = Canvas.Portrait
      ).scaleToFit.center

      val svg = canvas.toSvg

      svg should startWith("<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
      svg should include("<svg")
      svg should include("<path")
      svg should include("width=\"210")
      svg should include("height=\"297")
    }
  }

}

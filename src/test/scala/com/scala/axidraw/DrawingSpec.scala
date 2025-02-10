package com.scala.axidraw

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

class DrawingSpec extends AnyWordSpec with Matchers {

  "Клас Drawing" should {
    "повертати зображення" in {
      // Створюємо просте креслення: один шлях із двох точок (0,0) та (10,10)
      val p1 = Point(0, 0)
      val p2 = Point(10, 10)
      val path = Path(Seq(p1, p2))
      val drawing = Drawing(Seq(path))

      // Викликаємо метод render із параметрами за замовчуванням:
      // scale = 109, margin = 1 мм.
      val image: BufferedImage = drawing.render()

      // Обчислення очікуваних розмірів зображення:
      // Межі креслення: від (0,0) до (10,10) → розмір = 10 мм,
      // margin у пікселях = margin * scale = 1 * 109 = 109,
      // Розмір зображення = scale * (10 мм) + 2 * margin * scale = 109*10 + 2*109 = 1090 + 218 = 1308 пікселів.
      val scale = 109
      val margin = 1.0
      val m = margin * scale // 109 пікселів
      val expectedSize = scale * 10 + 2 * m // 1090 + 218 = 1308
      image.getWidth shouldBe expectedSize.toInt
      image.getHeight shouldBe expectedSize.toInt

      // Перевіряємо, що піксель у верхньому лівому куті (зона margin) має білий колір.
      image.getRGB(0, 0) shouldBe Color.WHITE.getRGB

      // Перевіряємо, що лінія креслення прорисована:
      // Центр креслення (в системі координат креслення) — точка (5,5).
      // Після аффінного перетворення ця точка відображається як:
      // (margin + 5 * scale, margin + 5 * scale) = (109 + 5*109, 109 + 5*109) = (654, 654)
      val centerX = (m + 5 * scale).toInt
      val centerY = (m + 5 * scale).toInt
      val centerRGB = image.getRGB(centerX, centerY)
      val centerColor = new Color(centerRGB)

      // Через антиаліасинг перевіряємо, що компоненти R, G, B є меншими за поріг (наприклад, 50),
      // що свідчить про темний (майже чорний) колір.
      centerColor.getRed should be < 50
      centerColor.getGreen should be < 50
      centerColor.getBlue should be < 50
    }
    "зберегти зображення у файл" in {
      // Створюємо просте креслення: один шлях із двох точок (0,0) та (10,10)
      val p1 = Point(0, 0)
      val p2 = Point(10, 10)
      val path = Path(Seq(p1, p2))
      val drawing = Drawing(Seq(path))

      // Рендеримо креслення у зображення (BufferedImage)
      val image: BufferedImage = drawing.render()

      // Створюємо тимчасовий файл для збереження зображення.
      val tempFile: File = File.createTempFile("test_drawing", ".png")
      tempFile.deleteOnExit() // Файл буде видалено при завершенні JVM

      // Зберігаємо зображення у файл у форматі PNG
      val written: Boolean = ImageIO.write(image, "png", tempFile)
      written shouldBe true

      // Перевіряємо, що файл існує та його розмір більше нуля.
      tempFile.exists() shouldBe true
      tempFile.length() should be > 0L

      // Виводимо шлях до файлу (для інформації)
      println(s"Зображення збережено у файл: ${tempFile.getAbsolutePath}")
    }
  }
}

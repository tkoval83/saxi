package com.scala.axidraw

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class PlannerSpec extends AnyWordSpec with Matchers {

  "Планувальник" should {
    "коректно розрахувати план для квадрата" in {
      // Створюємо набір точок, що задають квадрат 100 мм x 100 мм
      val points: Seq[Point] = Seq(
        Point(0, 0),
        Point(0, 10),
        Point(10, 10),
        Point(10, 0),
        Point(0, 0)
      )

      // Параметри руху
      val acceleration: Double = 16.0  // мм/с²
      val maxVelocity: Double  = 4.0   // мм/с
      val cornerFactor: Double = 0.001 // коефіцієнт корекції на поворотах

      // Обчислюємо план руху для заданого контуру
      val plan: Plan = Planner.plan(points, acceleration, maxVelocity, cornerFactor)

      // Виведення плану для перевірки (логування)
      println("Обчислений план руху:")
      println(plan)
    }
  }

}

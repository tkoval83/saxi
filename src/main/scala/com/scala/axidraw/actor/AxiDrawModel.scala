package com.scala.axidraw.actor

/** Супутній об'єкт для доступу до моделей AxiDraw */
object AxiDrawModel {

  /** Модель AxiDraw MiniKit 2 з технічними параметрами */
  case object MiniKit2
      extends AxiDrawModel(
        name = "AxiDraw MiniKit 2",
        usablePenTravelInches = (6.3, 4.0),
        usablePenTravelMm = (160.0, 101.0),
        verticalPenTravelInches = 0.7,
        verticalPenTravelMm = 17.0,
        maxXYTravelSpeedInches = 10.0,
        maxXYTravelSpeedCm = 25.0,
        nativeXYResolutionStepsPerInch = 2032,
        nativeXYResolutionStepsPerMm = 80,
        reproducibilityXYInches = 0.005,
        reproducibilityXYMm = 0.1
      )
}

/**
  * Ієрархія моделей AxiDraw з технічними характеристиками
  *
  * @param name                           Назва моделі
  * @param usablePenTravelInches          Робоча зона у дюймах (ширина, висота)
  * @param usablePenTravelMm              Робоча зона у міліметрах (ширина, висота)
  * @param verticalPenTravelInches        Вертикальний хід пера (дюйми)
  * @param verticalPenTravelMm            Вертикальний хід пера (міліметри)
  * @param maxXYTravelSpeedInches         Макс. швидкість переміщення (дюйми/сек)
  * @param maxXYTravelSpeedCm             Макс. швидкість переміщення (см/сек)
  * @param nativeXYResolutionStepsPerInch Роздільна здатність (кроки/дюйм)
  * @param nativeXYResolutionStepsPerMm   Роздільна здатність (кроки/мм)
  * @param reproducibilityXYInches        Точність позиціювання (дюйми)
  * @param reproducibilityXYMm            Точність позиціювання (міліметри)
  */
sealed abstract class AxiDrawModel(
    val name: String,
    val usablePenTravelInches: (Double, Double),
    val usablePenTravelMm: (Double, Double),
    val verticalPenTravelInches: Double,
    val verticalPenTravelMm: Double,
    val maxXYTravelSpeedInches: Double,
    val maxXYTravelSpeedCm: Double,
    val nativeXYResolutionStepsPerInch: Int,
    val nativeXYResolutionStepsPerMm: Int,
    val reproducibilityXYInches: Double,
    val reproducibilityXYMm: Double
)

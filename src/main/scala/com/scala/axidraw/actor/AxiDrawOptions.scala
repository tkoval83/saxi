package com.scala.axidraw.actor

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.implicits.catsSyntaxTuple3Semigroupal
import com.typesafe.config.{Config, ConfigFactory}

/** Супутній об'єкт для створення та валідації конфігурації */
object AxiDrawOptions {

  /** Типи помилок валідації */
  sealed trait ValidationError
  case object InvalidPosition extends ValidationError
  case object InvalidSpeed extends ValidationError
  case object InvalidModel extends ValidationError

  /**
    * Завантаження конфігурації з файлу
    *
    * @param config Об'єкт конфігурації Typesafe
    * @return Результат валідації у форматі Either
    */
  def load(config: Config = ConfigFactory.load()): Either[NonEmptyList[ValidationError], AxiDrawOptions] = {
    def getInt(path: String, default: Int): Int =
      if (config.hasPath(path)) config.getInt(path) else default

    def getDouble(path: String, default: Double): Double =
      if (config.hasPath(path)) config.getDouble(path) else default

    val options = AxiDrawOptions(
      timesliceMs = getInt("axidraw.timeslice_ms", 100),
      microsteppingMode = getInt("axidraw.microstepping_mode", 1),
      penUpPosition = getInt("axidraw.pen_up_position", 60),
      penUpSpeed = getInt("axidraw.pen_up_speed", 150),
      penUpDelay = getInt("axidraw.pen_up_delay", 0),
      penDownPosition = getInt("axidraw.pen_down_position", 40),
      penDownSpeed = getInt("axidraw.pen_down_speed", 150),
      penDownDelay = getInt("axidraw.pen_down_delay", 0),
      acceleration = getDouble("axidraw.acceleration", 16.0),
      maxVelocity = getDouble("axidraw.max_velocity", 4.0),
      cornerFactor = getDouble("axidraw.corner_factor", 0.001),
      model = if (config.hasPath("axidraw.model")) config.getString("axidraw.model") else "MiniKit2",
      port = if (config.hasPath("axidraw.port")) Some(config.getString("axidraw.port")) else Some("EiBotBoard"),
      portConfig = if (config.hasPath("axidraw.port_config")) Some(config.getString("axidraw.port_config")) else None
    )

    validateOptions(options).toEither
  }

  /** Валідація параметрів конфігурації */
  private def validateOptions(options: AxiDrawOptions): ValidatedNel[ValidationError, AxiDrawOptions] = {
    val positionValid = Validated.condNel(
      (0 to 100).contains(options.penUpPosition) &&
      (0 to 100).contains(options.penDownPosition),
      options,
      InvalidPosition
    )

    val speedValid = Validated.condNel(
      options.penUpSpeed >= 0 && options.penDownSpeed >= 0,
      options,
      InvalidSpeed
    )

    val modelValid = Validated.condNel(
      options.getModel.isRight,
      options,
      InvalidModel
    )

    (positionValid, speedValid, modelValid).mapN((_, _, _) => options)
  }
}

/**
  * Конфігураційні параметри AxiDraw
  *
  * @param timesliceMs       Часовий проміжок у мілісекундах
  * @param microsteppingMode Режим мікрокрокування
  * @param penUpPosition     Положення пера у верхньому положенні (0–100%)
  * @param penUpSpeed        Швидкість підйому пера (%/сек)
  * @param penUpDelay        Затримка після підйому пера (мс)
  * @param penDownPosition   Положення пера у нижньому положенні (0–100%)
  * @param penDownSpeed      Швидкість опускання пера (%/сек)
  * @param penDownDelay      Затримка після опускання пера (мс)
  * @param acceleration      Прискорення
  * @param maxVelocity       Максимальна швидкість
  * @param cornerFactor      Коефіцієнт для обробки кутів
  * @param model             Назва моделі пристрою
  * @param port              Опційний USB-порт
  * @param portConfig        Опційне налаштування порту
  */
case class AxiDrawOptions private (
  timesliceMs: Int,
  microsteppingMode: Int,
  penUpPosition: Int,
  penUpSpeed: Int,
  penUpDelay: Int,
  penDownPosition: Int,
  penDownSpeed: Int,
  penDownDelay: Int,
  acceleration: Double,
  maxVelocity: Double,
  cornerFactor: Double,
  model: String,
  port: Option[String],
  portConfig: Option[String]
) {

  /**
    * Отримання моделі пристрою
    *
    * @return Either з моделлю або повідомленням про помилку
    */
  def getModel: Either[String, AxiDrawModel] = model match {
    case "MiniKit2" => Right(AxiDrawModel.MiniKit2)
    case other      => Left(s"Непідтримувана модель: $other")
  }

  val stepDivider: Int = math.pow(2, microsteppingMode - 1).toInt

  val stepsPerInch: Int = getModel match {
    case Right(modelData) => modelData.nativeXYResolutionStepsPerInch / stepDivider
    case Left(_)          => 0
  }

  val stepsPerMm: Int = getModel match {
    case Right(modelData) => modelData.nativeXYResolutionStepsPerMm / stepDivider
    case Left(_)          => 0
  }
}

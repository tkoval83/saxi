package com.scala.axidraw

import com.scala.axidraw.Canvas.{Landscape, Orientation, Portrait}

/**
  * Декоратор для Paths, який додає операції з координатними перетвореннями з урахуванням фізичних параметрів пристрою.
  *
  * @param paths       Шляхи креслення
  * @param deviceWidth Фізична ширина пристрою у міліметрах
  * @param deviceHeight Фізична висота пристрою у міліметрах
  * @param origin      Початкова точка координатної системи пристрою
  * @param padding     Відступ від країв пристрою у міліметрах
  * @param orientation Орієнтація сторінки (портретна/ландшафтна)
  */
case class Canvas(
  paths: Paths,
  deviceWidth: Double,
  deviceHeight: Double,
  origin: Point = Point(0, 0),
  padding: Double = 10.0,
  orientation: Orientation = Portrait
) {

  /**
    * Обчислює ефективні розміри пристрою з урахуванням орієнтації.
    * У портретній орієнтації використовуються задані розміри,
    * а у ландшафтній — значення ширини та висоти обмінюються.
    *
    * @return Пара (ширина, висота) пристрою
    */
  private def effectiveDimensions: (Double, Double) = orientation match {
    case Portrait  => (deviceWidth, deviceHeight)
    case Landscape => (deviceHeight, deviceWidth)
  }

  /**
    * Обчислює межі робочої області пристрою з урахуванням початкової точки та відступів.
    *
    * @return Чотири значення меж: (мінімальне X, мінімальне Y, максимальне X, максимальне Y)
    */
  private def effectiveBounds: (Double, Double, Double, Double) = {
    val (w, h) = effectiveDimensions
    (origin.x + padding, origin.y + padding, origin.x + w - padding, origin.y + h - padding)
  }

  /**
    * Масштабує креслення так, щоб воно помістилося у робочу область пристрою.
    * Робоча область обчислюється з урахуванням фізичних розмірів, початкової точки,
    * відступів та орієнтації.
    *
    * @return Новий об'єкт Canvas з масштабованими та центрованими шляхами
    */
  def scaleToFit: Canvas = {
    val (minX, minY, maxX, maxY) = effectiveBounds
    val effectiveWidth = maxX - minX
    val effectiveHeight = maxY - minY
    copy(paths = paths.scaleToFit(effectiveWidth, effectiveHeight, 0))
  }

  /**
    * Видаляє зі шляхів ті, що містять хоча б одну точку поза межами робочої області пристрою.
    * Фізичні параметри робочої області обчислюються з урахуванням origin, padding та орієнтації.
    *
    * @return Новий об'єкт Canvas з відфільтрованими шляхами
    */
  def removePathsOutside(): Canvas = {
    val (minX, minY, maxX, maxY) = effectiveBounds
    val effectiveWidth = maxX - minX
    val effectiveHeight = maxY - minY
    copy(paths = paths.removePathsOutside(effectiveWidth, effectiveHeight))
  }

  /**
    * Обертає креслення таким чином, щоб воно помістилося у робочу область пристрою.
    * Метод перебирає кути від 0 до 180 градусів з заданим кроком і перевіряє,
    * чи отримане креслення після обертання має ширину та висоту, що не перевищують робочу область.
    *
    * @param step Крок обертання у градусах (за замовчуванням 5)
    * @return Опціональний об'єкт Canvas, в якому шляхи обернені та центровані, якщо знайдено відповідне обертання; None – інакше.
    */
  def rotateToFit(step: Int = 5): Option[Canvas] = {
    val (minX, minY, maxX, maxY) = effectiveBounds
    val effectiveWidth = maxX - minX
    val effectiveHeight = maxY - minY

    paths
      .rotateToFit(effectiveWidth, effectiveHeight, step)
      .map(rotatedPaths => copy(paths = rotatedPaths))
  }

  /**
    * Центрує креслення в робочій області пристрою.
    * Робоча область визначається з урахуванням фізичних розмірів, початкової точки та відступів.
    *
    * @return Новий об'єкт Canvas з центрованими шляхами
    */
  def center: Canvas = {
    val (minX, minY, maxX, maxY) = effectiveBounds
    val effectiveWidth = maxX - minX
    val effectiveHeight = maxY - minY
    copy(paths = paths.center(effectiveWidth, effectiveHeight))
  }

}

object Canvas {

  /**
    * Тип орієнтації сторінки
    */
  sealed trait Orientation

  /**
    * Портретна орієнтація (книжкова)
    */
  case object Portrait extends Orientation

  /**
    * Ландшафтна орієнтація (альбомна)
    */
  case object Landscape extends Orientation
}

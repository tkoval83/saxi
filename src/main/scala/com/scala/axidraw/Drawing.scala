package com.scala.axidraw

import org.locationtech.jts.geom.{Coordinate, GeometryFactory, LineString}
import org.locationtech.jts.simplify.DouglasPeuckerSimplifier

import java.awt._
import java.awt.geom._
import java.awt.image.BufferedImage
import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.math.{max, min, sqrt}

/**
  * Об’єкт з допоміжними константами та утилітними функціями для роботи з геометрією.
  */
object GeometryUtils {

  /** Допустима похибка для порівнянь з нулем. */
  val EPS: Double = 1e-9

  /** Обчислює гіпотенузу (евклідову відстань) для двох чисел. */
  def hypot(x: Double, y: Double): Double = sqrt(x * x + y * y)

  /**
    * Реалізує бінарний пошук для знаходження індексу, де елемент масиву стає більшим або рівним заданому значенню.
    *
    * @param arr Масив чисел.
    * @param x   Шукана величина.
    * @return    Індекс, який відповідає позиції для вставки значення.
    */
  def bisect(arr: ArraySeq[Double], x: Double): Int = {
    @tailrec
    def loop(low: Int, high: Int): Int =
      if (low > high) low
      else {
        val mid = (low + high) / 2
        if (arr(mid) < x) loop(mid + 1, high)
        else loop(low, mid - 1)
      }
    loop(0, arr.length - 1)
  }
}

/**
  * Модель точки у 2D-просторі.
  *
  * @param x Координата X.
  * @param y Координата Y.
  */
case class Point(x: Double, y: Double) {
  import GeometryUtils._

  /** Обчислює довжину вектора (евклідову норму) цієї точки. */
  def length: Double = hypot(x, y)

  /** Нормалізує вектор (приводить його до одиничної довжини). */
  def normalize: Point = {
    val len = length
    if (len <= EPS) Point(0, 0) else Point(x / len, y / len)
  }

  /** Обчислює евклідову відстань до іншої точки. */
  def distance(other: Point): Double = hypot(x - other.x, y - other.y)

  /** Обчислює квадрат евклідової відстані до іншої точки (для оптимізації). */
  def distanceSquared(other: Point): Double = {
    val dx = x - other.x
    val dy = y - other.y
    dx * dx + dy * dy
  }

  /** Виконує векторне додавання з іншою точкою. */
  def +(other: Point): Point = Point(x + other.x, y + other.y)

  /** Виконує векторне віднімання від іншої точки. */
  def -(other: Point): Point = Point(x - other.x, y - other.y)

  /** Множить вектор на скаляр. */
  def *(factor: Double): Point = Point(x * factor, y * factor)

  /** Обчислює скалярний добуток з іншою точкою. */
  def dot(other: Point): Double = x * other.x + y * other.y

  /**
    * Виконує лінійну інтерполяцію (lerp) між цією точкою та іншою.
    *
    * @param other Друга точка.
    * @param s     Відстань від поточної точки, на яку потрібно просунутися.
    * @return      Точка, що знаходиться на прямій між цими точками.
    */
  def lerp(other: Point, s: Double): Point = {
    val dir = (other - this).normalize
    this + (dir * s)
  }

  /**
    * Обчислює відстань від цієї точки до відрізка, заданого двома точками.
    *
    * @param v Початкова точка відрізка.
    * @param w Кінцева точка відрізка.
    * @return  Мінімальну відстань від цієї точки до відрізка.
    */
  def segmentDistance(v: Point, w: Point): Double = {
    val l2 = v.distanceSquared(w)
    if (l2 < EPS) return this.distance(v)
    val t = ((x - v.x) * (w.x - v.x) + (y - v.y) * (w.y - v.y)) / l2
    val tClamped = max(0.0, min(1.0, t))
    val proj = Point(v.x + tClamped * (w.x - v.x), v.y + tClamped * (w.y - v.y))
    distance(proj)
  }

  override def toString: String = {
    def fmt(d: Double): String = if (d % 1 == 0) d.toInt.toString else d.toString
    s"(${fmt(x)}, ${fmt(y)})"
  }
}

/**
  * Модель шляху малювання, що складається з послідовності точок.
  *
  * @param points Послідовність точок, що утворюють шлях малювання
  */
case class Path(points: Seq[com.scala.axidraw.Point]) {

  /**
    * Обчислює довжину шляху як суму відстаней між послідовними точками.
    *
    * @return Довжина шляху
    */
  def length: Double =
    points.sliding(2).collect { case Seq(p1, p2) => p1.distance(p2) }.sum

  /**
    * Спрощує шлях за допомогою алгоритму Douglas-Peucker із заданою допуском.
    *
    * @param tolerance Допустиме відхилення
    * @return Новий Path зі спрощеною послідовністю точок
    */
  def simplify(tolerance: Double): Path = {
    if (points.length < 2)
      return this

    val gf = new GeometryFactory()
    val coords: Array[Coordinate] = points.map(p => new Coordinate(p.x, p.y)).toArray
    val line: LineString = gf.createLineString(coords)
    val simplifiedGeom = DouglasPeuckerSimplifier.simplify(line, tolerance)
    val simplifiedPoints: Seq[com.scala.axidraw.Point] =
      simplifiedGeom.getCoordinates.map(c => com.scala.axidraw.Point(c.x, c.y)).toSeq
    Path(simplifiedPoints)
  }
}

/**
  * Модель креслення, що містить колекцію шляхів малювання.
  *
  * Креслення може мати власні габарити, але його можна масштабувати,
  * щоб воно помістилося у робочу зону пристрою AxiDraw.
  *
  * @param paths Колекція шляхів малювання (Path)
  */
case class Drawing(paths: Seq[Path]) {

  /**
    * Повертає всі точки креслення як послідовність об'єктів Point.
    *
    * @return Послідовність об'єктів Point
    */
  def points: Seq[com.scala.axidraw.Point] = paths.flatMap(_.points)

  /**
    * Спрощує креслення, застосовуючи алгоритм Douglas-Peucker до кожного шляху.
    *
    * @param tolerance Допустиме відхилення для спрощення (мм)
    * @return Нове креслення з спрощеними шляхами
    */
  def simplify(tolerance: Double): Drawing =
    Drawing(paths.map(_.simplify(tolerance)))

  /**
    * Трансформує креслення за допомогою заданої функції перетворення для кожної точки.
    *
    * @param f Функція, що приймає точку і повертає нову точку (наприклад, для обертання, перенесення чи іншого аффінного перетворення)
    * @return Нове креслення з трансформованими точками
    */
  def transform(f: com.scala.axidraw.Point => com.scala.axidraw.Point): Drawing = {
    val transformedPaths = paths.map { path =>
      path.copy(points = path.points.map(f))
    }
    Drawing(transformedPaths)
  }

  /**
    * Переносить (транслює) креслення на задані величини по осях X та Y.
    *
    * @param dx Зміщення по осі X (мм)
    * @param dy Зміщення по осі Y (мм)
    * @return Нове креслення з перенесеними точками
    */
  def translate(dx: Double, dy: Double): Drawing =
    transform(p => com.scala.axidraw.Point(p.x + dx, p.y + dy))

  /**
    * Масштабує креслення за заданим коефіцієнтом.
    *
    * @param factor Коефіцієнт масштабування (наприклад, 0.5 для зменшення, 2.0 для збільшення)
    * @return Нове креслення з масштабованими точками
    */
  def scale(factor: Double): Drawing =
    transform(p => com.scala.axidraw.Point(p.x * factor, p.y * factor))

  /**
    * Обертає креслення на заданий кут (в радіанах) навколо початку координат.
    *
    * @param angle Кут обертання в радіанах
    * @return Нове креслення з оберненими точками
    */
  def rotate(angle: Double): Drawing = {
    val cosA = math.cos(angle)
    val sinA = math.sin(angle)
    transform(p => com.scala.axidraw.Point(p.x * cosA - p.y * sinA, p.x * sinA + p.y * cosA))
  }

  /**
    * Обчислює габаритні розміри креслення (ширина та висота).
    *
    * @return Пара (width, height) у міліметрах
    */
  def dimensions: (Double, Double) = {
    val xs = paths.flatMap(_.points.map(_.x))
    val ys = paths.flatMap(_.points.map(_.y))
    if (xs.isEmpty || ys.isEmpty) (0.0, 0.0)
    else (xs.max - xs.min, ys.max - ys.min)
  }

  /**
    * Обчислює межі креслення: (мінімальне X, мінімальне Y, максимальне X, максимальне Y).
    *
    * @return Чотири значення меж креслення
    */
  def bounds: (Double, Double, Double, Double) = {
    val xs = paths.flatMap(_.points.map(_.x))
    val ys = paths.flatMap(_.points.map(_.y))
    if (xs.isEmpty || ys.isEmpty) (0.0, 0.0, 0.0, 0.0)
    else (xs.min, ys.min, xs.max, ys.max)
  }

  /**
    * Обчислює ширину креслення як різницю між максимальним та мінімальним значеннями X.
    *
    * @return Ширина креслення (мм)
    */
  def width: Double = {
    val (x1, _, x2, _) = bounds
    x2 - x1
  }

  /**
    * Обчислює висоту креслення як різницю між максимальним та мінімальним значеннями Y.
    *
    * @return Висота креслення (мм)
    */
  def height: Double = {
    val (_, y1, _, y2) = bounds
    y2 - y1
  }

  /**
    * Обчислює розміри креслення у вигляді пари (width, height).
    *
    * @return Пара (width, height) у міліметрах
    */
  def size: (Double, Double) = (width, height)

  /**
    * Переміщує креслення так, щоб точка, що розташована у відносних координатах (ax, ay) меж креслення,
    * опинилася в точці (x, y). Відносні координати ax та ay мають бути в межах [0, 1],
    * де 0 відповідає мінімальній координаті, а 1 – максимальній.
    *
    * @param x Цільова координата X (мм)
    * @param y Цільова координата Y (мм)
    * @param ax Відносна координата X меж креслення (від 0 до 1)
    * @param ay Відносна координата Y меж креслення (від 0 до 1)
    * @return Нове креслення з переміщеними точками
    */
  def move(x: Double, y: Double, ax: Double, ay: Double): Drawing = {
    val (x1, y1, x2, y2) = bounds
    val dx = x1 + (x2 - x1) * ax - x
    val dy = y1 + (y2 - y1) * ay - y
    translate(-dx, -dy)
  }

  /**
    * Центрує креслення у вказаних розмірах за допомогою методу move.
    * Наприклад, якщо задано ширину і висоту робочої області, центр креслення
    * буде переміщено в точку (width/2, height/2).
    *
    * @param width Загальна ширина області (мм)
    * @param height Загальна висота області (мм)
    * @return Нове креслення з центруванням
    */
  def center(width: Double, height: Double): Drawing =
    move(width / 2.0, height / 2.0, 0.5, 0.5)

  /**
    * Масштабує креслення так, щоб воно помістилося у задані габарити з урахуванням відступів.
    *
    * @param width Загальна ширина області (мм)
    * @param height Загальна висота області (мм)
    * @param padding Відступ від країв (мм), за замовчуванням 0
    * @return Нове креслення, масштабоване та центроване відповідно до вказаних габаритів
    */
  def scaleToFit(width: Double, height: Double, padding: Double = 0): Drawing = {
    val availableWidth = width - padding * 2
    val availableHeight = height - padding * 2
    val scaleFactor = math.min(availableWidth / width, availableHeight / height)
    scale(scaleFactor).center(availableWidth, availableHeight)
  }

  /**
    * Обертає креслення так, щоб воно помістилося у задану область.
    *
    * Метод перебирає кути від 0 до 180 градусів із заданим кроком (step).
    * Для кожного кута виконується обертання креслення; якщо отримане креслення має
    * ширину та висоту, що не перевищують width та height, воно центрується і повертається.
    * Якщо жодне з обертань не задовольняє умови, повертається None.
    *
    * @param width Цільова ширина області (мм)
    * @param height Цільова висота області (мм)
    * @param step Крок обертання (у градусах), за замовчуванням 5
    * @return Опціональне креслення, що відповідає умовам
    */
  def rotateToFit(width: Double, height: Double, step: Int = 5): Option[Drawing] =
    (0 until 180 by step).iterator
      .map { angle =>
        val rad = angle * math.Pi / 180.0
        val rotated = rotate(rad)
        if (rotated.width <= width && rotated.height <= height)
          Some(rotated.center(width, height))
        else
          None
      }
      .find(_.isDefined)
      .flatten

  /**
    * Видаляє всі шляхи, що містять хоча б одну точку поза межами заданої області.
    *
    * @param width Ширина області (мм)
    * @param height Висота області (мм)
    * @return Нове креслення, яке містить лише ті шляхи, що повністю знаходяться в межах області
    */
  def removePathsOutside(width: Double, height: Double): Drawing = {
    val e = 1e-8
    val validPaths = paths.filter { path =>
      path.points.forall { p =>
        p.x >= -e && p.y >= -e && p.x <= width + e && p.y <= height + e
      }
    }
    Drawing(validPaths)
  }

  /**
    * Рендерить креслення в BufferedImage, використовуючи AWT.
    *
    * @param scale Масштабний коефіцієнт (за замовчуванням 109)
    * @param margin Відступ (в міліметрах, за замовчуванням 1) – буде помножено на scale
    * @param lineWidth Товщина лінії (в міліметрах, за замовчуванням 0.35/25.4)
    * @param boundsOpt Опціональні межі креслення у форматі (x1, y1, x2, y2). Якщо не задано – використовується метод bounds.
    * @param showBounds Прапорець, чи малювати межі (bounding box) креслення
    * @return BufferedImage із зображенням креслення
    */
  def render(
    scale: Double = 109,
    margin: Double = 1,
    lineWidth: Double = 0.35 / 25.4,
    boundsOpt: Option[(Double, Double, Double, Double)] = None,
    showBounds: Boolean = true
  ): BufferedImage = {
    // Визначення меж: якщо boundsOpt не задано, використовуємо метод bounds креслення
    val (x1, y1, x2, y2) = boundsOpt.getOrElse(bounds)
    val w = x2 - x1
    val h = y2 - y1

    // Обчислення відступу (margin) та розмірів зображення (margin враховано)
    val m = margin * scale
    val imageWidth = (scale * w + 2 * m).toInt
    val imageHeight = (scale * h + 2 * m).toInt

    // Створення BufferedImage
    val image = new BufferedImage(imageWidth, imageHeight, BufferedImage.TYPE_INT_RGB)
    val g2 = image.createGraphics()
    try {
      // Включення антиаліасингу
      g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

      // *** FIX: Скидаємо трансформацію до одиничної та заповнюємо весь фон білим кольором.
      g2.setTransform(new AffineTransform()) // скидання до одиничної матриці
      g2.setColor(Color.WHITE)
      g2.fillRect(0, 0, imageWidth, imageHeight)

      // Побудова аффінного перетворення:
      // 1. Перенос на відступ margin (m)
      // 2. Масштабування на scale
      // 3. Перенос (-x1, -y1), щоб креслення розпочиналося з (x1, y1)
      val at = new AffineTransform()
      at.translate(m, m)
      at.scale(scale, scale)
      at.translate(-x1, -y1)
      g2.setTransform(at)

      // За бажанням, малюємо межі (bounding box) креслення сірим кольором
      if (showBounds) {
        g2.setColor(Color.GRAY)
        g2.setStroke(new BasicStroke((1.0 / scale).toFloat))
        g2.draw(new Rectangle2D.Double(x1, y1, w, h))
      }

      // Малювання шляхів креслення чорним кольором
      g2.setColor(Color.BLACK)
      g2.setStroke(new BasicStroke(lineWidth.toFloat, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND))
      for (path <- paths if path.points.nonEmpty) {
        val path2d = new Path2D.Double()
        val first = path.points.head
        path2d.moveTo(first.x, first.y)
        path.points.tail.foreach { p =>
          path2d.lineTo(p.x, p.y)
        }
        g2.draw(path2d)
      }
    } finally {
      g2.dispose()
    }
    image
  }

}

/**
  * Допоміжний об'єкт для роботи з кресленнями.
  */
object Drawing {}

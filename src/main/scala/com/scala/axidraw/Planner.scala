package com.scala.axidraw

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.math._

/**
  * Модель миттєвого стану руху.
  *
  * @param t Час від початку руху.
  * @param p Поточна позиція.
  * @param s Пройдена відстань.
  * @param v Миттєва швидкість.
  * @param a Миттєве прискорення.
  */
case class Instant(t: Double, p: Point, s: Double, v: Double, a: Double)

/**
  * Блок руху, який характеризується постійним прискоренням (або гальмуванням).
  *
  * @param a  Прискорення (якщо негативне – гальмування).
  * @param t  Тривалість блоку (в секундах); має бути невід’ємним.
  * @param vi Початкова швидкість у цьому блоці.
  * @param p1 Початкова точка блоку.
  * @param p2 Кінцева точка блоку.
  */
case class Block(a: Double, t: Double, vi: Double, p1: Point, p2: Point) {
  require(t >= 0, "Час блоку не може бути від'ємним")

  /** Загальна відстань, яку має пройти блок (евклідова відстань між p1 та p2). */
  val s: Double = p1.distance(p2)

  /**
    * Обчислює миттєвий стан руху в блоці для заданого локального часу.
    *
    * @param tLocal Локальний час (в секундах) всередині блоку.
    * @param dt     Додаткове зміщення часу від початку плану.
    * @param ds     Додаткове зміщення відстані від початку плану.
    * @return       Модель Instant, що описує стан руху.
    */
  def instant(tLocal: Double, dt: Double = 0, ds: Double = 0): Instant = {
    val tClamped = max(0.0, min(t, tLocal))
    val vLocal = vi + a * tClamped
    val sLocal = vi * tClamped + a * tClamped * tClamped / 2
    val sClamped = max(0.0, min(s, sLocal))
    Instant(
      t = tClamped + dt,
      p = p1.lerp(p2, sClamped),
      s = sClamped + ds,
      v = vLocal,
      a = a
    )
  }

  override def toString: String = {
    def fmtInt(d: Double): String = if (d % 1 == 0) d.toInt.toString else d.toString
    s"""Блок руху:
  Прискорення: ${fmtInt(a)}
  Тривалість: $t
  Початкова швидкість: ${fmtInt(vi)}
  Відстань: $s
  Початкова точка: $p1
  Кінцева точка: $p2"""
  }
}

/**
  * План руху, що складається з послідовності блоків.
  *
  * @param blocks Вектор блоків руху.
  */
case class Plan(blocks: Vector[Block]) {
  // Обчислюємо кумулятивний час та пройдену відстань для кожного блоку
  private val (ts, ss) = blocks
    .scanLeft((0.0, 0.0)) {
      case ((tAcc, sAcc), block) => (tAcc + block.t, sAcc + block.s)
    }
    .drop(1)
    .unzip

  /** Загальний час виконання плану (в секундах). */
  val totalTime: Double = ts.lastOption.getOrElse(0.0)

  /** Загальна пройдена відстань (в мм). */
  val totalDistance: Double = ss.lastOption.getOrElse(0.0)

  /**
    * Обчислює миттєвий стан руху для заданого часу.
    *
    * @param t Час від початку руху (в секундах).
    * @return  Instant, що описує стан руху у заданий момент часу.
    */
  def atTime(t: Double): Instant = {
    val tClamped = max(0.0, min(totalTime, t))
    val i = ts.indexWhere(_ > tClamped)
    val idx = if (i <= 0) 0 else i - 1
    val block = blocks(idx)
    block.instant(tClamped - ts(idx), ts(idx), ss(idx))
  }

  override def toString: String = {
    val sb = new StringBuilder
    sb.append(
      f"План:\n  Загальний час: $totalTime%.2f секунд\n  Загальна відстань: $totalDistance%.2f мм\n  Кількість блоків: ${blocks.length}\n"
    )
    blocks.foreach { block =>
      sb.append(block.toString + "\n")
    }
    sb.toString
  }
}

/**
  * Допоміжна структура для опису трикутного профілю руху, який складається з фаз прискорення та уповільнення.
  *
  * @param s1   Відстань, пройдена у фазі прискорення.
  * @param s2   Відстань, пройдена у фазі уповільнення.
  * @param t1   Час фази прискорення.
  * @param t2   Час фази уповільнення.
  * @param vmax Максимальна досягнута швидкість.
  * @param p1   Початкова точка руху.
  * @param p2   Точка, до якої досягається vmax.
  * @param p3   Кінцева точка руху.
  */
case class Triangle(
  s1: Double,
  s2: Double,
  t1: Double,
  t2: Double,
  vmax: Double,
  p1: Point,
  p2: Point,
  p3: Point
)

/**
  * Допоміжна структура для опису трапецієподібного профілю руху, який складається з фаз прискорення, круїзу та уповільнення.
  *
  * @param s1 Відстань, пройдена у фазі прискорення.
  * @param s2 Відстань, пройдена у фазі круїзного руху.
  * @param s3 Відстань, пройдена у фазі уповільнення.
  * @param t1 Час фази прискорення.
  * @param t2 Час фази круїзу.
  * @param t3 Час фази уповільнення.
  * @param p1 Початкова точка.
  * @param p2 Точка після фази прискорення.
  * @param p3 Точка перед фазою уповільнення.
  * @param p4 Кінцева точка.
  */
case class Trapezoid(
  s1: Double,
  s2: Double,
  s3: Double,
  t1: Double,
  t2: Double,
  t3: Double,
  p1: Point,
  p2: Point,
  p3: Point,
  p4: Point
)

/**
  * Сегмент шляху між двома точками.
  *
  * @param p1 Початкова точка сегмента.
  * @param p2 Кінцева точка сегмента.
  */
case class Segment(p1: Point, p2: Point) {

  /** Довжина сегмента (евклідова відстань між p1 та p2). */
  val length: Double = p1.distance(p2)

  /** Нормалізований напрямний вектор від p1 до p2. */
  val direction: Point = (p2 - p1).normalize

  /** Максимальна швидкість, з якою можна увійти в цей сегмент. */
  var maxEntryVelocity: Double = 0.0

  /** Початкова швидкість при вході в сегмент. */
  var entryVelocity: Double = 0.0

  /** Список блоків руху, що формують цей сегмент. */
  var blocks: Vector[Block] = Vector.empty

  override def toString: String =
    s"""Сегмент:
  Початкова точка: $p1
  Кінцева точка: $p2
  Довжина: $length
  Вхідна швидкість: $entryVelocity
  Максимальна вхідна швидкість: $maxEntryVelocity"""
}

/**
  * Обмежувач швидкості для коригування руху по шляху.
  *
  * Використовує дискретну часове квантування для обчислення максимальної швидкості,
  * з якою можна рухатись уздовж шляху, з урахуванням геометрії траєкторії.
  *
  * @param points    Послідовність точок шляху.
  * @param vmax      Гранична швидкість.
  * @param dt        Часовий крок (в секундах).
  * @param threshold Допустиме відхилення від прямої.
  */
case class Throttler(points: ArraySeq[Point], vmax: Double, dt: Double, threshold: Double) {
  import GeometryUtils._

  /**
    * Обчислює кумулятивні відстані для кожної точки.
    *
    * Для N точок повертає ArraySeq довжиною N, де перший елемент завжди 0.0,
    * а наступні — сума відстаней між послідовними точками.
    */
  private val distances: ArraySeq[Double] = {
    // Використовуємо scanLeft без drop, щоб отримати вектор довжин однакової довжини з points.
    points
      .sliding(2)
      .map {
        case Seq(p1, p2) => p1.distance(p2)
        case _           => 0.0
      }
      .scanLeft(0.0)(_ + _)
      .to(ArraySeq)
  }

  /**
    * Знаходить індекс точки, для якої кумулятивна відстань перевищує задане значення.
    *
    * @param d Шукана відстань.
    * @return  Індекс точки.
    */
  private def findIndex(d: Double): Int = {
    val index = bisect(distances, d)
    if (index >= points.length) points.length - 1 else index
  }

  /**
    * Перевіряє, чи є рух від заданої точки з певною швидкістю допустимим.
    *
    * Перевірка базується на тому, що всі точки між початковою та кінцевою
    * (за відповідним інтервалом кумулятивних відстаней) повинні лежати не далі за threshold від прямої.
    *
    * @param i0 Початковий індекс точки.
    * @param v  Швидкість руху (мм/с).
    * @return   true, якщо рух допустимий, інакше false.
    */
  def isFeasible(i0: Int, v: Double): Boolean = {
    val d = v * dt
    val x0 = distances(i0)
    val x1 = x0 + d
    val i1 = findIndex(x1)
    if (i0 == i1) return true

    val p0 = points(i0)
    val p10 = points(i1)
    val p11 = points.lift(i1 + 1).getOrElse(p10)
    val s = x1 - distances(i1)
    val p1 = p10.lerp(p11, s)

    (i0 + 1 to i1).forall { i =>
      points(i).segmentDistance(p0, p1) <= threshold
    }
  }

  /**
    * Обчислює максимальну допустиму швидкість для точки з заданим індексом.
    *
    * @param index Індекс точки.
    * @return      Максимальна допустима швидкість (мм/с) для цієї точки.
    */
  def maxVelocityAt(index: Int): Double = {
    if (isFeasible(index, vmax)) return vmax

    @tailrec
    def binarySearch(low: Double, high: Double, iterations: Int): Double =
      if (iterations <= 0) (low + high) / 2
      else {
        val mid = (low + high) / 2
        if (isFeasible(index, mid)) binarySearch(mid, high, iterations - 1)
        else binarySearch(low, mid, iterations - 1)
      }
    binarySearch(0, vmax, 16)
  }

  /**
    * Обчислює максимально допустимі швидкості для кожної точки шляху.
    *
    * @return ArraySeq з максимально допустимими швидкостями, де довжина відповідає кількості точок.
    */
  def computeMaxVelocities: ArraySeq[Double] =
    ArraySeq.tabulate(points.size)(maxVelocityAt)
}

/**
  * Основний планувальник руху з постійним прискоренням.
  *
  * Реалізує алгоритм побудови плану руху, розбиваючи траєкторію на сегменти та блоки руху,
  * з урахуванням кутових обмежень (cornering) та максимальних швидкостей.
  */
object Planner {
  import GeometryUtils._

  /**
    * Фабричний метод, що конвертує послідовність точок у план руху,
    * використовуючи задане прискорення, граничну швидкість та коефіцієнт для поворотів.
    *
    * @param points       Послідовність точок (Point).
    * @param acceleration Прискорення (мм/с²).
    * @param maxVelocity  Гранична швидкість (мм/с).
    * @param cornerFactor Фактор корекції швидкості на поворотах.
    * @return План руху (Plan) для заданого шляху.
    */
  def plan(
    points: Seq[Point],
    acceleration: Double,
    maxVelocity: Double,
    cornerFactor: Double
  ): Plan = constantAccelerationPlan(points, acceleration, maxVelocity, cornerFactor)

  /**
    * Фабричний метод, що конвертує шлях (Path) у план руху.
    *
    * @param path         Шлях (Path), що містить послідовність точок.
    * @param acceleration Прискорення (мм/с²).
    * @param maxVelocity  Гранична швидкість (мм/с).
    * @param cornerFactor Фактор корекції швидкості на поворотах.
    * @return План руху (Plan) для заданого шляху.
    */
  def plan(
    path: Path,
    acceleration: Double,
    maxVelocity: Double,
    cornerFactor: Double
  ): Plan = plan(path.points, acceleration, maxVelocity, cornerFactor)

  /**
    * Фабричний метод, що конвертує креслення (Drawing) у масив планів руху.
    *
    * Оскільки шляхи можуть бути не з'єднані, для кожного окремого шляху
    * створюється окремий план руху.
    *
    * @param drawing      Креслення, яке містить один або декілька шляхів.
    * @param acceleration Прискорення (мм/с²).
    * @param maxVelocity  Гранична швидкість (мм/с).
    * @param cornerFactor Фактор корекції швидкості на поворотах.
    * @return Послідовність планів руху (Seq[Plan]) для кожного шляху.
    */
  def plans(
    drawing: Drawing,
    acceleration: Double,
    maxVelocity: Double,
    cornerFactor: Double
  ): Seq[Plan] =
    drawing.paths.map(path => plan(path.points, acceleration, maxVelocity, cornerFactor))

  /**
    * Обчислює максимально допустиму швидкість на повороті між двома сегментами.
    *
    * @param s1     Перший сегмент.
    * @param s2     Другий сегмент.
    * @param vmax   Гранична швидкість.
    * @param a      Прискорення.
    * @param factor Фактор корекції швидкості на поворотах.
    * @return       Максимальна швидкість, яку можна досягти на повороті.
    */
  private def cornerVelocity(
    s1: Segment,
    s2: Segment,
    vmax: Double,
    a: Double,
    factor: Double
  ): Double = {
    val cosine = -s1.direction.dot(s2.direction)
    if ((cosine - 1.0).abs < EPS) return 0.0
    val sine = math.sqrt((1.0 - cosine) / 2.0)
    if ((sine - 1.0).abs < EPS) return vmax
    val v = math.sqrt((a * factor * sine) / (1.0 - sine))
    math.min(v, vmax)
  }

  /**
    * Побудова плану руху з постійним прискоренням.
    *
    * Розбиває заданий шлях (послідовність точок) на сегменти, обчислює максимально допустимі швидкості
    * для кожного сегмента з використанням Throttler, визначає кутові обмеження та формує блоки руху,
    * використовуючи трикутний або трапецієподібний профіль руху.
    *
    * @param points       Послідовність точок шляху.
    * @param acceleration Прискорення (мм/с²).
    * @param maxVelocity  Гранична швидкість (мм/с).
    * @param cornerFactor Фактор корекції швидкості на поворотах.
    * @return             План руху (Plan), що складається з блоків.
    */
  private def constantAccelerationPlan(
    points: Seq[Point],
    acceleration: Double,
    maxVelocity: Double,
    cornerFactor: Double
  ): Plan = {
    val throttler = Throttler(
      ArraySeq(points: _*),
      maxVelocity,
      dt = 0.02,
      threshold = 0.001
    )
    val maxVelocities = throttler.computeMaxVelocities

    // Формуємо сегменти між послідовними точками; додаємо dummy-сегмент для останньої точки.
    val segments = (points
      .sliding(2)
      .collect { case Seq(p1, p2) => Segment(p1, p2) })
      .toVector :+ Segment(points.last, points.last)

    // Призначаємо максимальні швидкості для входу в сегменти та обчислюємо швидкість на поворотах.
    segments.zip(maxVelocities).zip(segments.tail).foreach {
      case ((s1, v), s2) =>
        s1.maxEntryVelocity = min(s1.maxEntryVelocity, v)
        s2.maxEntryVelocity = cornerVelocity(s1, s2, maxVelocity, acceleration, cornerFactor)
    }

    /**
      * Рекурсивна функція для обробки сегментів та формування блоків руху.
      *
      * @param index Індекс поточного сегмента.
      */
    @tailrec
    def processSegments(index: Int): Unit = {
      if (index >= segments.size - 1) return

      val current = segments(index)
      val next = segments(index + 1)
      val s = current.length
      val vi = current.entryVelocity
      val vexit = next.maxEntryVelocity

      val triangleParams = computeTriangle(s, vi, vexit, acceleration, current.p1, current.p2)
      if (triangleParams.s1 < -EPS) {
        // Якщо обчислений параметр s1 негативний, знижуємо вхідну швидкість сегмента та повертаємося для корекції.
        current.maxEntryVelocity = sqrt(vexit * vexit + 2 * acceleration * s)
        // Захищаємося від негативного індексу: якщо index==0, переходимо до наступного сегмента.
        if (index > 0) processSegments(index - 1)
        else processSegments(index + 1)
      } else if (triangleParams.s2 < 0) {
        // Якщо s2 менше нуля, використовуємо профіль руху з лише прискоренням.
        val vf = sqrt(vi * vi + 2 * acceleration * s)
        current.blocks = Vector(Block(acceleration, (vf - vi) / acceleration, vi, current.p1, current.p2))
        next.entryVelocity = vf
        processSegments(index + 1)
      } else if (triangleParams.vmax > maxVelocity) {
        // Якщо максимальна швидкість за профілем перевищує граничну, використовуємо трапецієподібний профіль.
        val trapezoidParams = computeTrapezoid(s, vi, maxVelocity, vexit, acceleration, current.p1, current.p2)
        current.blocks = Vector(
          Block(acceleration, trapezoidParams.t1, vi, trapezoidParams.p1, trapezoidParams.p2),
          Block(0, trapezoidParams.t2, maxVelocity, trapezoidParams.p2, trapezoidParams.p3),
          Block(-acceleration, trapezoidParams.t3, maxVelocity, trapezoidParams.p3, trapezoidParams.p4)
        )
        next.entryVelocity = vexit
        processSegments(index + 1)
      } else {
        // Інакше використовуємо трикутний профіль (прискорення та уповільнення).
        current.blocks = Vector(
          Block(acceleration, triangleParams.t1, vi, triangleParams.p1, triangleParams.p2),
          Block(-acceleration, triangleParams.t2, triangleParams.vmax, triangleParams.p2, triangleParams.p3)
        )
        next.entryVelocity = vexit
        processSegments(index + 1)
      }
    }

    processSegments(0)

    val allBlocks = segments.flatMap(_.blocks).filter(_.t > EPS)
    Plan(allBlocks)
  }

  /**
    * Обчислює параметри трикутного профілю руху (прискорення та уповільнення).
    *
    * @param s  Довжина сегмента.
    * @param vi Початкова швидкість.
    * @param vf Кінцева швидкість.
    * @param a  Прискорення.
    * @return   Обчислений трикутний профіль (Triangle).
    */
  private def computeTriangle(
    s: Double,
    vi: Double,
    vf: Double,
    a: Double,
    p1: Point,
    p3: Point
  ): Triangle = {
    val s1 = (2 * a * s + vf * vf - vi * vi) / (4 * a)
    val s2 = s - s1
    val vmax = sqrt(vi * vi + 2 * a * s1)
    val t1 = (vmax - vi) / a
    val t2 = (vf - vmax) / -a
    val p2 = p1.lerp(p3, s1)

    Triangle(
      s1,
      s2,
      t1,
      t2,
      vmax,
      p1,
      p2,
      p3
    )
  }

  /**
    * Обчислює параметри трапецієподібного профілю руху (прискорення, круїз та уповільнення).
    *
    * @param s    Довжина сегмента.
    * @param vi   Початкова швидкість.
    * @param vmax Максимальна швидкість у фазі круїзу.
    * @param vf   Кінцева швидкість.
    * @param a    Прискорення.
    * @return     Обчислений трапецієподібний профіль (Trapezoid).
    */
  private def computeTrapezoid(
    s: Double,
    vi: Double,
    vmax: Double,
    vf: Double,
    a: Double,
    p1: Point,
    p4: Point
  ): Trapezoid = {
    val t1 = (vmax - vi) / a
    val s1 = (vmax + vi) / 2 * t1
    val t3 = (vf - vmax) / (-a)
    val s3 = (vf + vmax) / 2 * t3
    val s2 = s - s1 - s3
    val t2 = s2 / vmax

    val p2 = p1.lerp(p4, s1)
    val p3 = p1.lerp(p4, s - s3)

    Trapezoid(
      s1,
      s2,
      s3,
      t1,
      t2,
      t3,
      p1,
      p2,
      p3,
      p4
    )
  }
}

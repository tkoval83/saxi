package com.scala.axidraw.actor

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior, PostStop}
import cats.data.NonEmptyList
import com.scala.axidraw.GeometryUtils.EPS
import com.scala.axidraw._
import com.typesafe.config.{Config, ConfigFactory}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

/**
  * Головний актор керування пристроєм AxiDraw
  */
object AxiDrawActor {

  // Константи для сервоприводу
  final private val ServoMin: Int = 7500
  final private val ServoMax: Int = 28000
  final private val SpeedScaling: Int = 5

  /**
    * Повідомлення, які підтримує AxiDrawActor.
    */
  sealed trait Command

  /**
    * Команда для запуску малювання (складається з одного або кількох шляхів).
    */
  final case class Draw(drawing: Drawing) extends Command

  /**
    * Внутрішнє повідомлення про успішне завершення креслення.
    */
  private case object DrawingDone extends Command

  /**
    * Внутрішнє повідомлення про невдале виконання креслення (з помилкою).
    */
  final private case class DrawingFailed(ex: Throwable) extends Command

  /**
    * Фабричний метод для створення поведінки актора
    *
    * @param config Опційна конфігурація (якщо не передано, використовується конфігурація за замовчуванням)
    * @return Поведінка актора типу Nothing
    */
  def apply(config: Option[Config] = None): Behavior[Command] =
    Behaviors.setup[Command] { context =>
      // Спробуємо завантажити конфігурацію
      AxiDrawOptions.load(config.getOrElse(ConfigFactory.load())) match {
        case Left(errors) =>
          // Якщо є помилки валідації, логуємо їх і завершуємо актор
          handleErrors(errors, context)
          Behaviors.stopped

        case Right(validOptions) =>
          // Якщо все добре, переходимо до ініціалізації пристрою
          initializeDevice(validOptions, context)
      }
    }

  /**
    * Обробка помилок валідації
    */
  private def handleErrors(
    errors: NonEmptyList[AxiDrawOptions.ValidationError],
    context: ActorContext[Command]
  ): Unit =
    errors.toList.foreach {
      case AxiDrawOptions.InvalidPosition =>
        context.log.error("Позиції пера мають бути в діапазоні 0-100%")
      case AxiDrawOptions.InvalidSpeed =>
        context.log.error("Швидкості мають бути невід'ємними")
      case AxiDrawOptions.InvalidModel =>
        context.log.error("Вказана модель пристрою не підтримується")
    }

  /**
    * Ініціалізація пристрою
    */
  private def initializeDevice(
    options: AxiDrawOptions,
    context: ActorContext[Command]
  ): Behavior[Command] = {
    context.log.info(s"Ініціалізація пристрою ${options.model}")

    // Створюємо актор для послідовного порту та актор EiBotBoard
    val serialActor = context.spawn(SerialActor(), "serial-actor")
    val ebotActor = context.spawn(EiBotBoardActor(serialActor), "ebb-actor")

    // Конфігуруємо налаштування сервоприводів пера
    val (upPos, downPos, upSpeed, downSpeed) = calculateServoSettings(options)
    sendConfigurationCommands(ebotActor, upPos, downPos, upSpeed, downSpeed)

    // Повертаємося в idle (готові приймати Draw)
    idle(options, ebotActor)
  }

  /**
    * Стан `idle`: актор готовий приймати команду Draw, аби почати креслення.
    */
  private def idle(
    options: AxiDrawOptions,
    ebotActor: ActorRef[EiBotBoardActor.Command],
  ): Behavior[Command] =
    Behaviors.setup { context =>
      Behaviors
        .receiveMessage[Command] {
          case Draw(drawing) =>
            implicit val ec: ExecutionContext = context.executionContext

            // Використовуємо pipeToSelf для кращого оброблення помилок
            context.pipeToSelf(Future {
              runPaths(drawing.paths, options, ebotActor, context)
            }) {
              case Success(_)  => DrawingDone
              case Failure(ex) => DrawingFailed(ex)
            }

            // Переходимо у стан busy
            busy(options, ebotActor)

          case other =>
            context.log.warn(s"Непередбачене повідомлення в стані idle: $other")
            Behaviors.same
        }
        .receiveSignal {
          case (_, PostStop) =>
            // Якщо актор зупиняється, вимикаємо пристрій
            shutdownDevice(ebotActor, context)
            Behaviors.same
        }
    }

  /**
    * Стан `busy`: йде процес малювання. Нові `Draw` ігноруються,
    * аж поки процес не завершиться чи не зазнає помилки.
    */
  private def busy(
    options: AxiDrawOptions,
    ebotActor: ActorRef[EiBotBoardActor.Command]
  ): Behavior[Command] =
    Behaviors.setup { context =>
      Behaviors
        .receiveMessage[Command] {
          case Draw(_) =>
            context.log.warn("Креслення вже виконується. Нова команда Draw ігнорується.")
            Behaviors.same

          case DrawingDone =>
            context.log.info("Креслення завершено. Повертаюсь у стан idle.")
            idle(options, ebotActor)

          case DrawingFailed(ex) =>
            context.log.error("Помилка під час виконання креслення: ", ex)
            // Можна додати логіку повторної спроби, якщо потрібно
            idle(options, ebotActor)

          case other =>
            context.log.warn(s"Непередбачене повідомлення в стані busy: $other")
            Behaviors.same
        }
        .receiveSignal {
          case (_, PostStop) =>
            // Якщо актор зупиняється
            shutdownDevice(ebotActor, context)
            Behaviors.same
        }
    }

  /**
    * Виконує всі шляхи креслення з перевіркою з'єднувальних відстаней між шляхами.
    * Якщо відстань між кінцевою точкою попереднього шляху та першою точкою поточного шляху перевищує
    * поріг (gapThreshold), перо піднімається (pen up), а потім опускається (pen down) для початку нового шляху.
    *
    * @param paths      Кілька шляхів.
    * @param options    Конфігураційні параметри AxiDraw.
    * @param ebotActor  Актор для надсилання команд до пристрою (EiBotBoardActor).
    * @param context    Контекст актора.
    * @param ec         ExecutionContext для роботи з Future.
    */
  private def runPaths(
    paths: Seq[Path],
    options: AxiDrawOptions,
    ebotActor: ActorRef[EiBotBoardActor.Command],
    context: ActorContext[Command]
  )(implicit ec: ExecutionContext): Unit = {
    val gapThreshold = EPS
    var previousLast: Option[Point] = None

    paths.foreach { path =>
      if (path.points.nonEmpty) {
        previousLast match {
          case None =>
            penUp(ebotActor, options)
            penDown(ebotActor, options)
          case Some(prev) =>
            if (prev.distance(path.points.head) > gapThreshold) {
              penUp(ebotActor, options)
              runPath(Path(Seq(prev, path.points.head)), options, ebotActor, context)
              penDown(ebotActor, options)
            }
        }
        runPath(path, options, ebotActor, context)
        previousLast = Some(path.points.last)
      }
    }
    penUp(ebotActor, options)
  }

  /**
    * Виконує план руху для одного шляху. Якщо довжина шляху більша за порог EPS,
    * конвертує шлях у план руху та виконує його.
    *
    * @param path       Шлях, що містить послідовність точок.
    * @param options    Конфігураційні параметри AxiDraw.
    * @param ebotActor  Актор для роботи з пристроєм (EiBotBoardActor).
    * @param context    Контекст актора.
    */
  private def runPath(
    path: com.scala.axidraw.Path,
    options: AxiDrawOptions,
    ebotActor: ActorRef[EiBotBoardActor.Command],
    context: ActorContext[Command]
  ): Unit = {
    val plan = Planner.plan(path, options.acceleration, options.maxVelocity, options.cornerFactor)
    runPlan(plan, options, ebotActor, context)
  }

  /**
    * Генерує команди StepperMove на основі часових проміжків з акумуляцією похибок округлення.
    *
    * Для кожного інтервалу часу [t, t+dt] обчислюється миттєве положення (plan.atTime),
    * визначається зміна положення, конвертується у кроки з акумуляцією дробової похибки,
    * після чого відправляється команда StepperMove.
    *
    * @param plan План руху
    * @param options Конфігураційні параметри AxiDraw
    * @param ebotActor Актор для роботи з пристроєм (EiBotBoardActor)
    * @param context Контекст актора
    */
  private def runPlan(
    plan: Plan,
    options: AxiDrawOptions,
    ebotActor: ActorRef[EiBotBoardActor.Command],
    context: ActorContext[Command]
  ): Unit = {
    // Часовий крок у секундах (timesliceMs задано в мілісекундах)
    val dtSeconds = options.timesliceMs / 1000.0
    val totalTime = plan.totalTime
    var previousInstant = plan.atTime(0.0)
    var t = dtSeconds

    // Накопичення дробових частин округлення для осей X та Y
    var errorX = 0.0
    var errorY = 0.0

    // Допоміжна функція, що розділяє число на дробову та цілу частини, подібно до math.modf у Python
    def modf(x: Double): (Double, Double) = {
      val floorVal = math.floor(x)
      (x - floorVal, floorVal)
    }

    while (t <= totalTime) {
      val currentInstant = plan.atTime(t)
      // Обчислення зміни положення за інтервал
      val dx = currentInstant.p.x - previousInstant.p.x
      val dy = currentInstant.p.y - previousInstant.p.y

      // Обчислення "сирих" кроків з урахуванням накопиченої похибки
      val rawX = dx * options.stepsPerInch + errorX
      val rawY = dy * options.stepsPerInch + errorY

      // Розділення на дробову та цілу частини
      val (fracX, intX) = modf(rawX)
      val (fracY, intY) = modf(rawY)
      errorX = fracX
      errorY = fracY

      // Конвертація у ціле число (кількість кроків)
      val sx = intX.toInt
      val sy = intY.toInt

      ebotActor ! EiBotBoardActor.StepperMove(options.timesliceMs, sx, sy)

      previousInstant = currentInstant
      t += dtSeconds
    }
  }

  /**
    * Розраховує параметри для команди підняття пера та надсилає її на пристрій.
    *
    * Обчислення ґрунтується на різниці між положеннями (penUpPosition і penDownPosition),
    * швидкості підйому пера та затримці, визначеній у конфігурації.
    *
    * @param ebotActor Актор, який надсилає команди до пристрою (EiBotBoardActor)
    * @param options   Конфігураційні параметри AxiDraw
    */
  private def penUp(ebotActor: ActorRef[EiBotBoardActor.Command], options: AxiDrawOptions): Unit = {
    val delta = math.abs(options.penUpPosition - options.penDownPosition)
    val duration = 1000 * delta / options.penUpSpeed
    val delay = math.max(0, duration + options.penUpDelay)
    ebotActor ! EiBotBoardActor.SetPenState(1, Some(delay))
  }

  /**
    * Розраховує параметри для команди опускання пера та надсилає її на пристрій.
    *
    * Обчислення ґрунтується на різниці між положеннями (penUpPosition і penDownPosition),
    * швидкості опускання пера та затримці, визначеній у конфігурації.
    *
    * @param ebotActor Актор, який надсилає команди до пристрою (EiBotBoardActor)
    * @param options   Конфігураційні параметри AxiDraw
    */
  private def penDown(ebotActor: ActorRef[EiBotBoardActor.Command], options: AxiDrawOptions): Unit = {
    val delta = math.abs(options.penUpPosition - options.penDownPosition)
    val duration = 1000 * delta / options.penDownSpeed
    val delay = math.max(0, duration + options.penDownDelay)
    ebotActor ! EiBotBoardActor.SetPenState(0, Some(delay))
  }

  /** Розрахунок параметрів сервоприводу */
  private def calculateServoSettings(options: AxiDrawOptions): (Int, Int, Int, Int) = {
    val slope = (ServoMax - ServoMin).toDouble / 100.0
    (
      (ServoMin + slope * options.penUpPosition).round.toInt,
      (ServoMin + slope * options.penDownPosition).round.toInt,
      options.penUpSpeed * SpeedScaling,
      options.penDownSpeed * SpeedScaling
    )
  }

  /** Відправка команд налаштування */
  private def sendConfigurationCommands(
    ebotActor: ActorRef[EiBotBoardActor.Command],
    upPos: Int,
    downPos: Int,
    upSpeed: Int,
    downSpeed: Int
  ): Unit = {
    val commands = List(
      EiBotBoardActor.ConfigureMode(4, upPos),
      EiBotBoardActor.ConfigureMode(5, downPos),
      EiBotBoardActor.ConfigureMode(11, upSpeed),
      EiBotBoardActor.ConfigureMode(12, downSpeed),
      EiBotBoardActor.EnableMotors(1, 1),
      EiBotBoardActor.SetPenState(1)
    )
    commands.foreach(ebotActor ! _)
  }

  /**
    * Завершує роботу пристрою, опускає перо та вимикає мотори.
    */
  private def shutdownDevice(
    ebotActor: ActorRef[EiBotBoardActor.Command],
    context: ActorContext[_]
  ): Unit = {
    context.log.info("Завершення роботи пристрою AxiDraw")
    val commands = List(
      EiBotBoardActor.EnableMotors(0, 0),
      EiBotBoardActor.SetPenState(0)
    )
    commands.foreach(ebotActor ! _)
  }
}

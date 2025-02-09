package com.scala.axidraw.actor

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}
import com.scala.axidraw.actor
import com.scala.axidraw.actor.EiBotBoardActor.ResponseHandler.ResponseHandler

import java.util.UUID
import scala.collection.mutable

/**
  * Головний актор для управління роботом EiBotBoard через послідовний інтерфейс.
  * Виконує трансляцію високорівневих команд у низькорівневі команди протоколу EBB
  * та обробляє відповіді від пристрою.
  */
object EiBotBoardActor {

  /**
    * Базовий тип для всіх команд, які підтримує актор.
    * Всі команди можуть реалізувати метод process для обробки відповіді.
    */
  sealed trait Command {

    val correlationId: String = UUID.randomUUID().toString

    /**
      * Метод для обробки відповіді від пристрою.
      *
      * @param context Контекст актора
      */
    def process(context: ActorContext[Command]): Unit = {}
  }

  case class AdapterCommand(handler: ResponseHandler, response: SerialActor.CommandResponse) extends Command

  /**
    * Базовий тип для всіх відповідей, що повертаються з актора.
    */
  sealed trait Response extends Command

  /**
    * Команда скидання внутрішнього стану пристрою.
    * Відновлює стан EBB до початкових налаштувань після включення.
    * Не викликає повного перезавантаження, лише реініціалізацію програмного стану.
    */
  final case class Reset() extends Command

  /**
    * Команда активації або деактивації крокових моторів.
    *
    * @param enable1 Стан першого мотора (1 – активовано, 0 – вимкнено)
    * @param enable2 Стан другого мотора (1 – активовано, 0 – вимкнено)
    */
  final case class EnableMotors(enable1: Int, enable2: Int) extends Command

  /**
    * Команда зміни стану пера з можливістю вказати затримку.
    *
    * @param duration Опціональна затримка у мілісекундах між командами (1–65535)
    */
  final case class TogglePen(duration: Option[Int] = None) extends Command

  /**
    * Команда явного встановлення стану пера.
    *
    * @param penValue Цільовий стан пера (0 – опущено, 1 – піднято)
    * @param duration Опціональна затримка виконання команди
    * @param portBpin Номер використовуваного піну порту B (0–7)
    */
  final case class SetPenState(
      penValue: Int,
      duration: Option[Int] = None,
      portBpin: Option[Int] = None
  ) extends Command

  /**
    * Команда миттєвого перезавантаження пристрою.
    * Викликає повне перезавантаження EBB, аналогічне циклу живлення.
    * (Версії прошивки: v2.5.4 та новіші)
    */
  final case class ReBoot() extends Command

  /**
    * Запит поточного стану пера.
    *
    * @param replyTo Адресат, на який буде відправлено результат запиту.
    *                Повинна приймати як успішну відповідь, так і повідомлення про помилку.
    */
  final case class QueryPen(replyTo: ActorRef[Response]) extends Command

  /**
    * Результат запиту стану пера.
    *
    * @param isUp true, якщо перо підняте, false, якщо опущене
    */
  final case class PenStatus(isUp: Boolean) extends Response

  /**
    * Команда руху до Home або абсолютного переміщення.
    *
    * Команда: HM,StepFrequency[,Position1,Position2]<CR>
    *
    * @param stepFrequency Частота кроків, в діапазоні від 2 до 25000 (steps/s)
    * @param position1 Опціональна абсолютна позиція для мотора 1 (від -4294967 до 4294967).
    *                  Якщо не вказано, рух здійснюється до Home (0,0).
    * @param position2 Опціональна абсолютна позиція для мотора 2 (від -4294967 до 4294967).
    *                  Якщо не вказано, рух здійснюється до Home (0,0).
    */
  final case class MoveHome(stepFrequency: Int, position1: Option[Int] = None, position2: Option[Int] = None)
      extends Command

  /**
    * Команда конфігурації режимів двигунів та сервоприводів.
    *
    * Формат: SC,value1,value2<CR>
    * value1 — ціле число від 0 до 255, яке вказує параметр, що налаштовується.
    * value2 — ціле число від 0 до 65535, яке задає значення параметра.
    *
    * Приклади:
    *   SC,4,8000<CR> – встановлює мінімальне положення для підйому пера (servo_min).
    *   SC,1,1<CR>   – увімкнення тільки RC servo для підйому пера.
    *
    * Firmware: v1.0 і новіші.
    */
  final case class ConfigureMode(value1: Int, value2: Int) extends Command

  /**
    * Команда XM — Кроковий рух для геометрії з мішаними осями.
    *
    * Формат: XM,duration,AxisStepsA,AxisStepsB<CR>
    *
    * @param duration   Тривалість руху в мілісекундах (від 1 до 16777215)
    * @param axisStepsA Кількість кроків для осі A (від -16777215 до 16777215)
    * @param axisStepsB Кількість кроків для осі B (від -16777215 до 16777215)
    */
  final case class StepperMove(duration: Int, axisStepsA: Int, axisStepsB: Int) extends Command

  /**
    * Відповідь про помилку виконання команди для використання в ResponseHandler.
    *
    * @param reason Текст помилки
    */
  final case class CommandFailure(reason: String) extends Response

  /**
    * Відповідь про таймаут виконання команди для використання в ResponseHandler.
    *
    * @param partial Частково отримані дані
    */
  final case class CommandTimeout(partial: String) extends Response

  /**
    * Трейт для обробників команд, що визначає базову поведінку.
    */
  private trait CommandHandler {

    /**
      * Обробляє конкретну команду та повертає поведінку.
      *
      * @param msg Повідомлення, яке потрібно обробити
      * @param serialActor Референс на SerialActor для роботи з послідовним портом
      * @param context Контекст актора для логування та створення адаптерів
      * @return Поведінка актора після обробки повідомлення
      */
    def apply(
        msg: Command,
        serialActor: ActorRef[SerialActor.Command],
        context: ActorContext[Command]
    ): Behavior[Command]
  }

  /**
    * Обробник команди StepperMove.
    *
    * Виконує валідацію параметрів, обчислює значення для осей 1 і 2, перевіряє швидкість та відправляє команду.
    */
  private object StepperMoveHandler extends CommandHandler {
    def apply(
        msg: Command,
        serialActor: ActorRef[SerialActor.Command],
        context: ActorContext[Command]
    ): Behavior[Command] = msg match {
      case cmd @ StepperMove(duration, axisStepsA, axisStepsB) =>
        // Валідація вхідних параметрів за допомогою функціонального підходу
        val validationResult = for {
          _ <- Either.cond(duration >= 1 && duration <= 16777215,
                           (),
                           s"Невірне значення duration: $duration. Має бути від 1 до 16777215.")
          _ <- Either.cond(axisStepsA >= -16777215 && axisStepsA <= 16777215,
                           (),
                           s"Невірне значення AxisStepsA: $axisStepsA. Має бути від -16777215 до 16777215.")
          _ <- Either.cond(axisStepsB >= -16777215 && axisStepsB <= 16777215,
                           (),
                           s"Невірне значення AxisStepsB: $axisStepsB. Має бути від -16777215 до 16777215.")
        } yield ()

        validationResult.fold(
          err => {
            context.log.error(err)
            Behaviors.same
          },
          _ =>
            if (axisStepsA == 0 && axisStepsB == 0) {
              context.log.debug(s"Виконується затримка $duration мс (без руху).")
              Behaviors.same
            } else {
              // Обчислення значень для осі 1 та осі 2
              val axisSteps1 = axisStepsA + axisStepsB
              val axisSteps2 = axisStepsA - axisStepsB

              // Перевірка швидкості для осі 1 (якщо рух є)
              val speedCheck1 = if (axisSteps1 != 0) {
                val speed1 = (math.abs(axisSteps1) * 1000.0) / duration
                Either.cond(speed1 >= 1.31 && speed1 <= 25000,
                            (),
                            s"Швидкість осі 1 ($speed1 кроків/с) поза межами [1.31, 25000].")
              } else Right(())

              // Перевірка швидкості для осі 2 (якщо рух є)
              val speedCheck2 = if (axisSteps2 != 0) {
                val speed2 = (math.abs(axisSteps2) * 1000.0) / duration
                Either.cond(speed2 >= 1.31 && speed2 <= 25000,
                            (),
                            s"Швидкість осі 2 ($speed2 кроків/с) поза межами [1.31, 25000].")
              } else Right(())

              (speedCheck1, speedCheck2) match {
                case (Right(_), Right(_)) =>
                  // Формування команди
                  val command = s"XM,$duration,$axisStepsA,$axisStepsB"
                  context.log.info(s"Відправка команди StepperMove: $command")
                  val adapter = ResponseHandler.createAdapter(
                    ResponseHandler.StepperMoveResponseHandler(cmd.correlationId))(context)
                  serialActor ! SerialActor.QueryCommand(command, 1, adapter, cmd.correlationId)
                  Behaviors.same
                case (Left(err), _) =>
                  context.log.error(err)
                  Behaviors.same
                case (_, Left(err)) =>
                  context.log.error(err)
                  Behaviors.same
              }
          }
        )
      case _ => Behaviors.unhandled
    }
  }

  /**
    * Обробник команди скидання стану пристрою.
    */
  private object ResetHandler extends CommandHandler {
    def apply(
        msg: Command,
        serialActor: ActorRef[SerialActor.Command],
        context: ActorContext[Command]
    ): Behavior[Command] = msg match {
      case cmd @ Reset() =>
        context.log.info("Відправка команди скидання стану")
        val adapter = ResponseHandler.createAdapter(ResponseHandler.ResetResponseHandler(cmd.correlationId))(context)
        serialActor ! SerialActor.QueryCommand("R", 1, adapter, cmd.correlationId)
        Behaviors.same
      case _ => Behaviors.unhandled
    }
  }

  /**
    * Обробник команди активації моторів.
    */
  private object EnableMotorsHandler extends CommandHandler {
    def apply(
        msg: Command,
        serialActor: ActorRef[SerialActor.Command],
        context: ActorContext[Command]
    ): Behavior[Command] = msg match {
      case cmd @ EnableMotors(e1, e2) =>
        context.log.info(s"Генерація команди EM: $e1,$e2")
        val adapter =
          ResponseHandler.createAdapter(ResponseHandler.EnableMotorsResponseHandler(cmd.correlationId))(context)
        serialActor ! SerialActor.QueryCommand(s"EM,$e1,$e2", 1, adapter, cmd.correlationId)
        Behaviors.same
      case _ => Behaviors.unhandled
    }
  }

  /**
    * Обробник команди перемикання стану пера.
    */
  private object TogglePenHandler extends CommandHandler {
    def apply(
        msg: Command,
        serialActor: ActorRef[SerialActor.Command],
        context: ActorContext[Command]
    ): Behavior[Command] = msg match {
      case cmd @ TogglePen(duration) =>
        val validDuration = duration.filter(d => d >= 1 && d <= 65535)
        val command       = validDuration.fold("TP")(d => s"TP,$d")
        context.log.info(s"Відправка команди перемикання пера: $command")
        val adapter =
          ResponseHandler.createAdapter(ResponseHandler.TogglePenResponseHandler(cmd.correlationId))(context)
        serialActor ! SerialActor.QueryCommand(command, 1, adapter, cmd.correlationId)
        Behaviors.same
      case _ => Behaviors.unhandled
    }
  }

  /**
    * Обробник команди встановлення стану пера.
    */
  private object SetPenStateHandler extends CommandHandler {
    def apply(
        msg: Command,
        serialActor: ActorRef[SerialActor.Command],
        context: ActorContext[Command]
    ): Behavior[Command] = msg match {
      case cmd @ SetPenState(value, dur, pin) =>
        val penState = if (value == 1) 1 else 0
        val validDur = dur.filter(d => d >= 1 && d <= 65535)
        val validPin = pin.filter(p => p >= 0 && p <= 7)
        val command = (validDur, validPin) match {
          case (None, None)       => s"SP,$penState"
          case (Some(d), None)    => s"SP,$penState,$d"
          case (None, Some(p))    => s"SP,$penState,0,$p"
          case (Some(d), Some(p)) => s"SP,$penState,$d,$p"
        }
        context.log.info(s"Формування команди стану пера: $command")
        val adapter =
          ResponseHandler.createAdapter(ResponseHandler.SetPenStateResponseHandler(cmd.correlationId))(context)
        serialActor ! SerialActor.QueryCommand(command, 1, adapter, cmd.correlationId)
        Behaviors.same
      case _ => Behaviors.unhandled
    }
  }

  /**
    * Обробник запиту стану пера.
    */
  private object QueryPenHandler extends CommandHandler {
    def apply(
        msg: Command,
        serialActor: ActorRef[SerialActor.Command],
        context: ActorContext[Command]
    ): Behavior[Command] = msg match {
      case cmd @ QueryPen(replyTo) =>
        context.log.info("Ініціювання запиту стану пера QP")
        val adapter =
          ResponseHandler.createAdapter(ResponseHandler.PenQueryResponseHandler(cmd.correlationId, replyTo))(context)
        serialActor ! SerialActor.QueryCommand("QP", 1, adapter, cmd.correlationId)
        Behaviors.same
      case _ => Behaviors.unhandled
    }
  }

  /**
    * Обробник команди перезавантаження пристрою.
    */
  private object ReBootHandler extends CommandHandler {
    def apply(
        msg: Command,
        serialActor: ActorRef[SerialActor.Command],
        context: ActorContext[Command]
    ): Behavior[Command] = msg match {
      case cmd @ ReBoot() =>
        context.log.info("Ініціювання повного перезавантаження пристрою")
        val adapter = ResponseHandler.createAdapter(ResponseHandler.ReBootResponseHandler(cmd.correlationId))(context)
        serialActor ! SerialActor.QueryCommand("RB", 1, adapter, cmd.correlationId)
        Behaviors.same
      case _ => Behaviors.unhandled
    }
  }

  /**
    * Обробник команди руху до Home або абсолютного переміщення.
    */
  private object MoveHomeHandler extends CommandHandler {
    def apply(
        msg: Command,
        serialActor: ActorRef[SerialActor.Command],
        context: ActorContext[Command]
    ): Behavior[Command] = msg match {
      case cmd @ MoveHome(stepFreq, pos1, pos2) =>
        // Перевірка діапазону для stepFrequency
        if (stepFreq < 2 || stepFreq > 25000) {
          context.log.error(s"Невірне значення StepFrequency: $stepFreq. Допустимий діапазон: 2 - 25000.")
          Behaviors.same
        } else {
          // Якщо одна з позицій визначена, обидві мають бути визначені
          if (pos1.isDefined ^ pos2.isDefined) {
            context.log.error("Потрібно вказати обидві позиції Position1 та Position2 або жодної.")
            Behaviors.same
          } else {
            // Якщо позиції визначені, перевіряємо їх діапазон (від -4294967 до 4294967)
            val positionsValid = pos1.forall(p => math.abs(p) <= 4294967) &&
              pos2.forall(p => math.abs(p) <= 4294967)
            if (!positionsValid) {
              context.log.error("Значення позицій повинні бути в діапазоні від -4294967 до 4294967.")
              Behaviors.same
            } else {
              // Формуємо команду: якщо позиції не вказані, рух до Home (0,0)
              val command = (pos1, pos2) match {
                case (None, None)         => s"HM,$stepFreq"
                case (Some(p1), Some(p2)) => s"HM,$stepFreq,$p1,$p2"
                case _                    => "" // Ніколи не досягнеться через вище перевірку
              }
              context.log.info(s"Відправка команди Home/Absolute Move: $command")
              val adapter =
                ResponseHandler.createAdapter(ResponseHandler.MoveHomeResponseHandler(cmd.correlationId))(context)
              serialActor ! actor.SerialActor.QueryCommand(command, 1, adapter, cmd.correlationId)
              Behaviors.same
            }
          }
        }
      case _ => Behaviors.unhandled
    }
  }

  /**
    * Обробник команди конфігурації режимів двигунів та сервоприводів.
    */
  private object ConfigureModeHandler extends CommandHandler {
    def apply(
        msg: Command,
        serialActor: ActorRef[SerialActor.Command],
        context: ActorContext[Command]
    ): Behavior[Command] = msg match {
      case cmd @ ConfigureMode(v1, v2) =>
        val command = s"SC,$v1,$v2"
        context.log.info(s"Відправка команди конфігурації: $command")
        val adapter =
          ResponseHandler.createAdapter(ResponseHandler.ConfigureModeResponseHandler(cmd.correlationId))(context)
        serialActor ! actor.SerialActor.QueryCommand(command, 1, adapter, cmd.correlationId)
        Behaviors.same
      case _ => Behaviors.unhandled
    }
  }

  /**
    * Об'єкт для обробки відповідей від пристрою.
    */
  object ResponseHandler {

    /**
      * Трейт для обробників відповідей.
      */
    sealed trait ResponseHandler {

      val correlationId: String

      /**
        * Обробляє відповідь від пристрою.
        *
        * @param response Отримана відповідь від послідовного порту
        * @param context Контекст актора для логування та обробки
        */
      def handle(response: SerialActor.CommandResponse, context: ActorContext[Command]): Unit
    }

    /**
      * Генерікований обробник для базових операцій.
      *
      * @param commandName Назва команди для логування
      * @param successHandler Функція, що обробляє успішний результат
      * @param replyTo Опціональний адресат для відправки повідомлень про помилку або таймаут
      */
    private case class GenericHandler(
        correlationId: String,
        commandName: String,
        successHandler: String => Unit,
        replyTo: Option[ActorRef[Response]] = None
    ) extends ResponseHandler {
      def handle(response: SerialActor.CommandResponse, context: ActorContext[Command]): Unit = {
        response match {
          case SerialActor.CommandSuccess(_, _, data) if data.startsWith("OK") =>
            context.log.info(s"$commandName: успішно виконано")
            successHandler(data.stripPrefix("OK").trim)
          case SerialActor.CommandSuccess(_, _, data) =>
            context.log.info(s"$commandName: отримано дані - $data")
            successHandler(data)
          case SerialActor.CommandFailure(_, _, reason) =>
            context.log.error(s"$commandName: помилка - $reason")
            replyTo.foreach(_ ! CommandFailure(reason))
          case SerialActor.CommandTimeout(_, _, partial) =>
            context.log.warn(s"$commandName: таймаут, часткова відповідь - $partial")
            replyTo.foreach(_ ! CommandTimeout(partial))
        }
      }
    }

    /**
      * Обробник відповіді для команди StepperMove.
      *
      * @param correlationId Ідентифікатор запиту
      */
    private[EiBotBoardActor] case class StepperMoveResponseHandler(correlationId: String) extends ResponseHandler {
      def handle(response: SerialActor.CommandResponse, context: ActorContext[Command]): Unit =
        GenericHandler(correlationId,
                       "StepperMove",
                       data => context.log.info(s"StepperMove виконана успішно з даними: $data"))
          .handle(response, context)
    }

    /**
      * Обробник відповіді для команди Reset.
      */
    private[EiBotBoardActor] case class ResetResponseHandler(correlationId: String) extends ResponseHandler {
      def handle(response: SerialActor.CommandResponse, context: ActorContext[Command]): Unit =
        GenericHandler(correlationId, "Reset", _ => context.log.info("Скидання стану виконано"))
          .handle(response, context)
    }

    /**
      * Обробник відповіді для команди EnableMotors.
      */
    private[EiBotBoardActor] case class EnableMotorsResponseHandler(correlationId: String) extends ResponseHandler {
      def handle(response: SerialActor.CommandResponse, context: ActorContext[Command]): Unit =
        GenericHandler(correlationId,
                       "EnableMotors",
                       data => context.log.info(s"EnableMotors виконано успішно з даними: $data"))
          .handle(response, context)
    }

    /**
      * Обробник відповіді для команди TogglePen.
      */
    private[EiBotBoardActor] case class TogglePenResponseHandler(correlationId: String) extends ResponseHandler {
      def handle(response: SerialActor.CommandResponse, context: ActorContext[Command]): Unit =
        GenericHandler(correlationId,
                       "TogglePen",
                       data => context.log.info(s"TogglePen виконано успішно з даними: $data"))
          .handle(response, context)
    }

    /**
      * Обробник відповіді для команди SetPenState.
      */
    private[EiBotBoardActor] case class SetPenStateResponseHandler(correlationId: String) extends ResponseHandler {
      def handle(response: SerialActor.CommandResponse, context: ActorContext[Command]): Unit =
        GenericHandler(correlationId,
                       "SetPenState",
                       data => context.log.info(s"SetPenState виконано успішно з даними: $data"))
          .handle(response, context)
    }

    /**
      * Обробник відповіді для команди ReBoot.
      */
    private[EiBotBoardActor] case class ReBootResponseHandler(correlationId: String) extends ResponseHandler {
      def handle(response: SerialActor.CommandResponse, context: ActorContext[Command]): Unit =
        GenericHandler(correlationId, "ReBoot", data => context.log.info(s"ReBoot виконано успішно з даними: $data"))
          .handle(response, context)
    }

    /**
      * Обробник відповіді для запиту стану пера (QueryPen).
      *
      * @param replyTo Адресат, на який буде відправлено результат запиту
      */
    private[EiBotBoardActor] case class PenQueryResponseHandler(correlationId: String, replyTo: ActorRef[PenStatus])
        extends ResponseHandler {
      def handle(response: SerialActor.CommandResponse, context: ActorContext[Command]): Unit =
        GenericHandler(
          correlationId,
          "PenStatus",
          data => {
            val isUp = data match {
              case "1" => true
              case "0" => false
              case other =>
                context.log.warn(s"Невідомий статус пера: $other")
                false
            }
            replyTo ! PenStatus(isUp)
          }
        ).handle(response, context)
    }

    /**
      * Обробник відповіді для команди MoveHome.
      */
    private[EiBotBoardActor] case class MoveHomeResponseHandler(correlationId: String) extends ResponseHandler {
      def handle(response: SerialActor.CommandResponse, context: ActorContext[Command]): Unit =
        GenericHandler(correlationId,
                       "MoveHome",
                       data => context.log.info(s"MoveHome виконано успішно з даними: $data"))
          .handle(response, context)
    }

    /**
      * Обробник відповіді для команди ConfigureMode.
      */
    private[EiBotBoardActor] case class ConfigureModeResponseHandler(correlationId: String) extends ResponseHandler {
      def handle(response: SerialActor.CommandResponse, context: ActorContext[Command]): Unit =
        GenericHandler(correlationId,
                       "ConfigureMode",
                       data => context.log.info(s"ConfigureMode виконано успішно з даними: $data"))
          .handle(response, context)
    }

    private val pendingHandlers = mutable.Map.empty[String, ResponseHandler]

    /**
      * Фабричний метод для створення адаптера відповідей.
      *
      * @param handler Обробник відповіді, який буде викликаний при отриманні відповіді
      * @param context Контекст актора для створення адаптера
      * @return Адаптер для перетворення відповіді від послідовного порту у команду актора
      */
    def createAdapter(handler: ResponseHandler)(
        implicit context: ActorContext[Command]): ActorRef[SerialActor.CommandResponse] = {
      val id = handler.correlationId
      pendingHandlers += (id -> handler)
      context.messageAdapter { response: SerialActor.CommandResponse =>
        new AdapterCommand(handler, response) {
          override def process(ctx: ActorContext[Command]): Unit = {
            pendingHandlers.remove(response.correlationId).foreach(_.handle(response, context))
          }
        }
      }
    }

  }

  /**
    * Фабричний метод для створення головної поведінки актора.
    *
    * @param serialActor Референс на актор для роботи з послідовним портом
    * @return Комбінована поведінка актора для обробки всіх типів команд
    */
  def apply(serialActor: ActorRef[SerialActor.Command]): Behavior[Command] =
    Behaviors.setup { context =>
      Behaviors.receiveMessage[Command] {
        case cmd: EnableMotors  => EnableMotorsHandler(cmd, serialActor, context)
        case cmd: TogglePen     => TogglePenHandler(cmd, serialActor, context)
        case cmd: SetPenState   => SetPenStateHandler(cmd, serialActor, context)
        case cmd: QueryPen      => QueryPenHandler(cmd, serialActor, context)
        case cmd: Reset         => ResetHandler(cmd, serialActor, context)
        case cmd: ReBoot        => ReBootHandler(cmd, serialActor, context)
        case cmd: MoveHome      => MoveHomeHandler(cmd, serialActor, context)
        case cmd: ConfigureMode => ConfigureModeHandler(cmd, serialActor, context)
        case cmd: StepperMove   => StepperMoveHandler(cmd, serialActor, context)
        case cmd: AdapterCommand =>
          cmd.process(context)
          Behaviors.same
      }
    }
}

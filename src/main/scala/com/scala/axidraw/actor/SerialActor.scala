package com.scala.axidraw.actor

import akka.actor.typed.scaladsl.{Behaviors, StashBuffer, TimerScheduler}
import akka.actor.typed.{ActorRef, Behavior}
import com.fazecast.jSerialComm.{SerialPort, SerialPortDataListener, SerialPortEvent}

import java.io.IOException
import java.nio.charset.StandardCharsets
import java.util.UUID
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

/**
  * Актор для роботи з послідовним портом (EiBotBoard).
  *
  * Підтримує команди запису, запиту та очищення буферів.
  *
  * Оскільки пристрій відповідає лише на один запит за раз, актор реалізовано як
  * «станова машина» з двома основними станами:
  *   - Idle: немає pending‑команди, всі запити відправляються негайно.
  *   - Waiting: запит уже відправлено; інші запити складаються (через StashBuffer).
  */
object SerialActor {

  /**
    * Команди, що надходять до SerialActor.
    */
  sealed trait Command

  /**
    * Команда запису даних у послідовний порт без очікування відповіді.
    *
    * @param data Рядок даних для запису
    */
  final case class WriteCommand(data: String) extends Command

  /**
    * Команда запиту, що очікує відповідь.
    *
    * @param data Рядок запиту
    * @param expectedLines Кількість рядків, яку очікуємо отримати у відповіді
    * @param replyTo Адресат, на який буде відправлено відповідь
    */
  final case class QueryCommand(
      data: String,
      expectedLines: Int,
      replyTo: ActorRef[CommandResponse],
      correlationId: String
  ) extends Command

  /**
    * Команда очищення буферів вводу/виводу послідовного порту.
    */
  case class Flush() extends Command

  /**
    * Відповіді, що повертаються SerialActor.
    */
  sealed trait CommandResponse {
    val correlationId: String
  }
  final case class CommandSuccess(correlationId: String, command: String, response: String)        extends CommandResponse
  final case class CommandFailure(correlationId: String, command: String, reason: String)          extends CommandResponse
  final case class CommandTimeout(correlationId: String, command: String, partialResponse: String) extends CommandResponse

  /**
    * Внутрішні команди, що використовуються для управління таймерами та отримання даних.
    */
  private sealed trait InternalCommand extends Command

  /**
    * Сигнал про отримання даних з порту.
    *
    * @param data Отримані дані
    * @param isTimeout Чи викликано цей сигнал через таймаут
    */
  private final case class DataReceived(data: String, isTimeout: Boolean = false) extends InternalCommand

  /**
    * Представляє pending запит, для якого чекаємо відповіді.
    *
    * @param originalCommand Оригінальна команда, яку надіслав користувач
    * @param normalizedCommand Нормалізована (валідована) команда (з CR)
    * @param expectedLines Очікувана кількість рядків відповіді
    * @param replyTo Адресат, на який буде відправлено відповідь
    * @param responseBuffer Накопичені дані відповіді (залишок неповної останньої лінії)
    * @param timerKey Унікальний ключ таймера для цього запиту
    */
  private final case class PendingCommand(
      originalCommand: String,
      normalizedCommand: String,
      expectedLines: Int,
      replyTo: ActorRef[CommandResponse],
      responseBuffer: String,
      timerKey: String,
      correlationId: String
  )

  private val MaxCommandLength: Int                   = 64
  private val ResponseTimeoutDuration: FiniteDuration = 500.millis

  /**
    * Фабричний метод для створення поведінки SerialActor.
    *
    * Виконується налаштування послідовного порту, а потім переходить у стан idle.
    *
    * @return Поведінка актора, що обробляє команди.
    */
  def apply(): Behavior[Command] =
    Behaviors.withStash[Command](capacity = 100) { stashBuffer =>
      Behaviors.setup { context =>
        Behaviors.withTimers[Command] { timers =>
          context.log.info("Ініціалізація SerialActor...")
          findSerialPort() match {
            case Some(port) =>
              configurePort(port, context.self, context) match {
                case Success(_) =>
                  context.log.info(s"Порт ${port.getSystemPortName} готовий до роботи")
                  idle(port, timers, stashBuffer)
                case Failure(ex) =>
                  context.log.error("Помилка конфігурації порту", ex)
                  Behaviors.stopped
              }
            case None =>
              context.log.error("Не знайдено сумісних COM-портів")
              Behaviors.stopped
          }
        }
      }
    }

  /**
    * Стан idle: немає pending запиту, всі нові команди обробляються негайно.
    *
    * @param port Відкритий послідовний порт.
    * @param timers Планувальник таймерів.
    * @param stashBuffer Буфер для збереження команд, що надходять під час pending.
    * @return Поведінка актора в стані idle.
    */
  private def idle(
      port: SerialPort,
      timers: TimerScheduler[Command],
      stashBuffer: StashBuffer[Command]
  ): Behavior[Command] =
    Behaviors.receive { (context, msg) =>
      msg match {
        case WriteCommand(data) =>
          context.log.debug(s"Обробка WriteCommand: $data")
          handleWriteCommand(data, port, context)
          Behaviors.same

        case qc: QueryCommand =>
          context.log.debug(s"Обробка QueryCommand: ${qc.data}")
          handleQueryCommand(qc.data, qc.expectedLines, qc.replyTo, port, timers, context, qc.correlationId) match {
            case Right(pending) =>
              context.log.debug(s"Створено pending команду для: ${qc.data}")
              waiting(port, timers, pending, stashBuffer)
            case Left(error) =>
              qc.replyTo ! CommandFailure(qc.correlationId, qc.data, error)
              Behaviors.same
          }

        case Flush() =>
          context.log.debug("Обробка Flush")
          Try(port.flushIOBuffers()).fold(
            ex => context.log.error("Помилка очищення буферів", ex),
            _ => context.log.info("Буфери успішно скинуті")
          )
          Behaviors.same

        case DataReceived(data, _) =>
          context.log.debug(s"Отримано дані, але немає pending: $data")
          Behaviors.same
      }
    }

  /**
    * Стан waiting: запит вже відправлено, і ми чекаємо на відповідь.
    * Нові запити додаються до StashBuffer.
    *
    * @param port Відкритий послідовний порт.
    * @param timers Планувальник таймерів.
    * @param pending Pending запит, для якого чекаємо відповіді.
    * @param stashBuffer Буфер для збереження додаткових команд.
    * @return Поведінка актора в стані waiting.
    */
  private def waiting(
      port: SerialPort,
      timers: TimerScheduler[Command],
      pending: PendingCommand,
      stashBuffer: StashBuffer[Command]
  ): Behavior[Command] =
    Behaviors.receive { (context, msg) =>
      msg match {
        case qc: QueryCommand =>
          context.log.debug(s"Команда '${qc.data}' додана до черги")
          stashBuffer.stash(qc)
          Behaviors.same

        case WriteCommand(data) =>
          context.log.debug(s"Команда '$data' додана до черги")
          stashBuffer.stash(WriteCommand(data))
          Behaviors.same

        case Flush() =>
          context.log.debug("Обробка Flush")
          Try(port.flushIOBuffers()).fold(
            ex => context.log.error("Помилка очищення буферів", ex),
            _ => context.log.info("Буфери успішно скинуті")
          )
          Behaviors.same

        case DataReceived(data, isTimeout) =>
          context.log.debug(s"Отримано дані: '$data', isTimeout=$isTimeout")
          val newBuffer = pending.responseBuffer + data
          val parts     = newBuffer.split("\r", -1).toVector
          context.log.debug(s"Буфер розбитий на частини: $parts")
          val completeLines = parts.init
          val partialLine   = parts.last
          context.log.debug(s"Повних рядків: ${completeLines.size}, очікується: ${pending.expectedLines}")
          if (completeLines.size >= pending.expectedLines) {
            val response = completeLines.take(pending.expectedLines).mkString("\r")
            context.log.info(s"Отримано повну відповідь для '${pending.originalCommand}': '$response'")
            pending.replyTo ! CommandSuccess(pending.correlationId, pending.originalCommand, response)
            timers.cancel(pending.timerKey)
            unstashAll(port, timers, stashBuffer)
          } else if (isTimeout) {
            context.log.warn(s"Таймаут для '${pending.originalCommand}', накопичено: '$newBuffer'")
            pending.replyTo ! CommandTimeout(pending.correlationId, pending.originalCommand, newBuffer)
            timers.cancel(pending.timerKey)
            unstashAll(port, timers, stashBuffer)
          } else {
            context.log.debug(
              s"Недостатньо повних рядків, продовжуємо очікування. Неповний рядок: '$partialLine'")
            waiting(port, timers, pending.copy(responseBuffer = partialLine), stashBuffer)
          }

        case other =>
          context.log.warn(s"Невідоме повідомлення: $other")
          Behaviors.same
      }
    }

  /**
    * Unstashes усі відкладені команди та повертає поведінку у стан idle.
    *
    * @param port Відкритий послідовний порт.
    * @param timers Планувальник таймерів.
    * @param stashBuffer Буфер відкладених команд.
    * @return Поведінка у стані idle.
    */
  private def unstashAll(
      port: SerialPort,
      timers: TimerScheduler[Command],
      stashBuffer: StashBuffer[Command]
  ): Behavior[Command] =
    stashBuffer.unstashAll(idle(port, timers, stashBuffer))

  /**
    * Валідовує та нормалізує вхідну команду.
    *
    * @param data Вхідний рядок команди.
    * @return Either повідомлення про помилку або нормалізований рядок (закінчується CR).
    */
  private def validateCommand(data: String): Either[String, String] = {
    val command   = data.toUpperCase
    val cmdWithCR = if (command.endsWith("\r")) command else s"$command\r"
    if (cmdWithCR.length > MaxCommandLength)
      Left(s"Перевищено максимальну довжину команди ($MaxCommandLength символів)")
    else if (!cmdWithCR.matches("^[!-~]+\r$"))
      Left("Команда повинна складатися лише з ASCII символів без пробілів")
    else Right(cmdWithCR)
  }

  /**
    * Записує дані у послідовний порт.
    *
    * @param command Нормалізована команда.
    * @param port Відкритий послідовний порт.
    * @return Try, що вказує на успіх або помилку запису.
    */
  private def writeToPort(command: String, port: SerialPort): Try[Unit] = Try {
    val bytes   = command.getBytes(StandardCharsets.US_ASCII)
    val written = port.writeBytes(bytes, bytes.length)
    if (written != bytes.length)
      throw new IOException(s"Записано лише $written з ${bytes.length} байт")
  }

  /**
    * Обробляє WriteCommand: перевіряє команду, записує дані в порт та логірує результат.
    *
    * @param data Рядок команди.
    * @param port Відкритий послідовний порт.
    * @param context Контекст актора.
    */
  private def handleWriteCommand(
      data: String,
      port: SerialPort,
      context: akka.actor.typed.scaladsl.ActorContext[Command]
  ): Unit =
    validateCommand(data).fold(
      error => context.log.error(s"Невірний формат команди: $error"),
      validCmd =>
        writeToPort(validCmd, port).fold(
          ex => context.log.error("Помилка фізичного запису", ex),
          _ => context.log.debug(s"Успішно відправлено: ${validCmd.trim}")
      )
    )

  /**
    * Обробляє QueryCommand: перевіряє команду, записує її в порт, створює таймер та повертає pending запит.
    *
    * @param data Рядок запиту.
    * @param expectedLines Очікувана кількість рядків відповіді.
    * @param replyTo Адресат відповіді.
    * @param port Відкритий послідовний порт.
    * @param timers Планувальник таймерів.
    * @param context Контекст актора.
    * @return Either повідомлення про помилку або PendingCommand.
    */
  private def handleQueryCommand(
      data: String,
      expectedLines: Int,
      replyTo: ActorRef[CommandResponse],
      port: SerialPort,
      timers: TimerScheduler[Command],
      context: akka.actor.typed.scaladsl.ActorContext[Command],
      correlationId: String
  ): Either[String, PendingCommand] =
    validateCommand(data).fold(
      error => Left(error),
      validCmd =>
        writeToPort(validCmd, port) match {
          case Failure(ex) => Left(s"Помилка запису: ${ex.getMessage}")
          case Success(_) =>
            val timerKey = UUID.randomUUID().toString
            timers.startSingleTimer(timerKey, DataReceived("", isTimeout = true), ResponseTimeoutDuration)
            context.log.debug(s"Створено pending команду для '$data' з таймером $timerKey")
            Right(PendingCommand(data, validCmd, expectedLines, replyTo, "", timerKey, correlationId))
      }
    )

  /**
    * Шукає сумісний послідовний порт для EiBotBoard.
    *
    * @return Option з портом, якщо знайдено.
    */
  private def findSerialPort(): Option[SerialPort] =
    SerialPort.getCommPorts.find { port =>
      port.getDescriptivePortName.startsWith("EiBotBoard") ||
      port.getDescriptivePortName.contains("USB VID:PID=04D8:FD92")
    }

  /**
    * Налаштовує параметри послідовного порту та реєструє слухача даних.
    *
    * @param port Послідовний порт для налаштування.
    * @param self Посилання на цей актор.
    * @param context Контекст актора для логування.
    * @return Try, що вказує на успіх або помилку.
    */
  private def configurePort(
      port: SerialPort,
      self: ActorRef[Command],
      context: akka.actor.typed.scaladsl.ActorContext[Command]
  ): Try[Unit] = Try {
    port.setBaudRate(9600)
    port.setNumDataBits(8)
    port.setParity(SerialPort.NO_PARITY)
    port.setNumStopBits(SerialPort.ONE_STOP_BIT)
    if (!port.openPort()) throw new IOException("Неможливо відкрити порт")
    port.addDataListener(new SerialPortDataListener {
      override def getListeningEvents: Int = SerialPort.LISTENING_EVENT_DATA_AVAILABLE
      override def serialEvent(event: SerialPortEvent): Unit = {
        try {
          if (event.getEventType == SerialPort.LISTENING_EVENT_DATA_AVAILABLE) {
            val available = port.bytesAvailable()
            if (available > 0) {
              val buffer = new Array[Byte](available)
              val read   = port.readBytes(buffer, buffer.length)
              if (read > 0) {
                val data = new String(buffer, 0, read, StandardCharsets.US_ASCII)
                  .replace("\r\n", "\r")
                self ! DataReceived(data)
              }
            }
          }
        } catch {
          case ex: Exception =>
            context.log.error("Помилка під час читання даних", ex)
        }
      }
    })
  }
}

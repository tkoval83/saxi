package com.scala.axidraw

import akka.actor.typed.ActorSystem
import com.scala.axidraw.actor.AxiDrawActor
import com.typesafe.config.{Config, ConfigFactory}
import org.slf4j.LoggerFactory
import scopt.{OParser, OParserBuilder}

import scala.concurrent.Await
import scala.concurrent.duration._

/**
  * Об’єкт для розбору аргументів командного рядка.
  */
object CommandLineOptions {

  /** Параметри, що задаються через командний рядок. */
  case class Options(
    configFile: Option[String] = None
  )

  val builder: OParserBuilder[Options] = OParser.builder[Options]
  val parser: OParser[Unit, Options] = {
    import builder._
    OParser.sequence(
      programName("AxiDraw"),
      head("AxiDraw", "1.0"),
      opt[String]('c', "config")
        .action((x, c) => c.copy(configFile = Some(x)))
        .text("Шлях до конфігураційного файлу")
    )
  }
}

/**
  * Головний клас для запуску AxiDraw.
  */
object Application {

  private val logger = LoggerFactory.getLogger(getClass)

  def main(args: Array[String]): Unit =
    // Розбір аргументів командного рядка за допомогою scopt
    scopt.OParser.parse(CommandLineOptions.parser, args, CommandLineOptions.Options()) match {
      case Some(cliOptions) =>
        // Завантаження базової конфігурації з файлу (якщо задано), інакше використовується порожня конфігурація.
        val baseConfig: Config = cliOptions.configFile match {
          case Some(filePath) =>
            val file = new java.io.File(filePath)
            if (file.exists()) {
              logger.info(s"Завантаження конфігурації з файлу: ${file.getAbsolutePath}")
              ConfigFactory.parseFile(file)
            } else {
              logger.warn(s"Конфігураційний файл '$filePath' не знайдено, використовується порожня конфігурація")
              ConfigFactory.empty()
            }
          case None =>
            ConfigFactory.empty()
        }

        // Остаточна конфігурація: базова конфігурація з файлу або порожня, з fallback до стандартної.
        val finalConfig = baseConfig.withFallback(ConfigFactory.load())

        logger.info("Запуск системи AxiDraw...")

        // Створення акторної системи з використанням отриманої конфігурації.
        val system: ActorSystem[AxiDrawActor.Command] =
          ActorSystem[AxiDrawActor.Command](AxiDrawActor(Some(finalConfig)), "AxiDrawSystem")

        // Надсилання прикладового креслення через команду Draw.
        val samplePath = com.scala.axidraw.Path(
          Seq(
            com.scala.axidraw.Point(10, 10),
            com.scala.axidraw.Point(80, 90),
            com.scala.axidraw.Point(150, 20),
            com.scala.axidraw.Point(10, 10)
          )
        )
        val sampleDrawing = com.scala.axidraw.Drawing(Seq(samplePath))
        system ! AxiDrawActor.Draw(sampleDrawing)

        sys.addShutdownHook {
          logger.info("Отримано сигнал завершення роботи...")
          system.terminate()
          logger.info("Система зупинена.")
        }

        logger.info("Система готова до роботи. Для виходу натисніть Ctrl+C")
        Await.result(system.whenTerminated, Duration.Inf)

      case _ =>
        logger.error("Не вдалося розпарсити аргументи командного рядка.")
        System.exit(1)
    }

}

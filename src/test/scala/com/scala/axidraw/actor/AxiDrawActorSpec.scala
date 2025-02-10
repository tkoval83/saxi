package com.scala.axidraw.actor

import akka.actor.testkit.typed.scaladsl.ActorTestKit
import com.scala.axidraw._
import com.typesafe.config.ConfigFactory
import org.scalatest.BeforeAndAfterAll
import org.scalatest.wordspec.AnyWordSpecLike

class AxiDrawActorSpec extends AnyWordSpecLike with BeforeAndAfterAll {

  private val testKit = ActorTestKit()

  override def afterAll(): Unit = testKit.shutdownTestKit()

  "AxiDrawActor" should {
    "process a Draw message for a square without crashing" in {
      // Створюємо шлях для квадрата з куточками:
      // (0, 0) -> (0, 50) -> (50, 50) -> (50, 0) -> (0, 0)
      val squarePath = Path(
        Seq(
          Point(0, 0),
          Point(0, 50),
          Point(50, 50),
          Point(50, 0),
          Point(0, 0)
        )
      )
      val squareDrawing = Paths(Seq(squarePath))
      val config = ConfigFactory.empty()

      val axiDrawActor = testKit.spawn(AxiDrawActor(Some(config)), "axiDrawActorSquare")
      axiDrawActor ! AxiDrawActor.Draw(squareDrawing)

      // Чекаємо достатньо часу для виконання креслення (при потребі збільште час)
      Thread.sleep(4000)
    }
  }

}

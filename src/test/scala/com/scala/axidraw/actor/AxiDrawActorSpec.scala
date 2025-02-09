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
    "process a Draw message without crashing" in {
      val dummyPath = Path(Seq(Point(0, 0), Point(10, 10)))
      val drawing   = Drawing(Seq(dummyPath))
      val config    = ConfigFactory.empty()

      val axiDrawActor = testKit.spawn(AxiDrawActor(Some(config)), "axiDrawActor")
      axiDrawActor ! AxiDrawActor.Draw(drawing)

      Thread.sleep(4000)
    }
  }

}

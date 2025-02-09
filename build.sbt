name := "saxi"
version := "1.0"
scalaVersion := "2.13.16"
resolvers += "Akka library repository".at("https://repo.akka.io/maven")
lazy val akkaVersion = sys.props.getOrElse("akka.version", "2.10.1")

// Run in a separate JVM, to make sure sbt waits until all threads have
// finished before returning.
// If you want to keep the application running while executing other
// sbt tasks, consider https://github.com/spray/sbt-revolver/
fork := true

libraryDependencies ++= Seq(
  "com.fazecast"           % "jSerialComm"               % "2.11.0",
  "org.typelevel"          %% "cats-core"                % "2.13.0",
  "com.typesafe.akka"      %% "akka-actor-typed"         % akkaVersion,
  "ch.qos.logback"         % "logback-classic"           % "1.5.16",
  "org.locationtech.jts"   % "jts-core"                  % "1.20.0",
  "com.github.scopt"       %% "scopt"                    % "4.1.0",
  "io.circe"               %% "circe-core"               % "0.14.10",
  "io.circe"               %% "circe-generic"            % "0.14.10",
  "io.circe"               %% "circe-parser"             % "0.14.10",
  "org.scala-lang.modules" %% "scala-xml"                % "2.3.0",
  "com.typesafe.akka"      %% "akka-actor-testkit-typed" % akkaVersion % Test,
  "org.scalatest"          %% "scalatest"                % "3.2.19" % Test
)

assemblyMergeStrategy in assembly := {
  case "module-info.class" => MergeStrategy.discard
  case x                   => (assemblyMergeStrategy in assembly).value(x)
}

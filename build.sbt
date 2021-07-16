name := "helloworld"

version := "0.1"

scalaVersion := "2.13.6"

val http4sVersion = "0.21.24"
val circeVersion = "0.14.1"
val scalaTestVersion = "3.0.8"
val catsEffectVersion = "2.5.1"
val catsCoreVersion = "2.3.0"
val newTypeVersion = "0.4.4"
val logBackVersion = "1.1.3"


libraryDependencies ++= Seq(
  "org.http4s" %% "http4s-dsl" % http4sVersion,
  "org.http4s" %% "http4s-circe" % http4sVersion,
  "org.http4s" %% "http4s-blaze-server" % http4sVersion,
  "org.http4s" %% "http4s-blaze-client" % http4sVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-literal" % circeVersion,
  "io.circe" %% "circe-parser" % circeVersion,
  "org.scalatest" %% "scalatest" % scalaTestVersion % Test,
  "co.fs2"           %% "fs2-core"      % "2.2.2",
  "org.typelevel" %% "cats-effect-std" %  "3.1.1",
  "org.typelevel" %% "cats-effect" % catsEffectVersion,
  "org.typelevel" %% "cats-core" % catsCoreVersion,
  "io.estatico" %% "newtype" % newTypeVersion,
  "ch.qos.logback" % "logback-classic" % logBackVersion % Runtime
)

enablePlugins(JavaAppPackaging)
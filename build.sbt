//name := "ws"

version := "0.1"

scalaVersion := "2.13.6"

val http4sVersion = "0.21.24"
val circeVersion = "0.14.1"
val scalaTestVersion = "3.0.8"
val catsEffectVersion = "2.5.1"
val catsCoreVersion = "2.3.0"
val newTypeVersion = "0.4.4"
val logBackVersion = "1.1.3"
val elastic4sVersion = "7.12.3"
val fs2core = "2.2.2"
val catsEffectStd = "3.1.1"
val doobieVersion = "0.12.1"


libraryDependencies ++= Seq(
  "org.http4s" %% "http4s-dsl" % http4sVersion,
  "org.http4s" %% "http4s-circe" % http4sVersion,
  "org.http4s" %% "http4s-blaze-server" % http4sVersion,
  "org.http4s" %% "http4s-blaze-client" % http4sVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-literal" % circeVersion,
  "io.circe" %% "circe-parser" % circeVersion,
  "org.scalatest" %% "scalatest" % scalaTestVersion % Test,
  "co.fs2"           %% "fs2-core"      % fs2core,
  "org.typelevel" %% "cats-effect-std" %  catsEffectStd,
  "org.typelevel" %% "cats-effect" % catsEffectVersion,
  "org.typelevel" %% "cats-core" % catsCoreVersion,
  "io.estatico" %% "newtype" % newTypeVersion,
  "ch.qos.logback" % "logback-classic" % logBackVersion % Runtime,
  "org.tpolecat" %% "doobie-core"      % doobieVersion,
  "org.tpolecat" %% "doobie-postgres"  % doobieVersion,
)

enablePlugins(JavaAppPackaging)
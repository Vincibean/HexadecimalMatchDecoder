name := "Hexadecimal Stream Decoder"

version := "1.0"

scalaVersion := "2.12.1"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats" % "0.9.0",
  "org.specs2" %% "specs2-core" % "3.8.9" % "test"
)

scalacOptions in Test ++= Seq("-Yrangepos")

onLoad in Global := (Command
  .process("scalafmt", _: State)) compose (onLoad in Global).value

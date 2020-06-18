ThisBuild / scalaVersion := "2.13.1"
ThisBuild / useSuperShell := false

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "2.0.0",
  "com.davegurnell" %% "unindent" % "1.1.1",
  "org.scalacheck" %% "scalacheck" % "1.14.1" % "test"
)

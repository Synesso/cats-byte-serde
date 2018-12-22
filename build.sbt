name := "cats-decode"
organization := "io.github.synesso"
scalaVersion := "2.12.8"
libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "1.5.0",
  "org.specs2" %% "specs2-core" % "4.3.6" % "test",
  "org.specs2" %% "specs2-scalacheck" % "4.3.6" % "test"
)


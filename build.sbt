
lazy val common = Seq(
  name := "tuple-conversion",
  version := "0.1",
  scalaVersion := "2.13.4",
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
)

lazy val macros = project.in(file("macros"))
  .settings(common)

lazy val root = project.in(file("."))
  .settings(common)
  .dependsOn(macros)

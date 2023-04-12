lazy val root = (project in file("."))
  .settings(
    name := "persimmon",
    scalaVersion := "3.2.2",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1",
    libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.14",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.14",
    parallelExecution in Test := false
  )

Compile / scalaSource := baseDirectory.value / "src"

Test / scalaSource := baseDirectory.value / "test"
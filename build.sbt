lazy val root = (project in file(".")).
  settings(
    name := "sbt_practice",
    version := "1.0",
    scalaVersion := "2.11.7"
  )

libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "1.0.1"

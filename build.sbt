scalaVersion in ThisBuild := "2.13.0"

lazy val root = (project in file("."))
  .aggregate(exercises, answers)
  .settings(
    name := "fpinscala"
  )

lazy val exercises = (project in file("exercises"))
  .settings(
    name := "exercises"
  )

lazy val answers = (project in file("answers"))
  .settings(
    name := "answers"
  )

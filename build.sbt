ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(
    name := "Primes-Continuation",
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
  )
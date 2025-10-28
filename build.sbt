import Dependencies._

ThisBuild / scalaVersion     := "2.13.16"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val scrimageVersion = "4.0.31" // o el más reciente disponible

lazy val root = (project in file("."))
  .settings(
    name := "image-converter",

    libraryDependencies ++= Seq(
      munit % Test,
      "com.sksamuel.scrimage" % "scrimage-core" % scrimageVersion,
      "com.sksamuel.scrimage" %% "scrimage-scala" % scrimageVersion,
      "com.sksamuel.scrimage" % "scrimage-webp" % scrimageVersion
    )
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.

import Dependencies.*

ThisBuild / scalaVersion     := "2.13.16"
ThisBuild / version          := "0.5.5"
ThisBuild / organization     := "com.aranadedoros"
ThisBuild / organizationName := "aranadedoros"

Compile / doc / scalacOptions ++= Seq(
  "-skip-packages", "example"
)
Compile / packageBin / mappings := {
  val original = (Compile / packageBin / mappings).value
  original.filterNot { case (_, pathInJar) =>
    pathInJar.startsWith("main/")
  }
}

lazy val scrimageVersion = "4.0.31"
resolvers += Resolver.mavenCentral

lazy val root = (project in file("."))
  .settings(
    name := "AssetFlow",
    libraryDependencies ++= Seq(
      munit % Test,
      "com.sksamuel.scrimage" % "scrimage-core" % scrimageVersion,
      "com.sksamuel.scrimage" %% "scrimage-scala" % scrimageVersion,
      "com.sksamuel.scrimage" % "scrimage-webp" % scrimageVersion,
      "com.sksamuel.scrimage" % "scrimage-filters" % scrimageVersion,
      "de.androidpit" % "color-thief" % "1.1.2",
       "org.scala-lang" % "scala-reflect" % scalaVersion.value
    )
  )

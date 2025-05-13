// Root project
lazy val root = (project in file("."))
  .aggregate(core, cli)
  .settings(
    name := "ScalaMusicAI",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := "3.3.3"
  )

// Core module with the main functionality
lazy val core = (project in file("core"))
  .settings(
    name := "scala-music-ai-core",
    libraryDependencies ++= Seq(
      // Testing
      "org.scalatest" %% "scalatest" % "3.2.17" % Test,
      
      // Logging
      "ch.qos.logback" % "logback-classic" % "1.4.7",
      "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5"
    )
  )

// Command-line interface module
lazy val cli = (project in file("cli"))
  .dependsOn(core)
  .settings(
    name := "scala-music-ai-cli",
    // Add CLI-specific dependencies here
  )

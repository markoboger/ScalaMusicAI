// Root project
lazy val root = (project in file("."))
  .aggregate(core, cli)
  .settings(
    name := "ScalaMusicAI",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := "3.3.3",
    // Set UTF-8 encoding for all projects
    scalacOptions ++= Seq("-encoding", "utf8"),
    javacOptions ++= Seq("-encoding", "UTF-8"),
    // Set system properties for consistent encoding
    Compile / run / fork := true,
    Test / fork := true,
    run / javaOptions ++= Seq(
      "-Dfile.encoding=UTF-8",
      "-Duser.country=US",
      "-Duser.language=en"
    )
  )

// Core module with the main functionality
lazy val core = (project in file("core"))
  .settings(
    name := "scala-music-ai-core",
    // Ensure proper encoding for tests
    Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oD"),
    Test / javaOptions ++= Seq(
      "-Dfile.encoding=UTF-8",
      "-Duser.country=US",
      "-Duser.language=en"
    ),
    libraryDependencies ++= Seq(
      // Testing
      "org.scalatest" %% "scalatest" % "3.2.17" % Test,

      // Logging
      "ch.qos.logback" % "logback-classic" % "1.4.7",
      "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",
      
      // For worksheets
      "org.scala-lang" % "scala-compiler" % scalaVersion.value,
      "org.scala-lang" % "scala-reflect" % scalaVersion.value
    ),
    
    // Enable worksheet support
    Compile / console / scalacOptions ++= Seq("-deprecation", "-feature"),
    run / fork := true,
    run / connectInput := true,
    outputStrategy := Some(StdoutOutput)
  )

// Command-line interface module
lazy val cli = (project in file("cli"))
  .dependsOn(core)
  .settings(
    name := "scala-music-ai-cli",
    // Ensure proper encoding for CLI
    Compile / run / fork := true,
    run / javaOptions ++= Seq(
      "-Dfile.encoding=UTF-8",
      "-Duser.country=US",
      "-Duser.language=en"
    )
    // Add CLI-specific dependencies here
  )

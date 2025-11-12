val scala3Version = "3.3.7"

lazy val root = project
  .in(file("."))
  .settings(
    name := "my-dotty-cps-async",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    scalacOptions += "-Xprint:postInlining",
    scalacOptions += "-Xmax-inlines:100000",
    libraryDependencies += "io.github.dotty-cps-async" %% "dotty-cps-async" % "1.1.4"
  )

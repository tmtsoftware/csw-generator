import sbt.Keys._
import sbt._

//noinspection TypeAnnotation
// Defines the global build settings so they don't need to be edited everywhere
object Settings {
  val Version = "0.1-SNAPSHOT"
  val ScalaVersion = "2.13.5"

  val buildSettings = Seq(
    organization := "org.tmt",
    organizationName := "TMT",
    organizationHomepage := Some(url("http://www.tmt.org")),
    version := Version,
    scalaVersion := ScalaVersion,
    parallelExecution in Test := false,
    fork := true,
    resolvers += "jitpack" at "https://jitpack.io",
    updateOptions := updateOptions.value.withLatestSnapshots(false),
    scalacOptions ++= Seq(
      "-encoding",
      "UTF-8",
      "-feature",
      "-unchecked",
      "-deprecation",
      //-W Options
      "-Wdead-code",
      //-X Options
      "-Xlint:_,-missing-interpolator",
      "-Xsource:3",
      "-Xcheckinit",
      "-Xasync"
    ),
    testOptions in Test ++= Seq(
      // show full stack traces and test case durations
      Tests.Argument("-oDF")
    )
  )
}

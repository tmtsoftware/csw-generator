import sbt.Keys._
import sbt._

import Dependencies._
import Settings._

lazy val `csw-generator-root` = project.in(file(".")).aggregate(`csw-generator`)

lazy val `csw-generator` = project
  .enablePlugins(DeployApp)
  .settings(buildSettings: _*)
  .settings(libraryDependencies ++= `csw-generator-deps`)

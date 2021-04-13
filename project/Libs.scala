import sbt._

object Libs {
  val `scopt` = "com.github.scopt" %% "scopt" % "4.0.0" //MIT License
  val `scalaTest` = "org.scalatest" %% "scalatest" % "3.2.3" // ApacheV2
  val `scala-async` = "org.scala-lang.modules" %% "scala-async" % "1.0.0-M1" //BSD 3-clause "New" or "Revised" License
}

object CSW {
  private val Org = "com.github.tmtsoftware.csw"
//  private val Version = "0.1.0-SNAPSHOT"
  private val Version = "3.0.1"

  val `csw-logging-client` = Org %% "csw-logging-client" % Version
  val `csw-commons` = Org %% "csw-commons" % Version
  val `csw-event-client` = Org %% "csw-event-client" % Version
  val `csw-location-client` = Org %% "csw-location-client" % Version
  val `csw-prefix` = Org %% "csw-prefix" % Version
}

object ICD {
  private val Org = "com.github.tmtsoftware.icd"
  private val Version = "2.1.1"

  val `icd-db` = Org %% "icd-db" % Version
}

import sbt._

object Dependencies {

  val `csw-generator-deps` = Seq(
    CSW.`csw-event-client`,
    CSW.`csw-location-client`,
    CSW.`csw-location-client`,
    CSW.`csw-logging-client`,
    ICD.`icd-db`,
    Libs.`scopt`,
    Libs.`scalaTest` % Test
  )
}

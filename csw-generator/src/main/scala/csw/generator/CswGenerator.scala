package csw.generator

import akka.actor.typed.{ActorSystem, SpawnProtocol}
import csw.services.icd.db.{IcdDb, IcdDbException}
import icd.web.shared.BuildInfo

object CswGenerator extends App {
  import csw.services.icd.db.IcdDbDefaults._

  case class Options(
      dbName: String = defaultDbName,
      host: String = defaultHost,
      port: Int = defaultPort,
      subsystem: Option[String] = None,
      component: Option[String] = None,
      event: Option[String] = None,
      publish: Option[Unit] = None
  )

  // Parser for the command line options
  private val parser = new scopt.OptionParser[Options]("csw-event-generator") {
    head("csw-event-generator", BuildInfo.version)

    opt[String]('d', "db") valueName "<name>" action { (x, c) =>
      c.copy(dbName = x)
    } text s"The name of the database to use (default: $defaultDbName)"

    opt[String]('h', "host") valueName "<hostname>" action { (x, c) =>
      c.copy(host = x)
    } text s"The host name where the database is running (default: $defaultHost)"

    opt[Int]('p', "port") valueName "<number>" action { (x, c) =>
      c.copy(port = x)
    } text s"The port number to use for the database (default: $defaultPort)"

    opt[String]('s', "subsystem") valueName "<subsystem>[:version]" action { (x, c) =>
      c.copy(subsystem = Some(x))
    } text "Specifies the subsystem (and optional version) to be used by any following options"

    opt[String]('c', "component") valueName "<name>" action { (x, c) =>
      c.copy(component = Some(x))
    } text "Specifies the component to be used by any following options (subsystem must also be specified)"

    opt[String]('e', "event") valueName "<name>" action { (x, c) =>
      c.copy(event = Some(x))
    } text "Specifies the event to be used by any following options (subsystem and component must also be specified)"

    opt[Unit]("publish") action { (_, c) =>
      c.copy(publish = Some(()))
    } text "Start publishing the selected events at the defined rate (Needs subsystem, component and optionally event options)"

    help("help")
    version("version")
  }

  parser.parse(args, Options()) match {
    case Some(options) =>
      try {
        run(options)
      }
      catch {
        case _: IcdDbException =>
          println("Error: Failed to connect to mongodb. Make sure mongod server is running.")
          System.exit(1)
        case e: Throwable =>
          e.printStackTrace()
          System.exit(1)
      }
    case None => System.exit(1)
  }

  // Run the application
  private def run(options: Options): Unit = {
    val db = IcdDb(options.dbName, options.host, options.port)

    def error(msg: String): Unit = {
      println(msg)
      System.exit(1)
    }

    options.publish.foreach(_ => publish())

    // XXX TODO: Allow component to be missing? (get list of all subsystem components - could be a lot of events)
    // XXX TODO: Allow subsystem to be missing? (Might be too many events fired at once on single system!)
    def publish(): Unit = {
      if (options.subsystem.isEmpty) error("Missing required subsystem name: Please specify --subsystem <name>.")
      val subsystem = options.subsystem.get
      if (options.component.isEmpty) error("Missing required component name: Please specify --component <name>.")
      val component           = options.component.get
      val maybeComponentModel = db.query.getComponentModel(subsystem, component, None)
      if (maybeComponentModel.isEmpty) error(s"Component $component not found in the icd database.")

      implicit val actorSystem: ActorSystem[SpawnProtocol.Command] = ActorSystem(SpawnProtocol(), "csw-event-generator")
      val events = db.query
        .getPublishModel(maybeComponentModel.get, None)
        .toList
        .flatMap(_.eventList)
        .filter(p => options.event.isEmpty || options.event.get == p.name)
      if (events.isEmpty) {
        if (options.event.isDefined) error(s"Event '${options.event.get}'' not found in the database.")
        error("No events found.")
      }
      new EventGeneratorPublisher(subsystem, component, events)
    }
  }

}

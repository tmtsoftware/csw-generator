package csw.generator

import akka.actor.typed.{ActorSystem, SpawnProtocol}
import csw.event.api.scaladsl.{EventPublisher, EventService}
import csw.event.client.EventServiceFactory
import csw.location.api.scaladsl.LocationService
import csw.location.client.scaladsl.HttpLocationServiceFactory
import icd.web.shared.IcdModels.EventModel

import scala.concurrent.duration._
import csw.event.api.exceptions.PublishFailure
import csw.logging.client.scaladsl.{GenericLoggerFactory, LoggingSystemFactory}
import csw.params.events.{Event, EventName, SystemEvent}
import csw.prefix.models.Prefix

import java.net.InetAddress

class EventGeneratorPublisher(subsystem: String, component: String, events: List[EventModel])(implicit
    actorSystem: ActorSystem[SpawnProtocol.Command]
) {
  private val host = InetAddress.getLocalHost.getHostName
  LoggingSystemFactory.start("IcdEventGen", "0.1", host, actorSystem)
  private val log                      = GenericLoggerFactory.getLogger
  val locationService: LocationService = HttpLocationServiceFactory.makeLocalClient
  val eventService: EventService       = new EventServiceFactory().make(locationService)
  val publisher: EventPublisher        = eventService.defaultPublisher

  def onError(publishFailure: PublishFailure): Unit =
    log.error(
      s"Publish failed for event: [${publishFailure.event}]",
      ex = publishFailure.cause
    )

  events.foreach { event =>
    def eventGenerator(): Option[Event] = {
      try {
        val paramSet = ParamSetGenerator.makeParamSet(event.parameterList)
        Some(SystemEvent(Prefix(s"$subsystem.$component"), EventName(event.name), paramSet))
      } catch {
        case ex: Exception =>
          println(s"Failed to generate param key for event: ${event.name}: ${ex.getMessage}")
          None
      }
    }
    val maxRateHz = event.maybeMaxRate.getOrElse(EventModel.defaultMaxRate)
    publisher.publish(eventGenerator(), (1 / maxRateHz).seconds, p => onError(p))
  }
}

import zio._

import java.io.IOException
import java.time.LocalDateTime

//
// Meetup.com
//
// Notifications
//  notify every user who is subbed to an upcoming event
//  - get all events in the next 24 hours
//  - get all users are RSVPed to each event
//  - send an email notification to each user

final case class Event( name: String, startDate: LocalDateTime, userIds: List[Int])
final case class User(id: Int, email: String)

trait Events {
  def allTheEventsInTheNext24Hours: ZIO[Any, Throwable, List[Event]]
}

object Events {
  val live: ULayer[Events] =
    ZLayer.succeed(new Events {
    def allTheEventsInTheNext24Hours: ZIO[Any, Throwable, List[Event]] =
      ZIO.succeed(
        List(
          Event("Scala Meetup", LocalDateTime.now().plusHours(1), List(1)),
          Event("ZIO Hackathon", LocalDateTime.now().plusHours(1), List(3)),
        )
      )
  })
}

trait Users {
  def getUsersWithIds(userIds: List[Int]): ZIO[Any, Throwable, List[User]]
}

final case class UsersLive(events: Events) extends Users {
  def getUsersWithIds(userIds: List[Int]): ZIO[Any, Throwable, List[User]] =
    ZIO.succeed(
      List(
        User(1, "kit.langton@gmail.com"),
        User(2, "olive@gmail.com"),
        User(3, "crumb@hotmail.gov.nz"),
      ).filter(user => userIds.contains(user.id))
    )
}

object Users {
  val layer: ZLayer[Events, Nothing, UsersLive] =
    ZLayer.fromFunction(UsersLive.apply _)
}

trait Messaging {
  def sendTelegram(email: String): ZIO[Any, Throwable, Unit]
}

object Messaging {
  val live: ULayer[Messaging] = ZLayer.succeed {
    new Messaging {
      def sendTelegram(email: String): ZIO[Any, Throwable, Unit] =
        ZIO.debug(s"Sending telegram to $email")
    }
  }
}

final case class Notifications(
    events: Events,
    users: Users,
    messaging: Messaging
                              ) {
  val run: ZIO[Any, Throwable, Unit] =
    for {
      allEvents <- events.allTheEventsInTheNext24Hours
      users <- ZIO.foreach(allEvents) { event =>
        users.getUsersWithIds(event.userIds)
      }
      _ <- ZIO.foreachDiscard(users.flatten) { user =>
        messaging.sendTelegram(user.email)
      }
    } yield ()
}

object Notifications {
  val layer: ZLayer[Events with Users with Messaging, Nothing, Notifications] =
    ZLayer.fromFunction(Notifications.apply _)
}

//
object Main extends ZIOAppDefault {

  val program: ZIO[Notifications, Throwable, Unit] =
    ZIO.serviceWithZIO[Notifications](_.run)

  val run = program
    .provide(
      ZLayer.Debug.mermaid,
      Notifications.layer,
      Messaging.live,
      Users.layer,
      Events.live
    )

}
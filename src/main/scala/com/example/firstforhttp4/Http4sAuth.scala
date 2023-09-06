import cats.data.{Kleisli, OptionT}
import cats.effect.{IO, IOApp}
import com.comcast.ip4s
import com.comcast.ip4s.{ipv4, port}
import org.http4s
import org.http4s.*
import org.http4s.dsl.io
import org.http4s.dsl.io.*
import org.http4s.ember.server.*
import org.http4s.headers.Authorization
import org.http4s.server.AuthMiddleware
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory

import scala.language.postfixOps

case class User(id: Int, name: String, password: String)

//curl -v localhost:8080/welcome
/* echo "daniel:rockthejvm" | base64
curl -v -H 'Authorization:Basic ZGFuaWVsOnJvY2t0aGVqdm0K' localhost:8080/welcome
 */

object NoAuth extends IOApp.Simple {
  implicit val loggerFactory: LoggerFactory[IO] = Slf4jFactory.create[IO]

  var routes: HttpRoutes[IO] =
    HttpRoutes.of { case GET -> Root / "welcome" / user =>
      Ok(s"Welcome $user")
    }

  val server = EmberServerBuilder
    .default[IO]
    .withHost(ipv4"0.0.0.0")
    .withPort(port"8080")
    .withHttpApp(routes.orNotFound)
    .build

  override def run: IO[Unit] =
    server.use(_ => IO.never).void
}

object BasicAuth extends IOApp.Simple {
  implicit val loggerFactory: LoggerFactory[IO] = Slf4jFactory.create[IO]

  // Request[IO] => IO[Either[String, User]]
  // equivalent to Kleisli[IO, Request[IO], Either[String, User]]
  // Kleisli[F, A, B] equivalent to A => F[B]

  //
  val basicAuthMethod = Kleisli.apply[IO, Request[IO], Either[String, User]] { request =>
    // auth logic
    val authHeader = request.headers.get[Authorization]
    authHeader match {
      case Some(Authorization(BasicCredentials(credentials))) if isValidCredentials(credentials) =>
        IO(Right(User(1, credentials._1, credentials._2))) // this user normally get from DB
      case Some(_) => IO(Left("No basic credentials"))
      case None    => IO(Left("Unauthorized1"))
    }
  }

  var onFailure: AuthedRoutes[String, IO] = Kleisli { (req: AuthedRequest[IO, String]) =>
    OptionT.pure[IO](Response[IO](status = Status.Unauthorized))
  }
  //middleware
  val userBasicAuthMiddleware: AuthMiddleware[IO, User] = AuthMiddleware(basicAuthMethod, onFailure)

  var routes = AuthedRoutes.of[User, IO] { case GET -> Root / "welcome" as user =>
    Ok(s"Welcome $user")
  }

  val server = EmberServerBuilder
    .default[IO]
    .withHost(ipv4"0.0.0.0")
    .withPort(port"8080")
    .withHttpApp(userBasicAuthMiddleware(routes).orNotFound)
    .build

  // another way
  val basicAuthMethod1: Kleisli[OptionT[IO, *], Request[IO], User] =
    Kleisli(request => {
      val maybeCredentials: Option[(String, String)] = request.headers.get[Authorization].collect {
        case Authorization(BasicCredentials(credentials)) => credentials
      }

      maybeCredentials match {
        case Some(creds) if isValidCredentials(creds) =>
          OptionT.liftF(IO(User(1, creds._1, creds._2)))
        case _ =>
          OptionT.none[IO, User]
      }
    })

  val userBasicAuthMiddleware1: AuthMiddleware[IO, User] =
    AuthMiddleware(basicAuthMethod1)

  val server1 = EmberServerBuilder
    .default[IO]
    .withHost(ipv4"0.0.0.0")
    .withPort(port"8080")
    .withHttpApp(userBasicAuthMiddleware1(routes).orNotFound)
    .build

  def isValidCredentials(credentials: (String, String)): Boolean =
    credentials._1 match {
      case "daniel" => true
      case _        => false
    }

  override def run: IO[Unit] =
//    server.use(_ => IO.never).void
    server1.use(_ => IO.never).void
}

object DigestAuth extends IOApp.Simple {
  implicit val loggerFactory: LoggerFactory[IO] = Slf4jFactory.create[IO]



  val server = EmberServerBuilder
    .default[IO]
    .withHost(ipv4"0.0.0.0")
    .withPort(port"8080")
    .withHttpApp(???)
    .build

  override def run: IO[Unit] =
    server.use(_ => IO.never).void
}

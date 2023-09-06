package Http4sTests

import cats.Monad
import cats.effect.*
import cats.implicits.*
import com.comcast.ip4s.{ipv4, port}
import org.http4s.circe.*
import org.http4s.*
import io.circe.generic.auto.*
import io.circe.syntax.*
import org.http4s.Method.GET
import org.http4s.dsl.*
import org.http4s.dsl.impl.*
import org.http4s.dsl.io.*
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.headers.*
import org.http4s.implicits.*
import org.http4s.server.*
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory

import java.time.Year
import java.util.UUID
import scala.collection.mutable
import scala.util.Try

object Http4sTutorial extends IOApp {

  //movie database
  private type Actor = String
  case class Movie(id: String, title: String, year: Int, actors: List[Actor], director: String)
  case class Director(firstName: String, lastName: String) {
    override def toString: Actor = s"$firstName $lastName"
  }
  case class DirectorDetails(firstName: String, lastName: String, role: String)

  //prepare Database
  var snjl: Movie = Movie(
    "ed7e9740-09ee-4748-857c-c692e32bdfee",
    "Zark Snyder's Justice League",
    2021,
    List("Henry", "Gal", "Ezra", "Ben", "Ray"),
    "Zack Snyder"
  )

  val movies: Map[String, Movie] = Map(snjl.id -> snjl)

  private def findMovieById(id: UUID) = movies.get(id.toString)

  private def findMoviesByDirector(director: String): List[Movie] =
    movies.values.filter(_.director == director).toList

  /*
    - Get all movies for a director under a given year
    - Get all actors for a movie
    - Get detail of a director
    - Post add a new director
   */

  // Request -> Response normally it will be Request -> F[Response]
  // Some times it will be Request -> F[Option[Response]]
  // HttpRoutes[F]

//  implicit val yearParamDecoder: QueryParamDecoder[Year] =
//    QueryParamDecoder[Int].map(yearInt => Year.of(yearInt))
  implicit val yearParamDecoder: QueryParamDecoder[Year] =
    QueryParamDecoder[Int].emap(yearInt =>
      Try(Year.of(yearInt)).toEither
        .leftMap { e => //change Throwable to ParseFailure by leftMap
          ParseFailure(e.getMessage, e.getMessage)
        }
    )

  object DirectorParamDecoderMatcher extends QueryParamDecoderMatcher[String]("director")
//  object YearParamDecoderMatcher extends QueryParamDecoderMatcher[Year]("year")
  object YearParamDecoderMatcher extends OptionalValidatingQueryParamDecoderMatcher[Year]("year")

  def movieRoutes[F[_]: Monad]: HttpRoutes[F] = {
    val dsl = Http4sDsl[F]
    import dsl._

    HttpRoutes.of[F] {
      case GET -> Root / "movies" :? DirectorParamDecoderMatcher(director) +& YearParamDecoderMatcher(maybeYear) => {
        maybeYear match {
          case Some(validatedYear) =>
            validatedYear.fold( // the method from Validated. if failed return left, otherwise return right
              _ => BadRequest("bad request"),
              year => {
                val moviesByDirector = findMoviesByDirector(director)
                val moviesByYear = moviesByDirector.filter(_.year == year.getValue)
                Ok(moviesByYear.asJson)
              }
            )
          case None => Ok()
        }
      }
      case GET -> Root / "movies" / UUIDVar(movieId) / "actors" =>
        findMovieById(movieId).map(_.actors) match {
          case Some(actors) => Ok(actors.asJson)
          case _            => NotFound()
        }
    }
  }

  object DirectorPath {
    def unapply(str: String): Option[Director] = {
      Try {
        val token = str.split(" ")
        Director(token(0), token(1))
      }.toOption
    }
  }

  val memoryDB: mutable.Map[Director, DirectorDetails] =
    mutable.Map(Director("Zark", "Snyder") -> DirectorDetails("Zark", "Snyder", "Superhero"))

  // GET /movies?director=Zark%20Snyder&Year=2021
  def directorRoutes[F[_]: Monad]: HttpRoutes[F] = {
    val dsl = Http4sDsl[F]
    import dsl._

    HttpRoutes.of[F] {
      case GET -> Root / "directors" / DirectorPath(director) =>
        memoryDB.get(director) match {
          case Some(dirDetails) => Ok(dirDetails.asJson)
          case _                => NotFound("There is no director")
        }
      case POST -> Root / "directors" / "acc" => ???
    }
  }

  def allRoutes[F[_]: Monad]: HttpRoutes[F] =
    movieRoutes[F] <+> directorRoutes[F]

  def allRoutesCompleted[F[_]: Monad]: HttpApp[F] =
    allRoutes[F].orNotFound

  implicit val loggerFactory: LoggerFactory[IO] = Slf4jFactory.create[IO]

  override def run(args: List[String]): IO[ExitCode] = {
    val apis = Router(
      "/api" -> movieRoutes[IO],
      "/api/admin" -> directorRoutes[IO]
    ).orNotFound

    EmberServerBuilder
      .default[IO]
      .withHost(ipv4"0.0.0.0")
      .withPort(port"8080")
      .withHttpApp(apis)
      .build
      .use(_ => IO.never)
      .as(ExitCode.Success)
  }
}

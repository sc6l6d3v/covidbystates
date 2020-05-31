package com.iscs.covidbystates.domains

import java.util.concurrent.ConcurrentHashMap

import cats.effect.{Concurrent, Sync}
import cats.implicits._
import com.iscs.covidbystates.covid.{CovidApiUri, CovidTrackApiUri}
import com.typesafe.scalalogging.Logger
import dev.profunktor.redis4cats.RedisCommands
import io.circe._
import io.circe.generic.semiauto._
import org.http4s.Method._
import org.http4s.circe._
import org.http4s.client.Client
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s._
import org.http4s.{EntityDecoder, EntityEncoder}
import upickle.default._
import upickle.default.{ReadWriter => RW, macroRW}

trait Covid[F[_]] {
  def getByState(state: String, date: String): F[Covid.State]
  def getByStates(states: List[String], date: String): F[Covid.State]
  def getByCity(state: String, city: String): F[Covid.City]
  def getByCities(state: String, cities: List[String]): F[Covid.City]
}

object Covid {
  private val L = Logger[this.type]

  def apply[F[_]](implicit ev: Covid[F]): Covid[F] = ev

  final case class State(state: String, positive: Int, death: Int) {
    override def toString: String = write(this)
  }
  final case class City(state: String, city: String, confirmed: Int, deaths: Int) {
    override def toString: String =  write(this)
  }

  object State {
    implicit val rw: RW[State] = macroRW

    implicit val stateDecoder: Decoder[State] = (c: HCursor) => {
      val state = c.downField("state").as[String].getOrElse("NOSTATE")
      val positive = c.downField("positive").as[Int].getOrElse(0)
      val death = c.downField("death").as[Int].getOrElse(0)
      Right(State(state, positive, death))
    }

    implicit def stateEntityDecoder[F[_]: Sync]: EntityDecoder[F, State] = jsonOf
    implicit val stateEncoder: Encoder[State] = deriveEncoder[State]
    implicit def stateEntityEncoder[F[_]: Sync]: EntityEncoder[F, State] = jsonEncoderOf
  }

  object City {
    implicit val rw: RW[City] = macroRW

    implicit val cityDecoder: Decoder[City] = (c: HCursor) => {
      val topObj = c.downField("data").downArray
      val regionObj = topObj.downField("region")
      val state = regionObj.downField("province").as[String].getOrElse("NOSTATE")
      val city = regionObj.downField("cities").downArray.downField("name").as[String].getOrElse("NOCITY")
      val citiesObj = regionObj.downField("cities").downArray
      val confirmed = citiesObj.downField("confirmed").as[Int].getOrElse(0)
      val deaths = citiesObj.downField("deaths").as[Int].getOrElse(0)
      Right(City(state, city, confirmed, deaths))
    }

    implicit def cityEntityDecoder[F[_]: Sync]: EntityDecoder[F, City] = jsonOf
    implicit val cityEncoder: Encoder[City] = deriveEncoder[City]
    implicit def cityEntityEncoder[F[_]: Sync]: EntityEncoder[F, City] = jsonEncoderOf
  }  

  final case class DataError(e: Throwable) extends RuntimeException

  private def fromCity(city: String): City = read[City](city)

  private def fromState(state: String): State = read[State](state)

  def impl[F[_]: Concurrent: Sync](C: Client[F], nameMap: ConcurrentHashMap[String, String], cmd: RedisCommands[F, String, String]): Covid[F] = new Covid[F]{
    val dsl: Http4sClientDsl[F] = new Http4sClientDsl[F]{}
    import dsl._
    def getByState(state: String, date: String): F[State] =  for {
      key <- Concurrent[F].delay(s"covState:$state:$date")
      hasKey <- cmd.exists(key)
      stateUri <- Concurrent[F].delay(Uri.unsafeFromString(CovidTrackApiUri.builder(CovidTrackApiUri(state, date))))
      resp <- if (!hasKey) {
        for {
          cdata <- C.expect[State](GET(stateUri)).adaptError { case t => DataError(t) }
          asString <- Concurrent[F].delay(cdata.toString)
          _ <- Concurrent[F].delay(L.info("\"setting key\" key={} value={}", key, asString))
          _ <- cmd.set(key, asString)
        } yield cdata
      } else
        for {
          memValOpt <- cmd.get(key)
          retrieved <- Concurrent[F].delay(memValOpt.map{ memVal =>
            L.info("\"retrieved key\" key={} value={}", key, memVal)
            fromState(memVal)
          }.getOrElse(State("", 0, 0)))
        } yield retrieved
    } yield resp

    override def getByCity(state: String, city: String): F[City] = for {
      stateNameOpt <-  Concurrent[F].delay(if (nameMap.containsKey(state)) Some(nameMap.get(state)) else None)
      resp <- stateNameOpt match {
        case Some(stateName) =>
          for {
            key <- Concurrent[F].delay(s"covCity:$state:$city")
            hasKey <- cmd.exists(key)
            innerResp <- if (!hasKey) {
              for {
                cityUri <-  Concurrent[F].delay(Uri.unsafeFromString(CovidApiUri.builder(CovidApiUri(stateName, city))))
                cdata <- C.expect[City](GET(cityUri)).adaptError { case t => DataError(t) }
                asString <- Concurrent[F].delay(cdata.toString)
                _ <- Concurrent[F].delay(L.info("\"setting key\" key={} value={}", key, asString))
                _ <- cmd.set(key, asString)
              } yield cdata
            } else
              for {
                memValOpt <- cmd.get(key)
                retrieved <- Concurrent[F].delay(memValOpt.map{ memVal =>
                  L.info("\"retrieved key\" key={} value={}", key, memVal)
                  fromCity(memVal)
                }.getOrElse(City("", "", 0, 0)))
              } yield retrieved
          } yield innerResp
        case None           => Concurrent[F].delay(City("", "", 0, 0))
      }
    } yield resp

    override def getByStates(states: List[String], date: String): F[State] = for {
      stateStats <- states.map(state => getByState(state, date)).sequence
      stateTotals <- Concurrent[F].delay(stateStats.tail.foldLeft(stateStats.head){ (acc, elem) =>
        acc.copy(state = s"${acc.state},${elem.state}", positive = acc.positive + elem.positive, death = acc.death + elem.death)
      })
    } yield stateTotals

    override def getByCities(state: String, cities: List[String]): F[City] = for {
      cityStats <- cities.map(city => getByCity(state, city)).sequence
      cityTotals <- Concurrent[F].delay(cityStats.tail.foldLeft(cityStats.head){ (acc, elem) =>
        acc.copy(city = s"${acc.city},${elem.city}", confirmed = acc.confirmed + elem.confirmed, deaths = acc.deaths + elem.deaths)
      })
    } yield cityTotals
  }
}


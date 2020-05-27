package com.iscs.covidbystates.domains

import java.util.concurrent.ConcurrentHashMap

import cats.effect.{Concurrent, Sync}
import cats.implicits._
import com.iscs.covidbystates.covid.{CovidApiUri, CovidTrackApiUri}
import io.circe._
import io.circe.generic.semiauto._
import org.http4s.Method._
import org.http4s.circe._
import org.http4s.client.Client
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s._
import org.http4s.{EntityDecoder, EntityEncoder}

trait Covid[F[_]] {
  def getByState(state: String, date: String): F[Covid.State]
  def getByStates(states: List[String], date: String): F[Covid.State]
  def getByCity(state: String, city: String): F[Covid.City]
  def getByCities(state: String, cities: List[String]): F[Covid.City]
}

object Covid {
  def apply[F[_]](implicit ev: Covid[F]): Covid[F] = ev

  final case class State(state: String, positive: Int, death: Int)
  final case class City(state: String, city: String, confirmed: Int, deaths: Int)

  object State {
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

  def impl[F[_]: Concurrent: Sync](C: Client[F], nameMap: ConcurrentHashMap[String, String]): Covid[F] = new Covid[F]{
    val dsl: Http4sClientDsl[F] = new Http4sClientDsl[F]{}
    import dsl._
    def getByState(state: String, date: String): F[State] = {
      val stateUri = Uri.unsafeFromString(CovidTrackApiUri.builder(CovidTrackApiUri(state, date)))
      C.expect[State](GET(stateUri))
        .adaptError { case t => DataError(t) } // Prevent Client Json Decoding Failure Leaking
    }

    override def getByCity(state: String, city: String): F[City] = {
      val stateName = if (nameMap.containsKey(state)) nameMap.get(state) else "XX"
      val cityUri = Uri.unsafeFromString(CovidApiUri.builder(CovidApiUri(stateName, city)))
      C.expect[City](GET(cityUri))
        .adaptError { case t => DataError(t) } // Prevent Client Json Decoding Failure Leaking
    }

    override def getByStates(states: List[String], date: String): F[State] = for {
      stateStats <- states.map(state => getByState(state, date)).sequence
      stateTotals <- Concurrent[F].delay(stateStats.foldLeft(State("", 0, 0)){ (acc, elem) =>
        acc.copy(state = s"${acc.state},${elem.state}", positive = acc.positive + elem.positive, death = acc.death + elem.death)
      })
    } yield stateTotals

    override def getByCities(state: String, cities: List[String]): F[City] = for {
      cityStats <- cities.map(city => getByCity(state, city)).sequence
      cityTotals <- Concurrent[F].delay(cityStats.foldLeft(City(state, "", 0, 0)){ (acc, elem) =>
        acc.copy(city = s"${acc.city},${elem.city}", confirmed = acc.confirmed + elem.confirmed, deaths = acc.deaths + elem.deaths)
      })
    } yield cityTotals
  }
}


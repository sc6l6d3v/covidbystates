package com.iscs.covidbystates.domains


import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import java.util.concurrent.ConcurrentHashMap

import cats.effect.{Blocker, Concurrent, ConcurrentEffect, ContextShift, Sync}
import cats.implicits._
import com.iscs.covidbystates.covid.{CovidStateHistoryApiUri, CovidUSHistoryApiUri}
import com.iscs.covidbystates.csv.StateHistoryStream
import com.iscs.covidbystates.elect.Political
import com.typesafe.scalalogging.Logger
import dev.profunktor.redis4cats.RedisCommands
import fs2.Stream
import io.circe._
import io.circe.generic.semiauto._
import io.circe.parser._
import io.circe.syntax._
import org.http4s.Method._
import org.http4s.circe._
import org.http4s.client.Client
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.{EntityDecoder, EntityEncoder, _}

trait CovidHistory[F[_]] extends Cache[F] {
  def getHistoryByState(state: String): Stream[F, CovidHistory.State]
  def getHistoryByState2(state: String): Stream[F, List[CovidHistory.State]]
  def getUSHistory: F[CovidHistory.Country]
}

object CovidHistory {
  private val L = Logger[this.type]

  def apply[F[_]](implicit ev: CovidHistory[F]): CovidHistory[F] = ev

  final case class State(state: String, vote: String = "", positive: Int, death: Int, date: ZonedDateTime) {
    //override def toString: String = s"$this"
  }
  final case class Country(name: String, vote: String = "", confirmed: Int, deaths: Int) {
    //override def toString: String =  s"$this"
  }

  object State {
    implicit val stateDecoder: Decoder[State] = (c: HCursor) => {
      val state = c.downField("state").as[String].getOrElse("NOSTATE")
      val positive = c.downField("positive").as[Int].getOrElse(0)
      val death = c.downField("death").as[Int].getOrElse(0)
      val dateStr = c.downField("date").as[String].getOrElse("")
      val formatter = DateTimeFormatter.ISO_OFFSET_DATE_TIME
      val date = ZonedDateTime.parse(dateStr, formatter)
      Right(State(state, positive = positive, death = death, date = date))
    }

    implicit def stateEntityDecoder[F[_]: Sync]: EntityDecoder[F, State] = jsonOf
    implicit val stateEncoder: Encoder[State] = deriveEncoder[State]
    implicit def stateEntityEncoder[F[_]: Sync]: EntityEncoder[F, State] = jsonEncoderOf

    implicit def statesEntityDecoder[F[_]: Sync]: EntityDecoder[F, List[State]] = jsonOf

    def empty: State = State("", "", 0, 0, ZonedDateTime.now)
  }

  object Country {
    implicit val cityDecoder: Decoder[Country] = (c: HCursor) => {
      val topObj = c.downField("data").downArray
      val regionObj = topObj.downField("region")
      val state = regionObj.downField("province").as[String].getOrElse("NOSTATE")
      val city = regionObj.downField("cities").downArray.downField("name").as[String].getOrElse("NOCITY")
      val citiesObj = regionObj.downField("cities").downArray
      val confirmed = citiesObj.downField("confirmed").as[Int].getOrElse(0)
      val deaths = citiesObj.downField("deaths").as[Int].getOrElse(0)
      Right(Country(state, city, confirmed = confirmed, deaths = deaths))
    }

    implicit def countryEntityDecoder[F[_]: Sync]: EntityDecoder[F, Country] = jsonOf
    implicit val countryEncoder: Encoder[Country] = deriveEncoder[Country]
    implicit def countryEntityEncoder[F[_]: Sync]: EntityEncoder[F, Country] = jsonEncoderOf

    def empty: Country = Country("", "", 0, 0)
  }

  final case class DataError(e: Throwable) extends RuntimeException

  def fromCountry(country: String): Country = parse(country).getOrElse(Json.Null).as[Country].getOrElse(Country.empty)

  def fromState(state: String): State = parse(state).getOrElse(Json.Null).as[State].getOrElse(State.empty)

  def impl[F[_]: ConcurrentEffect: Sync](C: Client[F], nameMap: ConcurrentHashMap[String, String],
                                   countyElectMap: ConcurrentHashMap[String, Political],
                                   electMap: ConcurrentHashMap[String, Political])
                                  (implicit cmd: RedisCommands[F, String, String], con: ContextShift[F]): CovidHistory[F] = new CovidHistory[F] {
    val dsl: Http4sClientDsl[F] = new Http4sClientDsl[F] {}
    import dsl._

    override def getHistoryByState(state: String): Stream[F, State] = for {
      key <- Stream.eval(Concurrent[F].delay(s"covStateHx:$state"))
      hasKey <- Stream.eval(cmd.exists(key))
      stateUri <- Stream.eval(Concurrent[F].delay(Uri.unsafeFromString(CovidStateHistoryApiUri.builder(CovidStateHistoryApiUri(state)))))
      resp <- if (!hasKey) {
        for {
          req <- Stream.eval(GET(stateUri).adaptError { case t => DataError(t) })
          hxStream = new StateHistoryStream(req, C)
          cdata <- hxStream.stream(Blocker.liftExecutionContext(scala.concurrent.ExecutionContext.global))
          vote <- Stream.eval(Concurrent[F].delay(if (electMap.containsKey(state)) electMap.get(state).toString else ""))
          cdataWithVote <- Stream.eval(Concurrent[F].delay(cdata.copy(vote = vote)))
          _ <- Stream.eval(setRedisKey(key, cdataWithVote.asJson.toString))
        } yield cdataWithVote
      } else
        Stream.eval(getStateHxFromRedis(key))
    } yield resp

    override def getUSHistory: F[Country] = for {
      key <- Concurrent[F].delay(s"covUSHx")
      hasKey <- cmd.exists(key)
      stateUri <- Concurrent[F].delay(Uri.unsafeFromString(CovidUSHistoryApiUri.toString()))
      resp <- if (!hasKey) {
        for {
          cdata <- C.expect[Country](GET(stateUri)).adaptError { case t => DataError(t) }
          vote <- Concurrent[F].delay("")
          cdataWithVote <- Concurrent[F].delay(cdata.copy(vote = vote))
          _ <- setRedisKey(key, cdataWithVote.toString)
        } yield cdataWithVote
      } else
        getUSHxFromRedis(key)
    } yield resp

    override def getHistoryByState2(state: String): Stream[F,List[State]] = for {
      statesSeq <- getHistoryByState(state).chunkN(20).map(_.toList)
    } yield statesSeq
  }
}

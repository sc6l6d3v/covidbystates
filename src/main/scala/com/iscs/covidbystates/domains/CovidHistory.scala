package com.iscs.covidbystates.domains


import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger

import cats.effect.{Concurrent, ConcurrentEffect, ContextShift, Sync}
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

import scala.util.Try

trait CovidHistory[F[_]] extends Cache[F] {
  def getHistoryByStates(state: String): Stream[F, CovidHistory.States]
  def getUSHistory: F[CovidHistory.Country]
}

object CovidHistory {
  private val L = Logger[this.type]

  def apply[F[_]](implicit ev: CovidHistory[F]): CovidHistory[F] = ev

  final case class State(state: String, vote: String = "", positive: Int, death: Int, date: LocalDate)
  final case class States(seq: List[State])
  final case class Country(name: String, vote: String = "", confirmed: Int, deaths: Int)

  object State {
    implicit val stateDecoder: Decoder[State] = (c: HCursor) => {
      val state = c.downField("state").as[String].getOrElse("NOSTATE")
      val positive = c.downField("positive").as[Int].getOrElse(0)
      val death = c.downField("death").as[Int].getOrElse(0)
      val dateStr = c.downField("date").as[String].getOrElse("")
      val formatter = DateTimeFormatter.ISO_DATE
      val date = Try(LocalDate.parse(dateStr, formatter)).toOption.getOrElse{
        L.error(""""Date not parsed" date={}""", dateStr)
        LocalDate.now
      }
      Right(State(state, positive = positive, death = death, date = date))
    }

    implicit def stateEntityDecoder[F[_]: Sync]: EntityDecoder[F, State] = jsonOf
    implicit val stateEncoder: Encoder[State] = deriveEncoder[State]
    implicit def stateEntityEncoder[F[_]: Sync]: EntityEncoder[F, State] = jsonEncoderOf

    def empty: State = State("", "", 0, 0, LocalDate.now)
  }

  object States {
    implicit val statesDecoder: Decoder[States] = (c: HCursor) => {
      val seq= c.downField("seq").as[List[State]].getOrElse(List.empty[State])
      Right(States(seq))
    }
    implicit def statesEntityDecoder[F[_]: Sync]: EntityDecoder[F, List[State]] = jsonOf
    implicit val statesEncoder: Encoder[States] = deriveEncoder[States]
    implicit def statesEntityEncoder[F[_]: Sync]: EntityEncoder[F, States] = jsonEncoderOf

    def empty: States = States(List.empty[State])
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

  def fromStates(states: String): States = parse(states).getOrElse(Json.Null).as[States].getOrElse(States.empty)

  def impl[F[_]: ConcurrentEffect: Sync](C: Client[F], nameMap: ConcurrentHashMap[String, String],
                                   countyElectMap: ConcurrentHashMap[String, Political],
                                   electMap: ConcurrentHashMap[String, Political])
                                  (implicit cmd: RedisCommands[F, String, String], con: ContextShift[F]): CovidHistory[F] = new CovidHistory[F] {
    val dsl: Http4sClientDsl[F] = new Http4sClientDsl[F] {}
    import dsl._

    def getHistoryByState(state: String): Stream[F, State] = for {
      stateUri <- Stream.eval(Concurrent[F].delay(Uri.unsafeFromString(CovidStateHistoryApiUri.builder(CovidStateHistoryApiUri(state)))))
      req <- Stream.eval(GET(stateUri).adaptError { case t => DataError(t) })
      hxStream = new StateHistoryStream(req, C)
      cdata <- hxStream.stream
      vote <- Stream.eval(Concurrent[F].delay(if (electMap.containsKey(state)) electMap.get(state).toString else ""))
      cdataWithVote <- Stream.eval(Concurrent[F].delay(cdata.copy(vote = vote)))
    } yield cdataWithVote

    override def getUSHistory: F[Country] = for {
      stateUri <- Concurrent[F].delay(Uri.unsafeFromString(CovidUSHistoryApiUri.toString()))
      cdata <- C.expect[Country](GET(stateUri)).adaptError { case t => DataError(t) }
      vote <- Concurrent[F].delay("")
      cdataWithVote <- Concurrent[F].delay(cdata.copy(vote = vote))
    } yield cdataWithVote

    override def getHistoryByStates(state: String): Stream[F,States] = for {
      key <- Stream.eval(Concurrent[F].delay(s"covStateHx:$state"))
      hasKey <- Stream.eval(cmd.exists(key))
      resp <- if (!hasKey) {
        for {
          statesSeq <- Stream.eval(getHistoryByState(state).compile.toList)
          _ <- Stream.eval(setRedisKey(key, States(statesSeq).asJson.noSpaces))
        } yield States(statesSeq)
      } else
        Stream.eval(getStatesHxFromRedis(key))
    } yield resp
  }
}

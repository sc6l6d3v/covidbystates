package com.iscs.covidbystates.domains

import java.util.concurrent.ConcurrentHashMap

import cats.effect.{Concurrent, Sync}
import cats.implicits._
import dev.profunktor.redis4cats.RedisCommands
import io.circe._
import io.circe.generic.semiauto._
import io.circe.parser._
import io.circe.syntax._
import org.http4s.circe._
import org.http4s.{EntityDecoder, EntityEncoder}

trait Groupings[F[_]] extends Cache[F] {
  def getByCounty(state: String): F[Groupings.County]
  def getByState(country: String): F[Groupings.State]
}

object Groupings {
  def apply[F[_]](implicit ev: Groupings[F]): Groupings[F] = ev

  final case class County(state: String, counties: List[String]) {
    override def toString: String = s"$this"
  }
  final case class State(country: String, states: List[String]) {
    override def toString: String = s"$this"
  }

  object County {
    implicit val countyDecoder: Decoder[County] = deriveDecoder[County]
    implicit def countyEntityDecoder[F[_]: Sync]: EntityDecoder[F, County] = jsonOf
    implicit val countyEncoder: Encoder[County] = deriveEncoder[County]
    implicit def countyEntityEncoder[F[_]: Sync]: EntityEncoder[F, County] = jsonEncoderOf
    def empty: County = County("", List.empty[String])
  }

  object State {
    implicit val stateDecoder: Decoder[State] = deriveDecoder[State]
    implicit def stateEntityDecoder[F[_]: Sync]: EntityDecoder[F, State] = jsonOf
    implicit val stateEncoder: Encoder[State] = deriveEncoder[State]
    implicit def stateEntityEncoder[F[_]: Sync]: EntityEncoder[F, State] = jsonEncoderOf

    def empty: State = State("", List.empty[String])
  }

  def fromCounty(county: String): County = parse(county).getOrElse(Json.Null).as[County].getOrElse(County.empty)

  def fromState(state: String): State = parse(state).getOrElse(Json.Null).as[State].getOrElse(State.empty)

  final case class DataError(e: Throwable) extends RuntimeException

  def impl[F[_]: Concurrent](countyMap: ConcurrentHashMap[String, List[String]])(implicit cmd: RedisCommands[F, String, String]): Groupings[F] = new Groupings[F] {
    override def getByCounty(state: String): F[County] = for {
      key <- Concurrent[F].delay(s"group-county-$state")
      hasKey <- cmd.exists(key)
      resp <- if (!hasKey) {
        for {
          countyMap <- Concurrent[F].delay(if (countyMap.containsKey(state))
            countyMap.get(state)
          else
            List.empty[String])
          county <- Concurrent[F].delay(County(state, countyMap))
          _ <- setRedisKey(key, county.asJson.noSpaces)
        } yield county
      } else
        getCountyFromRedis(key)
    } yield resp

    override def getByState(country: String): F[State] = for {
      key <- Concurrent[F].delay(s"group-country-$country")
      hasKey <- cmd.exists(key)
      resp <- if (!hasKey) {
        for {
          state <- Concurrent[F].delay(State(country,
            if (countyMap.containsKey(country))
              countyMap.get(country)
            else
              List.empty[String]
          ))
          _ <- setRedisKey(key, state.asJson.noSpaces)
        } yield state
      } else
        getGrpStateFromRedis(key)
    } yield resp
  }

}




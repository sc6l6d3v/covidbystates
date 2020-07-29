package com.iscs.covidbystates.domains

import cats.effect.{Concurrent, Sync}
import cats.implicits._
import com.iscs.covidbystates.domains.Covid.{City, State, fromCity, fromState}
import com.iscs.covidbystates.domains.CovidHistory.{Country, States, fromCountry, fromStates, State => StateHx, fromState => fromStateHx}
import com.iscs.covidbystates.domains.Census.{Data, fromString}
import com.iscs.covidbystates.domains.Groupings.{County, fromCounty, State => GrpState, fromState => fromGrpState}
import com.typesafe.scalalogging.Logger
import dev.profunktor.redis4cats.RedisCommands
import io.circe.Json
import io.circe.parser._

trait Cache[F[_]] {
  private val L = Logger[this.type]

  def getCityFromRedis[F[_]: Concurrent: Sync](key: String)(implicit cmd: RedisCommands[F, String, String]): F[City] = for {
    memValOpt <- cmd.get(key)
    retrieved <- Concurrent[F].delay(memValOpt.map{ memVal =>
      L.info("\"retrieved key\" key={} value={}", key, memVal)
      fromCity(memVal)
    }.getOrElse(City.empty))
  } yield retrieved

  def getStateFromRedis[F[_]: Concurrent: Sync](key: String)(implicit cmd: RedisCommands[F, String, String]): F[State] = for {
    memValOpt <- cmd.get(key)
    retrieved <- Concurrent[F].delay(memValOpt.map{ memVal =>
      L.info("\"retrieved key\" key={} value={}", key, memVal)
      fromState(memVal)
    }.getOrElse(State.empty))
  } yield retrieved

  def getStateHxFromRedis[F[_]: Concurrent: Sync](key: String)(implicit cmd: RedisCommands[F, String, String]): F[StateHx] = for {
    memValOpt <- cmd.get(key)
    retrieved <- Concurrent[F].delay(memValOpt.map{ memVal =>
      L.info("\"retrieved key\" key={} value={}", key, memVal)
      fromStateHx(memVal)
    }.getOrElse(StateHx.empty))
  } yield retrieved

  def getStatesHxFromRedis[F[_]: Concurrent: Sync](key: String)(implicit cmd: RedisCommands[F, String, String]): F[States] = for {
    memValOpt <- cmd.get(key)
    retrieved <- Concurrent[F].delay(memValOpt.map{ memVal =>
      L.info("\"retrieved key\" key={} value={}", key, memVal)
      fromStates(memVal)
    }.getOrElse(States.empty))
  } yield retrieved

  def getStatesHxJsonFromRedis[F[_]: Concurrent: Sync](key: String)(implicit cmd: RedisCommands[F, String, String]): F[Json] = for {
    memValOpt <- cmd.get(key)
    retrieved <- Concurrent[F].delay(memValOpt.map{ memVal =>
      L.info("\"retrieved key\" key={} value={}", key, memVal)
      Json.fromValues(
        memVal.split("\\|").map { str =>
          parse(str) match {
            case Right(validJson) =>
              validJson //.asArray.getOrElse(Vector.empty[Json]).toList
            case Left(failure) =>
              L.error(""""bad json" json={}""", memVal)
              Json.Null
          }
        })
    }.getOrElse(Json.Null))
  } yield retrieved

  def getUSHxFromRedis[F[_]: Concurrent: Sync](key: String)(implicit cmd: RedisCommands[F, String, String]): F[Country] = for {
    memValOpt <- cmd.get(key)
    retrieved <- Concurrent[F].delay(memValOpt.map{ memVal =>
      L.info("\"retrieved key\" key={} value={}", key, memVal)
      fromCountry(memVal)
    }.getOrElse(Country.empty))
  } yield retrieved

  def getCensusFromRedis[F[_]: Concurrent: Sync](key: String)(implicit cmd: RedisCommands[F, String, String]): F[Data] = for {
    memValOpt <- cmd.get(key)
    retrieved <- Concurrent[F].delay(memValOpt.map{ memVal =>
      L.info("\"retrieved key\" key={} value={}", key, memVal)
      fromString(memVal)
    }.getOrElse(Data(Map.empty[String, String])))
  } yield retrieved

  def getCountyFromRedis[F[_]: Concurrent: Sync](key: String)(implicit cmd: RedisCommands[F, String, String]): F[County] = for {
    memValOpt <- cmd.get(key)
    retrieved <- Concurrent[F].delay(memValOpt.map{ memVal =>
      L.info("\"retrieved key\" key={} value={}", key, memVal)
      fromCounty(memVal)
    }.getOrElse(County("", List.empty[String])))
  } yield retrieved

  def getGrpStateFromRedis[F[_]: Concurrent: Sync](key: String)(implicit cmd: RedisCommands[F, String, String]): F[GrpState] = for {
    memValOpt <- cmd.get(key)
    retrieved <- Concurrent[F].delay(memValOpt.map{ memVal =>
      L.info("\"retrieved key\" key={} value={}", key, memVal)
      fromGrpState(memVal)
    }.getOrElse(GrpState("", List.empty[String])))
  } yield retrieved

  def setRedisKey[F[_]: Concurrent: Sync](key: String, inpValue: String)(implicit cmd: RedisCommands[F, String, String]): F[Unit] = for {
    asString <- Concurrent[F].delay(inpValue)
    _ <- Concurrent[F].delay(L.info("\"setting key\" key={} value={}", key, asString))
    _ <- cmd.set(key, asString)
  } yield ()

}

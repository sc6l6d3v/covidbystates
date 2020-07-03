package com.iscs.covidbystates.domains

import cats.effect.{Concurrent, Sync}
import cats.implicits._
import com.iscs.covidbystates.domains.Covid.{City, State, fromCity, fromState}
import com.iscs.covidbystates.domains.Census.{Data, fromString}
import com.iscs.covidbystates.domains.Groupings.{fromCounty, County, fromState => fromGrpState, State => GrpState}
import com.typesafe.scalalogging.Logger
import dev.profunktor.redis4cats.RedisCommands

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

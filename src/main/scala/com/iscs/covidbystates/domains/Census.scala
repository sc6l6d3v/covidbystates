package com.iscs.covidbystates.domains

import java.util.concurrent.ConcurrentHashMap

import cats.effect.{Concurrent, Sync}
import cats.implicits._
import com.iscs.covidbystates.census.CensusApiUri
import com.typesafe.scalalogging.Logger
import dev.profunktor.redis4cats.RedisCommands
import io.circe.generic.semiauto._
import io.circe.{Decoder, Encoder, HCursor}
import org.http4s.Method._
import org.http4s.circe._
import org.http4s.client.Client
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.{EntityDecoder, EntityEncoder, Uri}

trait Census[F[_]]{
  def get(state: String): F[Census.Data]
}

object Census {
  private val L = Logger[this.type]

  def apply[F[_]](implicit ev: Census[F]): Census[F] = ev

  final case class Data(popMap: Map[String, String]) {
    override def toString: String =
      popMap.map(_.productIterator.mkString(":")).mkString("|")
  }

  object Data {
    implicit val llSDecoder: Decoder[Data] = (c: HCursor) => {
      val arr = c.values.get.toList
      val first = arr(0).asArray.get.map(js => js.asString.get)
      val second = arr(1).asArray.get.map(js => js.asString.get)
      val popMap = (first zip second).toMap
      Right(Data(popMap))
    }

    implicit def llSEntityDecoder[F[_]: Sync]: EntityDecoder[F, Data] =
      jsonOf
    implicit val censusEncoder: Encoder[Data] = deriveEncoder[Data]
    implicit def censusEntityEncoder[F[_]: Sync]: EntityEncoder[F, Data] =
      jsonEncoderOf
  }

  final case class DataError(e: Throwable) extends RuntimeException

  private def fromString(inp: String): Data =
    Data(inp.split("\\|")
      .toList
      .map(_
        .split(":")
        .toList
      )
      .map(ll =>
        (ll.head -> ll.last)
      )
      .toMap)

  def impl[F[_]: Concurrent](C: Client[F], stateMap: ConcurrentHashMap[String, String], cmd: RedisCommands[F, String, String]): Census[F] = new Census[F]{
    val dsl: Http4sClientDsl[F] = new Http4sClientDsl[F]{}
    import dsl._
    def get(state: String): F[Data] = for {
      stateNum <- Concurrent[F].delay(if (stateMap.containsKey(state)) stateMap.get(state) else "36")
      popUri <- Concurrent[F].delay(Uri.unsafeFromString(CensusApiUri.builder(CensusApiUri(stateNum))))
      key <- Concurrent[F].delay(s"census-$state")
      hasKey <- cmd.exists(key)
      resp <- if (!hasKey) {
        val fdata = C.expect[Data](GET(popUri))
          .adaptError { case t => DataError(t) } // Prevent Client Json Decoding Failure Leaking
        fdata.flatMap{data =>
          val asString =  data.toString
          L.info("\"setting key\" key={} value={}", key, asString)
          cmd.set(key, asString)}
        fdata
      } else
        cmd.get(key).flatMap(_.map{memVal =>
          L.info("\"retrieved key\" key={} value={}", key, memVal)
          Concurrent[F].delay(fromString(memVal))}
          .getOrElse(Concurrent[F].delay(Data(Map.empty[String, String]))))
    } yield resp
  }
}
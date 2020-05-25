package com.iscs.covidbystates.domains

import java.util.concurrent.ConcurrentHashMap

import cats.effect.Sync
import cats.implicits._
import com.iscs.covidbystates.census.CensusApiUri
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
  def apply[F[_]](implicit ev: Census[F]): Census[F] = ev

  final case class Data(popMap: Map[String, String])

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

  def impl[F[_]: Sync](C: Client[F], stateMap: ConcurrentHashMap[String, String]): Census[F] = new Census[F]{
    val dsl: Http4sClientDsl[F] = new Http4sClientDsl[F]{}
    import dsl._
    def get(state: String): F[Data] = {

      val stateNum = if (stateMap.containsKey(state)) stateMap.get(state) else "36"
      val popUri = Uri.unsafeFromString(CensusApiUri.builder(CensusApiUri(stateNum)))
      C.expect[Data](GET(popUri))
        .adaptError { case t => DataError(t) } // Prevent Client Json Decoding Failure Leaking
    }
  }
}
package com.iscs.covidbystates.domains

import java.util.concurrent.ConcurrentHashMap

import cats.effect.{Concurrent, Sync}
import io.circe._
import io.circe.generic.semiauto._
import org.http4s.circe._
import org.http4s.{EntityDecoder, EntityEncoder}

trait Groupings[F[_]] {
  def getByCounty(state: String): F[Groupings.County]
  def getByState(country: String): F[Groupings.State]
}

object Groupings {
  def apply[F[_]](implicit ev: Groupings[F]): Groupings[F] = ev

  final case class County(state: String, counties: List[String])
  final case class State(country: String, states: List[String])

  object County {
    implicit val countyDecoder: Decoder[County] = deriveDecoder[County]
    implicit def countyEntityDecoder[F[_]: Sync]: EntityDecoder[F, County] = jsonOf
    implicit val countyEncoder: Encoder[County] = deriveEncoder[County]
    implicit def countyEntityEncoder[F[_]: Sync]: EntityEncoder[F, County] = jsonEncoderOf
  }

  object State {
    implicit val stateDecoder: Decoder[State] = deriveDecoder[State]
    implicit def stateEntityDecoder[F[_]: Sync]: EntityDecoder[F, State] = jsonOf
    implicit val stateEncoder: Encoder[State] = deriveEncoder[State]
    implicit def stateEntityEncoder[F[_]: Sync]: EntityEncoder[F, State] = jsonEncoderOf
  }

  final case class DataError(e: Throwable) extends RuntimeException

  def impl[F[_]: Concurrent](countyMap: ConcurrentHashMap[String, List[String]]): Groupings[F] = new Groupings[F] {
    override def getByCounty(state: String): F[County] = Concurrent[F].delay(County(state,
      if (countyMap.containsKey(state))
        countyMap.get(state)
      else
        List.empty[String]
    ))

    override def getByState(country: String): F[State] = Concurrent[F].delay(State(country,
      if (countyMap.containsKey(country))
        countyMap.get(country)
      else
        List.empty[String]
    ))
  }

}




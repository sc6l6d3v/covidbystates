package com.iscs.covidbystates.routes

import cats.effect.{Concurrent, Sync}
import cats.implicits._
import com.typesafe.scalalogging.Logger
import com.iscs.covidbystates.domains.{Census, Covid, CovidHistory, Groupings}
import fs2.{Chunk, Stream}
import io.circe.Encoder
import io.circe.generic.semiauto._
import org.http4s._
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl

object CovidbystatesRoutes {
  private val L = Logger[this.type]

  implicit val llSEncoder: Encoder[List[List[String]]] = deriveEncoder[List[List[String]]]
  implicit def llSEntityEncoder[F[_]: Sync]: EntityEncoder[F, List[List[String]]] =
    jsonEncoderOf

  def covidStateRoutes[F[_]: Sync](C: Covid[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F]{}
    import dsl._
    HttpRoutes.of[F] {
      case GET -> Root / "covidState" / state / date =>
        for {
          grid <- C.getByState(state.toLowerCase, date)
          resp <- Ok(grid)
        } yield resp
      case GET -> Root / "covidStates" / states / date =>
        for {
          grid <- C.getByStates(states.split(",").toList.map(s => s.toLowerCase), date)
          resp <- Ok(grid)
        } yield resp
      case GET -> Root / "covidCounties" / state / counties =>
        for {
          grid <- C.getByCities(state, counties.split(",").toList)
          resp <- Ok(grid)
        } yield resp
    }
  }

  def covidHistoryRoutes[F[_]: Sync: Concurrent](C: CovidHistory[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F]{}
    import dsl._
    HttpRoutes.of[F] {
      case GET -> Root / "covidStateHistory" / state =>
        for {
          resp <- Ok(C.getHistoryByStates(state.toLowerCase))
        } yield resp
      case GET -> Root / "covidUSHistory"  =>
        for {
          grid <- C.getUSHistory
          resp <- Ok(grid)
        } yield resp
    }
  }

  def covidCityRoutes[F[_]: Sync](C: Covid[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F]{}
    import dsl._
    HttpRoutes.of[F] {
      case GET -> Root / "covidCity" / state / city =>
        for {
          grid <- C.getByCity(state, city)
          resp <- Ok(grid)
        } yield resp
    }
  }

  def censusRoutes[F[_]: Sync](C: Census[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F]{}
    import dsl._
    HttpRoutes.of[F] {
      case GET -> Root / "census" / state =>
        for {
          grid <- C.get(state)
          resp <- Ok(grid)
        } yield resp
    }
  }

  def groupingsRoutes[F[_]: Sync](G: Groupings[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F]{}
    import dsl._
    HttpRoutes.of[F] {
      case GET -> Root / "counties" / state =>
        for {
          grid <- G.getByCounty(state)
          resp <- Ok(grid)
        } yield resp
      case GET -> Root / "states" / country =>
        for {
          grid <- G.getByState(country)
          resp <- Ok(grid)
        } yield resp
    }
  }
}

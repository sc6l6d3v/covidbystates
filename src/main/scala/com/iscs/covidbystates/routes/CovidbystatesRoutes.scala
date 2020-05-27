package com.iscs.covidbystates.routes

import cats.effect.Sync
import cats.implicits._
import com.iscs.covidbystates.domains.{Census, Covid, Groupings}
import com.iscs.covidbystates.{HelloWorld, Jokes}
import io.circe.Encoder
import io.circe.generic.semiauto._
import org.http4s.{EntityEncoder, HttpRoutes}
import org.http4s.dsl.Http4sDsl
import org.http4s.circe._

object CovidbystatesRoutes {
  implicit val llSEncoder: Encoder[List[List[String]]] = deriveEncoder[List[List[String]]]
  implicit def llSEntityEncoder[F[_]: Sync]: EntityEncoder[F, List[List[String]]] =
    jsonEncoderOf

  def covidStateRoutes[F[_]: Sync](C: Covid[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F]{}
    import dsl._
    HttpRoutes.of[F] {
      case GET -> Root / "covidState" / state / date =>
        for {
          grid <- C.getByState(state, date)
          resp <- Ok(grid)
        } yield resp
      case GET -> Root / "covidStates" / states / date =>
        for {
          grid <- C.getByStates(states.split(",").toList, date)
          resp <- Ok(grid)
        } yield resp
      case GET -> Root / "covidCounties" / state / counties =>
        for {
          grid <- C.getByCities(state, counties.split(",").toList)
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


  def jokeRoutes[F[_]: Sync](J: Jokes[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F]{}
    import dsl._
    HttpRoutes.of[F] {
      case GET -> Root / "joke" =>
        for {
          joke <- J.get
          resp <- Ok(joke)
        } yield resp
    }
  }

  def helloWorldRoutes[F[_]: Sync](H: HelloWorld[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F]{}
    import dsl._
    HttpRoutes.of[F] {
      case GET -> Root / "hello" / name =>
        for {
          greeting <- H.hello(HelloWorld.Name(name))
          resp <- Ok(greeting)
        } yield resp
    }
  }
}

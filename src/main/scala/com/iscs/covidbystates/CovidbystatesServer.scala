package com.iscs.covidbystates

import java.util.concurrent.ConcurrentHashMap

import cats.effect.{Blocker, Concurrent, ConcurrentEffect, ContextShift, IO, LiftIO, Sync, Timer}
import cats.implicits._
import com.iscs.covidbystates.domains.{Census, Covid, Groupings}
import com.iscs.covidbystates.routes.CovidbystatesRoutes
import com.iscs.covidbystates.util.ResourceProcessor
import fs2.Stream
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.middleware.{Logger => hpLogger}
import com.typesafe.scalalogging.Logger

import scala.concurrent.ExecutionContext.global

object CovidbystatesServer {

  private val stateCodeMap = new ConcurrentHashMap[String, String]()
  private val stateNameMap = new ConcurrentHashMap[String, String]()
  private val stateNSMap = new ConcurrentHashMap[String, String]()
  private val countyStateMap = new ConcurrentHashMap[String, String]()
  private val stateCountyMap = new ConcurrentHashMap[String, List[String]]()

  private val delim = "\\|"
  private val delim2 = ","
  private val lineFeed = "\n"

  private val L = Logger[this.type]

  def getResource[F[_]: Sync: ContextShift: Timer: ConcurrentEffect](resName: String, blocker: Blocker): F[String] = for {
    resProc <- Concurrent[F].delay(new ResourceProcessor(resName))
    fipsCSV <- resProc.readLinesFromFile(blocker)
    _ <- Concurrent[F].delay(L.info("\"getting resource file\" file={} contents={}", resName, fipsCSV))
  } yield fipsCSV


  def stream[F[_]: ConcurrentEffect](stateCSV: String, countyCSV: String)(implicit T: Timer[F], C: ContextShift[F]): Stream[F, Nothing] = Stream.resource(Blocker[F]).flatMap { blocker =>

    val csvStream = for {
      _ <- Stream.eval(Concurrent[F].delay(L.info("\"got resource file\" contents={}", stateCSV)))
      lines <- Stream.emits(stateCSV.split(lineFeed).toList)
        .drop(1)
      parts <- Stream.eval(Concurrent[F].delay(lines.split(delim).toList))
      _ <- Stream.eval(Concurrent[F].delay{
        stateCodeMap.put(parts(1), parts.head)
        stateNameMap.put(parts(1), parts(2))
        stateNSMap.put(parts(1), parts(3))
      })
    } yield ()

    val countyCsvStream = for {
      _ <- Stream.eval(Concurrent[F].delay(L.info("\"got resource file\" contents={}", countyCSV)))
      lines <- Stream.emits(countyCSV.split(lineFeed).toList)
        .drop(1)
      parts <- Stream.eval(Concurrent[F].delay(lines.split(delim2).toList))
      _ <- Stream.eval(Concurrent[F].delay{
        {
          val intlOffset: Int = if (parts(3) == "US") 0 else 1
          countyStateMap.put(parts(1 + intlOffset), parts(2 + intlOffset))
          if (stateCountyMap.containsKey(parts(2 + intlOffset))) {
            val counties = stateCountyMap.get(parts(2 + intlOffset))
            stateCountyMap.put(parts(2 + intlOffset), counties :+ parts(1 + intlOffset))
          } else {
            stateCountyMap.put(parts(2 + intlOffset), List(parts(1 + intlOffset)))
          }
        }})
    } yield ()

    val srvStream = for {
      client <- BlazeClientBuilder[F](global).stream
      helloWorldAlg = HelloWorld.impl[F]
      jokeAlg = Jokes.impl[F](client)
      censusAlg = Census.impl[F](client, stateCodeMap)
      groupingsAlg = Groupings.impl[F](stateCountyMap)
      covidAlg = Covid.impl[F](client, stateNameMap)

      // Combine Service Routes into an HttpApp.
      // Can also be done via a Router if you
      // want to extract a segments not checked
      // in the underlying routes.
      httpApp = (
        CovidbystatesRoutes.helloWorldRoutes[F](helloWorldAlg) <+>
          CovidbystatesRoutes.jokeRoutes[F](jokeAlg) <+>
          CovidbystatesRoutes.censusRoutes[F](censusAlg) <+>
          CovidbystatesRoutes.covidStateRoutes[F](covidAlg) <+>
          CovidbystatesRoutes.covidCityRoutes[F](covidAlg) <+>
          CovidbystatesRoutes.groupingsRoutes[F](groupingsAlg)
      ).orNotFound

      // With Middlewares in place
      finalHttpApp = hpLogger.httpApp(logHeaders = true, logBody = true)(httpApp)

      exitCode <- BlazeServerBuilder[F]
        .bindHttp(8080, "0.0.0.0")
        .withHttpApp(finalHttpApp)
        .serve
    } yield exitCode
    val stream2 = (csvStream ++ countyCsvStream ++ srvStream).drain
    stream2
  }
}
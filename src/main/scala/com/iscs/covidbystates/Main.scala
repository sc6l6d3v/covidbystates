package com.iscs.covidbystates

import cats.effect.{Blocker, Concurrent, ExitCode, IO, IOApp}
import cats.implicits._
import fs2.Stream
import com.typesafe.scalalogging.Logger

object Main extends IOApp {
  private val L = Logger[this.type]
  private val fipsCSV = "fips.csv"
  private val covidCSV = "covid-raw-2020-05-23.csv"

  def run(args: List[String]): IO[ExitCode] = {

    val serverStream = for {
      resCSV <- Stream.eval(Blocker[IO].use { blocker =>
        CovidbystatesServer.getResource[IO](fipsCSV,blocker)
      }).handleErrorWith { ex =>
        L.error("\"could not read {}\" ex={}", fipsCSV, ex.toString)
        Stream.eval(Concurrent[IO].pure("STATE|STUSAB|STATE_NAME|STATENS\n12|FL|Florida|00294478"))
      }
      countyCSV <- Stream.eval(Blocker[IO].use { blocker =>
        CovidbystatesServer.getResource[IO](covidCSV, blocker)
      }).handleErrorWith { ex =>
        L.error("\"could not read {}\" ex={}", covidCSV, ex.toString)
        Stream.eval(Concurrent[IO].pure("STATE|STUSAB|STATE_NAME|STATENS\n12|FL|Florida|00294478"))
      }
      str <- CovidbystatesServer.stream[IO](resCSV, countyCSV)
    } yield str

    serverStream
      .compile.drain.as(ExitCode.Success)
      .handleErrorWith(ex => IO {
        L.error("\"exception during stream startup\" exception={} ex={}", ex.toString, ex)
        ExitCode.Error
      })
  }
}
package com.iscs.covidbystates

import cats.effect.{Blocker, Concurrent, ExitCode, IO, IOApp, Resource}
import cats.implicits._
import dev.profunktor.redis4cats.{Redis, RedisCommands}
import dev.profunktor.redis4cats.effect.Log.Stdout._
import fs2.Stream
import com.typesafe.scalalogging.Logger
import dev.profunktor.redis4cats.connection.{RedisClient, RedisURI}
import dev.profunktor.redis4cats.data.RedisCodec
import org.http4s.Uri

object Main extends IOApp {
  private val L = Logger[this.type]
  private val fipsCSV = "fips.csv"
  private val covidCSV = "covid-raw-2020-05-23.csv"
  private val electoralCSV = "electoral.csv"
  private val winnerCSV = "winner.csv"
  private val pwd = Uri.encode(sys.env.getOrElse("REDISKEY", "NOREDISKEY")).replace("@","%40")

  def run(args: List[String]): IO[ExitCode] = {
    val redis: Resource[IO, RedisCommands[IO, String, String]] =
      for {
        uri <- Resource.liftF(RedisURI.make[IO](s"redis://$pwd@localhost"))
        cli <- RedisClient[IO](uri)
        cmd <- Redis[IO].fromClient(cli, RedisCodec.Utf8)
      } yield cmd

    redis.use { cmd =>

      val serverStream = for {
        resCSV <- Stream.eval(Blocker[IO].use { blocker =>
          CovidbystatesServer.getResource[IO](fipsCSV, blocker)
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
        electCSV <- Stream.eval(Blocker[IO].use { blocker =>
          CovidbystatesServer.getResource[IO](electoralCSV, blocker)
        }).handleErrorWith { ex =>
          L.error("\"could not read {}\" ex={}", electoralCSV, ex.toString)
          Stream.eval(Concurrent[IO].pure("STATE|STUSAB|STATE_NAME|STATENS\n12|FL|Florida|00294478"))
        }
        countyWinnerCSV <- Stream.eval(Blocker[IO].use { blocker =>
          CovidbystatesServer.getResource[IO](winnerCSV, blocker)
        }).handleErrorWith { ex =>
          L.error("\"could not read {}\" ex={}", winnerCSV, ex.toString)
          Stream.eval(Concurrent[IO].pure("STATE|STUSAB|STATE_NAME|STATENS\n12|FL|Florida|00294478"))
        }
        str <- CovidbystatesServer.stream[IO](resCSV, countyCSV, electCSV, countyWinnerCSV, cmd)
      } yield str

      serverStream
        .compile.drain.as(ExitCode.Success)
        .handleErrorWith(ex => IO {
          L.error("\"exception during stream startup\" exception={} ex={}", ex.toString, ex)
          ExitCode.Error
        })
    }
  }
}
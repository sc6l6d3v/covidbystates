package com.iscs.covidbystates

import cats.effect.{Blocker, Concurrent, ExitCode, IO, IOApp, Resource}
import cats.implicits._
import com.iscs.covidbystates.util.Mongo
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
  private val redisHost = sys.env.getOrElse("REDISHOST", "localhost")
  private val pwd = Uri.encode(sys.env.getOrElse("REDISKEY", "NOREDISKEY")).replace("@", "%40")
  private val mongoUri = sys.env.getOrElse("MONGOURI", "mongodb://localhost:27017")

  def run(args: List[String]): IO[ExitCode] = for {
    start <- IO.delay(System.currentTimeMillis)
    resources = for {
      redis <- for {
          uri <- Resource.liftF {
            L.info("\"starting Redis\" host={}", redisHost)
            RedisURI.make[IO](s"redis://$pwd@$redisHost")
          }
          cli <- RedisClient[IO](uri)
          cmd <- Redis[IO].fromClient(cli, RedisCodec.Utf8)
        } yield cmd
      mongo <- Mongo.fromUrl[IO](mongoUri)
    } yield (redis, mongo)

    ec <- resources.use { case (cmd, mongo) =>
      implicit val redisCmd: RedisCommands[IO, String, String] = cmd
      for {
        start <- IO.delay(System.currentTimeMillis)

        serverStream = for {
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
          str <- CovidbystatesServer.stream[IO](resCSV, countyCSV, electCSV, countyWinnerCSV)
        } yield str

        s <- serverStream
          .compile.drain.as(ExitCode.Success)
          .handleErrorWith(ex => IO {
            L.error("\"exception during stream startup\" exception={} ex={}", ex.toString, ex)
            ExitCode.Error
          })
      } yield s
    }
  } yield ec
}
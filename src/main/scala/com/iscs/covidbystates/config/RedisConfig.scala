package com.iscs.covidbystates.config

import cats.effect.{IO, Resource}
import dev.profunktor.redis4cats.connection.RedisURI
import org.http4s.Uri

case class RedisConfig(redisHost: String, pwd: String, database: Option[String] = None) {
  val uri = Resource.liftF(RedisURI.make[IO](s"redis://$pwd@$redisHost"))
}

object RedisConfig {
  def apply(): RedisConfig = {
    val redisHost = sys.env.getOrElse("REDISHOST", "localhost")
    val pwd = Uri.encode(sys.env.getOrElse("REDISKEY", "NOREDISKEY")).replace("@", "%40")
    RedisConfig(redisHost, pwd)
  }
}



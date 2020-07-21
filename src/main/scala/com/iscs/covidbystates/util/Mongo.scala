package com.iscs.covidbystates.util

import org.mongodb.scala.MongoClientSettings
import cats.effect.{Resource, Sync}
import com.iscs.covidbystates.config.MongodbConfig
import org.mongodb.scala.MongoClient

object Mongo {
  def fromUrl[F[_]]()(implicit F: Sync[F]): Resource[F, MongoClient] =
    Resource.make(F.delay(MongoClient(MongodbConfig().client))) { client =>
      F.delay(client.close())
    }

  def fromSettings[F[_]](settings: MongoClientSettings)(
    implicit F: Sync[F]): Resource[F, MongoClient] = {
    Resource.make(F.delay(MongoClient(settings)))(client =>
      F.delay(client.close()))
  }
}

package com.iscs.covidbystates.util

import cats.effect.{ConcurrentEffect, Effect}
import cats.effect.implicits._
import com.iscs.covidbystates.config.MongodbConfig
import org.mongodb.scala.bson.Document
import org.mongodb.scala.{MongoClient, MongoCollection, MongoDatabase}

case class DbClient[F[_]: Effect](config: MongodbConfig, dbName: String, collNames: List[String])(implicit E: Effect[F]) extends AutoCloseable {
  val client = MongoClient(config.settings)

  val db: MongoDatabase = client.getDatabase(dbName)

  val collectionMap = collNames.map { name =>
    (name -> db.getCollection(name))
  }.toMap

  def dbFX(collection: MongoCollection[Document]) = new MongoCollectionEffect[Document](collection)

  val fxMap =
    collNames.map { name =>
      (name -> dbFX(collectionMap(name)))
    }.toMap

  override def close(): Unit = client.close()
}

object DbClient {
  def apply[F[_]](client: MongodbConfig, names: List[String])
                             (implicit E: ConcurrentEffect[F]): F[DbClient[F]] =
    E.delay(DbClient(client, names.head, names.tail))
}

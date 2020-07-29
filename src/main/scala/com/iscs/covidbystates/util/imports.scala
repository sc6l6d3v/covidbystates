package com.iscs.covidbystates.util

import cats.implicits._
import cats.effect.implicits._
import cats.effect.{ConcurrentEffect, IO}
import com.mongodb.client.model.WriteModel
import com.mongodb.client.result.UpdateResult
import fs2.concurrent.Queue
import fs2._
import org.bson.conversions.Bson
import org.mongodb.scala.result.{DeleteResult, InsertManyResult, InsertOneResult}
import org.mongodb.scala.{BulkWriteResult, MongoCollection, Observable}

import scala.reflect.ClassTag

object imports {
  final val Mongo = com.iscs.covidbystates.util.Mongo

  def toAsync[F[_]: ConcurrentEffect, T](obs: Observable[T]): F[Option[T]] =
    ConcurrentEffect[F].async[Option[T]] { (cb: Either[Throwable, Option[T]] => Unit) =>
      obs.subscribe(
        (result: T) => cb(Right(result.some)),
        (ex: Throwable) => cb(Left(ex)),
        () => cb(Option.empty[T].asRight))
    }

  def toStream[F[_]: ConcurrentEffect, T](observ:Observable[T]): Stream[F, T] =
    for {
      que <- Stream.eval(Queue.noneTerminated[F,Either[Throwable, T]])
      _ <- Stream.eval{
        ConcurrentEffect[F].delay(observ.subscribe(
          (next: T) =>
            que.enqueue1(Right(next).some)
              .runAsync(_ => IO.unit).unsafeRunSync(),
          (ex: Throwable) =>
            que.enqueue1(Left(ex).some)
              .runAsync(_ => IO.unit).unsafeRunSync(),
          () =>
            que.enqueue1(Option.empty[Either[Throwable, T]])
              .runAsync(_ => IO.unit).unsafeRunSync()
        ))
      }
      row <- que.dequeue.rethrow
    } yield row
}

class MongoCollectionEffect[A](val underlying: MongoCollection[A])(implicit c: ClassTag[A]) {
  private val batchsize = 1000

  import imports._

  def bulkWrite[F[_]: ConcurrentEffect](requests: Seq[WriteModel[A]]): F[Option[BulkWriteResult]] =
    toAsync(underlying.bulkWrite(requests))

  def distinct[F[_]: ConcurrentEffect](key: String): Stream[F,A] = toStream(underlying.distinct(key))

  def findOne[F[_]: ConcurrentEffect](filter: Bson): F[Option[A]] = toAsync(underlying.find(filter)
    .limit(1).first())

  def find[F[_]: ConcurrentEffect](filter: Bson): Stream[F,A] =
    toStream(underlying.find(filter).batchSize(batchsize))

  def find[F[_]: ConcurrentEffect](filter: Bson, limit: Int, offset: Int): Stream[F,A] =
    toStream(underlying.find(filter).skip(offset).limit(limit))

  def count[F[_]: ConcurrentEffect]: F[Option[Long]] = toAsync(underlying.countDocuments())

  def count[F[_]: ConcurrentEffect](filter: Bson): F[Option[Long]] = toAsync(underlying.countDocuments(filter))

  def insertOne[F[_]: ConcurrentEffect](document: A): F[Option[InsertOneResult]] =
    toAsync(underlying.insertOne(document))

  def insertMany[F[_]: ConcurrentEffect](documents: Seq[A]): F[Option[InsertManyResult]] =
    toAsync(underlying.insertMany(documents))

  def removeOne[F[_]: ConcurrentEffect](filter: Bson): F[Option[DeleteResult]] =
    toAsync(underlying.deleteOne(filter))

  def removeMany[F[_]: ConcurrentEffect](filter: Bson): F[Option[DeleteResult]] =
    toAsync(underlying.deleteMany(filter))

  def updateOne[F[_]: ConcurrentEffect](filter: Bson, update: Bson): F[Option[UpdateResult]] =
    toAsync(underlying.updateOne(filter, update))

  def updateMany[F[_]: ConcurrentEffect](filter: Bson, update: Bson): F[Option[UpdateResult]] =
    toAsync(underlying.updateMany(filter, update))
}


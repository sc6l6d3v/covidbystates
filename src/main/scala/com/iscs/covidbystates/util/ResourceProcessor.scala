package com.iscs.covidbystates.util

import cats.effect.{Blocker, ConcurrentEffect, ContextShift, Resource, Sync, Timer}

import scala.io.{BufferedSource, Source}

class ResourceProcessor[F[_]: ConcurrentEffect](resourceName: String)(implicit CS: ContextShift[F], S: Sync[F], T: Timer[F]) {

  private def readAllLines(bufferedReader: BufferedSource, blocker: Blocker): F[String] =
    blocker.delay[F, String] {
      bufferedReader.getLines().toList.mkString("\n")
    }

  private def reader(file: String): Resource[F, BufferedSource] =
    Resource.fromAutoCloseable(S.delay { Source.fromResource(file) })

  def readLinesFromFile(blocker: Blocker): F[String] =
    reader(resourceName: String).use(br => readAllLines(br, blocker))
}

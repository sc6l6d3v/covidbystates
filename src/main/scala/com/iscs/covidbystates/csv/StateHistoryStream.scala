package com.iscs.covidbystates.csv

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.util.concurrent.Executors

import com.typesafe.scalalogging.Logger
import cats.effect.{Blocker, ConcurrentEffect, ContextShift}
import com.iscs.covidbystates.domains.CovidHistory.State
import fs2.text.utf8Decode
import fs2.{Pipe, Stream, text}
import org.http4s.Request
import org.http4s.client.Client

import scala.util.Try

class StateHistoryStream[F[_]: ConcurrentEffect : ContextShift](req: Request[F], C: Client[F]) {
  private val L = Logger[this.type]

  implicit val blocker = Blocker.liftExecutorService(Executors.newFixedThreadPool(4))

  def line2state(): Pipe[F, String, State] =
    _.map(s => {
      val parts = s.split(",").toList
      val formatter = DateTimeFormatter.BASIC_ISO_DATE
      val parsed = Try(LocalDate.parse(parts.head, formatter)).toOption.getOrElse{
        L.error(""""Could not parse time" time="{}"""", parts.head)
        LocalDate.now
      }
      val positive = Try(parts(2).toInt).toOption.getOrElse{
        L.error(""""Could not parse positive" val={}""", parts(2))
        0
      }
      val death = Try(parts(16).toInt).toOption.getOrElse{
        L.error(""""Could not parse death" val="{}"""", parts(2))
        0
      }
      State(parts(1), "", positive, death, parsed)
    })

  def stream: Stream[F, State] = for {
    csvStream <- C.stream(req).flatMap(_.body
      .through(utf8Decode)
      .through(text.lines)
      .drop(1)
      .filter(_.split(",").size == 40)
      .through(line2state)
    )
  } yield csvStream
}

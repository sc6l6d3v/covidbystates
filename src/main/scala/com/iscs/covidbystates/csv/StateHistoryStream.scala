package com.iscs.covidbystates.csv

import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter

import cats.effect.{Blocker, ConcurrentEffect, ContextShift}
import com.iscs.covidbystates.domains.CovidHistory.State
import fs2.text.utf8Decode
import fs2.{Pipe, Stream, text}
import org.http4s.Request
import org.http4s.client.Client

class StateHistoryStream[F[_]: ConcurrentEffect : ContextShift](req: Request[F], C: Client[F]) {

  def line2state(): Pipe[F, String, State] =
    _.map(s => {
      val parts = s.split(",").toList
      val formatter = DateTimeFormatter.ISO_OFFSET_DATE_TIME
      val parsed = ZonedDateTime.parse(parts(14), formatter)
      State(parts(1), "", parts(2).toInt, parts(16).toInt, parsed)
    })

  def stream(blocker: Blocker): Stream[F, State] = for {
    csvStream <- C.stream(req).flatMap(_.body
      .through(utf8Decode)
      .through(text.lines)
      .drop(1)
      .through(line2state)
    )
  } yield csvStream
}

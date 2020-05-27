package com.iscs.covidbystates.covid

import java.time._
import java.time.format.DateTimeFormatter

import scala.util.Try

case class CovidTrackApiUri(base: String, path: String, state: String, date: String = LocalDate.now().format(DateTimeFormatter.ISO_LOCAL_DATE)) {
  def withBase(newBase: String): CovidTrackApiUri = copy(base = newBase)

  def withPath(newPath: String): CovidTrackApiUri = copy(path = newPath)

  def withState(newState: String): CovidTrackApiUri = copy(state = newState)

  def blank = CovidTrackApiUri(base = "", path = "", state =  "")
}

object CovidTrackApiUri {
  val baseUri = "https://covidtracking.com"
  val basePath = "/api/v1/states"

  def apply(inState: String, dt: String): CovidTrackApiUri =
    CovidTrackApiUri(base = baseUri, path = basePath, state = inState, date = dt)

  def builder(uri: CovidTrackApiUri): String = {
    val validDate = Try(LocalDate.parse(uri.date)).isSuccess
    val checkedDate = if (validDate) uri.date else LocalDate.now().format(DateTimeFormatter.ISO_LOCAL_DATE)
    s"${uri.base}${uri.path}/${uri.state}/${checkedDate.replaceAll("-","")}.json"
  }
}




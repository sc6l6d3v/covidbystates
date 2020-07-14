package com.iscs.covidbystates.covid

case object CovidUSHistoryApiUri {
  val uri = "https://covidtracking.com/api/v1/states/daily.csv"
  override def toString(): String = uri
}

case class CovidStateHistoryApiUri(base: String, path: String, state: String) {
  def withBase(newBase: String): CovidStateHistoryApiUri = copy(base = newBase)

  def withPath(newPath: String): CovidStateHistoryApiUri = copy(path = newPath)

  def withState(newState: String): CovidStateHistoryApiUri = copy(state = newState)

  def blank = CovidStateHistoryApiUri(base = "", path = "", state =  "")
}

object CovidStateHistoryApiUri {
  val baseUri = "https://covidtracking.com"
  val basePath = "/api/v1/states"

  def apply(inState: String): CovidStateHistoryApiUri = CovidStateHistoryApiUri(base = baseUri, path = basePath,
    state = inState)

  def builder(uri: CovidStateHistoryApiUri): String = s"${uri.base}${uri.path}/${uri.state}/daily.csv"
}











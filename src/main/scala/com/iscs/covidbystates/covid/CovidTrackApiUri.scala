package com.iscs.covidbystates.covid

case class CovidTrackApiUri(base: String, path: String, state: String) {
  def withBase(newBase: String): CovidTrackApiUri = copy(base = newBase)

  def withPath(newPath: String): CovidTrackApiUri = copy(path = newPath)

  def withState(newState: String): CovidTrackApiUri = copy(state = newState)

  def blank = CovidTrackApiUri(base = "", path = "", state =  "")
}

object CovidTrackApiUri {
  val baseUri = "https://covidtracking.com"
  val basePath = "/api/v1/states"

  def apply(inState: String): CovidTrackApiUri =
    CovidTrackApiUri(base = baseUri, path = basePath, state = inState)

  def builder(uri: CovidTrackApiUri): String = s"${uri.base}${uri.path}/${uri.state}/current.json"
}




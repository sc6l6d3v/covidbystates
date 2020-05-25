package com.iscs.covidbystates.census

case class CensusApiUri(base: String, path: String, state: String) {
  def withBase(newBase: String): CensusApiUri = copy(base = newBase)

  def withPath(newPath: String): CensusApiUri = copy(path = newPath)

  def withState(newState: String): CensusApiUri = copy(state = newState)

  def blank = CensusApiUri(base = "", path = "", state =  "")
}

object CensusApiUri {
  val baseUri = "https://api.census.gov"
  val basePath = "/data/2019/pep/population?get=POP,NAME,DENSITY"
  val key = "ee0fbe76aa7bb2e1dd36228a5582b009de649014"

  def apply(inState: String): CensusApiUri =
    CensusApiUri(base = baseUri, path = basePath, state = inState)

  def builder(uri: CensusApiUri): String = s"${uri.base}${uri.path}&for=state:${uri.state}&key=$key"

}







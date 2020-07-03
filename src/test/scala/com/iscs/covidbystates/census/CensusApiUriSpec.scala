package com.iscs.covidbystates.census

import org.specs2.matcher.MatchResult

class CensusApiUriSpec extends org.specs2.mutable.Specification {

  val key = sys.env.get("CENSUSKEY").getOrElse("NOCENSUSKEY")

  private[this] def baseCheck(): MatchResult[String] =
    CensusApiUri.builder(CensusApiUri("NY")) must beEqualTo(s"https://api.census.gov/data/2019/pep/population?get=POP,NAME,DENSITY&for=state:NY&key=$key")

  private[this] def withBaseCheck(): MatchResult[String] =
    CensusApiUri.builder(CensusApiUri("NY").withBase("foo")) must beEqualTo(s"foo/data/2019/pep/population?get=POP,NAME,DENSITY&for=state:NY&key=$key")

  private[this] def withPathCheck(): MatchResult[String] =
    CensusApiUri.builder(CensusApiUri("NY").withPath("/foo")) must beEqualTo(s"https://api.census.gov/foo&for=state:NY&key=$key")

  private[this] def withStateCheck(): MatchResult[String] =
    CensusApiUri.builder(CensusApiUri("").withState("GA")) must beEqualTo(s"https://api.census.gov/data/2019/pep/population?get=POP,NAME,DENSITY&for=state:GA&key=$key")

  private[this] def asBlankCheck(): MatchResult[String] =
    CensusApiUri.builder(CensusApiUri("").blank) must beEqualTo(s"&for=state:&key=$key")

  "CensusApiUri" >> {
    "base checks" >> {
      baseCheck()
    }
    "withBase override" >> {
      withBaseCheck()
    }
    "withPath override" >> {
      withPathCheck()
    }
    "withState override" >> {
      withStateCheck()
    }
    "asBlank check" >> {
      asBlankCheck()
    }
  }

}

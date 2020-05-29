package com.iscs.covidbystates.census

import org.specs2.matcher.MatchResult

class CensusApiUriSpec extends org.specs2.mutable.Specification {

  private[this] def baseCheck(): MatchResult[String] =
    CensusApiUri.builder(CensusApiUri("NY")) must beEqualTo("https://api.census.gov/data/2019/pep/population?get=POP,NAME,DENSITY&for=state:NY&key=NOKEYFOUND")

  private[this] def withBaseCheck(): MatchResult[String] =
    CensusApiUri.builder(CensusApiUri("NY").withBase("foo")) must beEqualTo("foo/data/2019/pep/population?get=POP,NAME,DENSITY&for=state:NY&key=NOKEYFOUND")

  private[this] def withPathCheck(): MatchResult[String] =
    CensusApiUri.builder(CensusApiUri("NY").withPath("/foo")) must beEqualTo("https://api.census.gov/foo&for=state:NY&key=NOKEYFOUND")

  private[this] def withStateCheck(): MatchResult[String] =
    CensusApiUri.builder(CensusApiUri("").withState("GA")) must beEqualTo("https://api.census.gov/data/2019/pep/population?get=POP,NAME,DENSITY&for=state:GA&key=NOKEYFOUND")

  private[this] def asBlankCheck(): MatchResult[String] =
    CensusApiUri.builder(CensusApiUri("").blank) must beEqualTo("&for=state:&key=NOKEYFOUND")

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

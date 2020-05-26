package com.iscs.covidbystates.covid

import org.specs2.matcher.MatchResult

class CovidTrackApiUriSpec extends org.specs2.mutable.Specification {

  def baseCheck(): MatchResult[String] =
    CovidTrackApiUri.builder(CovidTrackApiUri("NY")) must beEqualTo("https://covidtracking.com/api/v1/states/NY/current.json")

  def withPathCheck(): MatchResult[String] =
    CovidTrackApiUri.builder(CovidTrackApiUri("NY").withPath("/path")) must beEqualTo("https://covidtracking.com/path/NY/current.json")


  def withStateCheck(): MatchResult[String] =
    CovidTrackApiUri.builder(CovidTrackApiUri("NY").withState("FL")) must beEqualTo("https://covidtracking.com/api/v1/states/FL/current.json")


  def asBlankCheck(): MatchResult[String] =
    CovidTrackApiUri.builder(CovidTrackApiUri("").blank) must beEqualTo("//current.json")


  "CovidTrackApiURI" >> {
    "base checks" >> {
      baseCheck()
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

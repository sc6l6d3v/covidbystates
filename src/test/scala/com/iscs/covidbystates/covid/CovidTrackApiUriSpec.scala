package com.iscs.covidbystates.covid

import org.specs2.matcher.MatchResult

class CovidTrackApiUriSpec extends org.specs2.mutable.Specification {

  def baseCheck(): MatchResult[String] =
    CovidTrackApiUri.builder(CovidTrackApiUri("NY", "2020-05-21")) must beEqualTo("https://covidtracking.com/api/v1/states/NY/20200521.json")

  def withPathCheck(): MatchResult[String] =
    CovidTrackApiUri.builder(CovidTrackApiUri("NY", "2020-05-21").withPath("/path")) must beEqualTo("https://covidtracking.com/path/NY/20200521.json")


  def withStateCheck(): MatchResult[String] =
    CovidTrackApiUri.builder(CovidTrackApiUri("NY", "2020-05-21").withState("FL")) must beEqualTo("https://covidtracking.com/api/v1/states/FL/20200521.json")


  def asBlankCheck(): MatchResult[String] =
    CovidTrackApiUri.builder(CovidTrackApiUri("", "2020-05-21").blank) must beEqualTo("//20200521.json")


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

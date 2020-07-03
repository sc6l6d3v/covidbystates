package com.iscs.covidbystates.covid

import org.specs2.matcher.MatchResult

class CovidApiUriSpec extends org.specs2.mutable.Specification {

  private[this] def baseCheck(): MatchResult[String] =
    CovidApiUri.builder(CovidApiUri("", "")) must beEqualTo("https://covid-api.com/api/reports/?date=2020-06-29&q=US%20&iso=USA&region_name=US&region_province=&city_name=")

  private[this] def withBaseCheck(): MatchResult[String] =
    CovidApiUri.builder(CovidApiUri("NY", "New York")) must beEqualTo("https://covid-api.com/api/reports/?date=2020-06-29&q=US%20NY&iso=USA&region_name=US&region_province=NY&city_name=New%20York")

  private[this] def withPathCheck(): MatchResult[String] =
    CovidApiUri.builder(CovidApiUri("NY", "New York").withPath("/foo")) must beEqualTo("https://covid-api.com/foo/?date=2020-06-29&q=US%20NY&iso=USA&region_name=US&region_province=NY&city_name=New%20York")

  private[this] def withQueryCheck(): MatchResult[String] =
    CovidApiUri.builder(CovidApiUri("NY", "New York").withQuery("/query")) must beEqualTo("https://covid-api.com/api/reports/?date=2020-06-29&q=/query&iso=USA&region_name=US&region_province=NY&city_name=New%20York")

  def withISOCheck(): MatchResult[String] =
    CovidApiUri.builder(CovidApiUri("NY", "New York").withIso("/iso")) must beEqualTo("https://covid-api.com/api/reports/?date=2020-06-29&q=US%20NY&iso=/iso&region_name=US&region_province=NY&city_name=New%20York")

  def withRegionNameCheck(): MatchResult[String] =
    CovidApiUri.builder(CovidApiUri("NY", "New York").withRegionName("regionname")) must beEqualTo("https://covid-api.com/api/reports/?date=2020-06-29&q=US%20NY&iso=USA&region_name=regionname&region_province=NY&city_name=New%20York")

  def withRegionProvinceCheck(): MatchResult[String] =
    CovidApiUri.builder(CovidApiUri("NY", "New York").withRegionProvince("regionprovince")) must beEqualTo("https://covid-api.com/api/reports/?date=2020-06-29&q=US%20NY&iso=USA&region_name=US&region_province=regionprovince&city_name=New%20York")


  def withCityNameCheck(): MatchResult[String] =
    CovidApiUri.builder(CovidApiUri("NY", "New York").withCityName("cityname")) must beEqualTo("https://covid-api.com/api/reports/?date=2020-06-29&q=US%20NY&iso=USA&region_name=US&region_province=NY&city_name=cityname")

  private[this] def asBlankCheck(): MatchResult[String] =
  CovidApiUri.builder(CovidApiUri("", "").blank) must beEqualTo("/?date=2020-06-29&q=US&iso=USA&region_name=US&region_province=&city_name=")

  "CovidApiUri" >> {
    "base checks" >> {
      baseCheck()
    }
    "withBase override" >> {
      withBaseCheck()
    }
    "withPath override" >> {
      withPathCheck()
    }
    "withQuery override" >> {
      withQueryCheck()
    }
    "withISO override" >> {
      withISOCheck()
    }
    "withRegionName override" >> {
      withRegionNameCheck()
    }
    "withRegionProvince override" >> {
      withRegionProvinceCheck()
    }
    "withCityName override" >> {
      withCityNameCheck()
    }
    "asBlank check" >> {
      asBlankCheck()
    }
  }

}

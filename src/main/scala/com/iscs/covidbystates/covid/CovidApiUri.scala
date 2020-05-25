package com.iscs.covidbystates.covid

import java.time._
import java.time.format.DateTimeFormatter

case class CovidApiUri(base: String, path: String, date: String = LocalDate.now.minusDays(1).format(DateTimeFormatter.ISO_LOCAL_DATE),
                       query: String = "US", iso: String = "USA", regionName: String = "US",
                       regionProvince: String, cityName: String) {
  def withBase(newBase: String): CovidApiUri = copy(base = newBase)

  def withPath(newPath: String): CovidApiUri = copy(path = newPath)

  def withQuery(newQuery: String): CovidApiUri = copy(query = newQuery)

  def withIso(newISO: String): CovidApiUri = copy(iso = newISO)

  def withRegionName(newRegionName: String): CovidApiUri = copy(regionName = newRegionName)

  def withRegionProvince(newRegionProvince: String): CovidApiUri = copy(regionProvince = newRegionProvince)

  def withCityName(newCityName: String): CovidApiUri = copy(cityName = newCityName)

  def blank = CovidApiUri(base = "", path = "", regionProvince =  "", cityName =  "")
}

object CovidApiUri {
  val baseUri = "https://covid-api.com"
  val basePath = "/api/reports"
  val params = List("date=2020-05-14",
    "q",
    "iso",
    "region_name",
    "region_province",
    "city_name"
  )

  def apply(state: String, city: String): CovidApiUri =
    CovidApiUri(base = baseUri, path = basePath, regionProvince = state, cityName = city).withQuery(s"US%20$state")

  def builder(uri: CovidApiUri): String = s"${uri.base}${uri.path}/?date=${uri.date}&q=${uri.query}&iso=${uri.iso}&" +
    s"region_name=${uri.regionName}&region_province=${uri.regionProvince}&city_name=${uri.cityName}"

}

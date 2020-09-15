package zio.stm.graph

import zio.stm.graph.Type.{EdgeType, VertexType}

object AirRoutesSchema extends GraphSchema {

  override type VTs[K, V] = AirRoutesVertexType[K, V]
  override type CTs[IK, IV, OK, OV] = AirRoutesConnectionType[IK, IV, OK, OV]
  override type ETs[IK, IV, EK, E, OK, OV] = AirRoutesEdgeType[IK, IV, EK, E, OK, OV]

  //vertices
  sealed case class Airport(id: Int, code: String, icao: String, desc: String)
  sealed case class Country(id: Int, code: String, desc: String)
  sealed case class Continent(id: Int, code: String, desc: String)

  //edges
  sealed case class Route(id: Int, distance: Int)
  sealed case class Contains(id: Int)

  //types
  sealed trait AirRoutesVertexType[VK, V] extends VertexType[VK, V]
  object AirRoutesVertexType {

    implicit case object Airports extends AirRoutesVertexType[Int, Airport] {
      self =>
      override val key: Airport => Int = _.id
    }
    implicit case object Countries extends AirRoutesVertexType[Int, Country] {
      self =>
      override val key: Country => Int = _.id
    }
    implicit case object Continents extends AirRoutesVertexType[Int, Continent] {
      self =>
      override val key: Continent => Int = _.id
    }
  }
  sealed trait AirRoutesConnectionType[IK, IV, OK, OV] extends ConnectionType[IK, IV, OK, OV]
  object AirRoutesConnectionType {
    import zio.stm.graph.AirRoutesSchema.AirRoutesVertexType._

    implicit case object AirportConnection extends AirRoutesConnectionType[Int, Airport, Int, Airport] {
      override val from: VertexType[Int, Airport] = Airports
      override val to: VertexType[Int, Airport] = Airports
    }
    implicit case object ContinentAirport extends AirRoutesConnectionType[Int, Continent, Int, Airport] {
      override val from: VertexType[Int, Continent] = Continents
      override val to: VertexType[Int, Airport] = Airports
    }
    implicit case object CountryAirport extends AirRoutesConnectionType[Int, Country, Int, Airport] {
      override val from: VertexType[Int, Country] = Countries
      override val to: VertexType[Int, Airport] = Airports
    }
  }

  sealed trait AirRoutesEdgeType[IK, IV, EK, E, OK, OV] extends EdgeType[IK, IV, EK, E, OK, OV]
  object AirRoutesEdgeType {
    import zio.stm.graph.AirRoutesSchema.AirRoutesConnectionType._
    implicit case object Routes extends AirRoutesEdgeType[Int, Airport, Int, Route, Int, Airport] {
      self =>
      override val key: Route => Int = _.id
      override val ct: AirRoutesConnectionType[Int, Airport, Int, Airport] = AirportConnection
    }

    implicit case object ContinentContainsAirport
      extends AirRoutesEdgeType[Int, Continent, Int, Contains, Int, Airport] {
      self =>
      override val key: Contains => Int = _.id
      override val ct: AirRoutesConnectionType[Int, Continent, Int, Airport] = ContinentAirport
    }

    implicit case object CountryContainsAirport extends AirRoutesEdgeType[Int, Country, Int, Contains, Int, Airport] {
      self =>
      override val key: Contains => Int = _.id
      override val ct: AirRoutesConnectionType[Int, Country, Int, Airport] = CountryAirport
    }
  }
}

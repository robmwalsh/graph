package com.github.unclebob418.graph

import com.github.unclebob418.graph.AirRoutesSchema.AirRoutesConnectionType.{AirportConnection, ContinentAirport, CountryAirport}
import com.github.unclebob418.graph.AirRoutesSchema.AirRoutesEdgeType.Routes
import com.github.unclebob418.graph.AirRoutesSchema.AirRoutesVertexType.{Airports, Continents, Countries}
import com.github.unclebob418.graph.Key.{EdgeKey, VertexKey}
import com.github.unclebob418.graph.Type.{EdgeType, VertexType}


//based on https://github.com/krlawrence/graph/tree/master/sample-data

object AirRoutesSchema extends GraphSchema {

  override type VTs[K, V]                  = AirRoutesVertexType[K, V]
  override type CTs[IK, IV, OK, OV]        = AirRoutesConnectionType[IK, IV, OK, OV]
  override type ETs[IK, IV, EK, E, OK, OV] = AirRoutesEdgeType[IK, IV, EK, E, OK, OV]

  val airports: AirRoutesVertexType[Int, Airport]                       = Airports
  val countries: AirRoutesVertexType[Int, Country]                      = Countries
  val continents: AirRoutesVertexType[Int, Continent]                   = Continents
  val routes: AirRoutesEdgeType[Int, Airport, Int, Route, Int, Airport] = Routes

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

    implicit case object Airports extends AirRoutesVertexType[Int, Airport] { self =>
      override def key(v: Airport): VertexKey[Int, Airport] = VertexKey(v.id , self)
    }
    implicit case object Countries extends AirRoutesVertexType[Int, Country] { self =>
      override def key(v: Country): VertexKey[Int, Country] = VertexKey(v.id, self)
    }
    implicit case object Continents extends AirRoutesVertexType[Int, Continent] { self =>
      override def key(v: Continent): VertexKey[Int, Continent] = VertexKey(v.id, self)
    }
  }
  sealed trait AirRoutesConnectionType[IK, IV, OK, OV] extends ConnectionType[IK, IV, OK, OV]
  object AirRoutesConnectionType {

    implicit case object AirportConnection extends AirRoutesConnectionType[Int, Airport, Int, Airport] {
      override val from: VertexType[Int, Airport] = Airports
      override val to: VertexType[Int, Airport]   = Airports
    }
    implicit case object ContinentAirport extends AirRoutesConnectionType[Int, Continent, Int, Airport] {
      override val from: VertexType[Int, Continent] = Continents
      override val to: VertexType[Int, Airport]     = Airports
    }
     implicit case object CountryAirport extends AirRoutesConnectionType[Int, Country, Int, Airport] {
      override val from: VertexType[Int, Country] = Countries
      override val to: VertexType[Int, Airport]   = Airports
    }
  }

  sealed trait AirRoutesEdgeType[IK, IV, EK, E, OK, OV] extends EdgeType[IK, IV, EK, E, OK, OV]
  object AirRoutesEdgeType {

    implicit case object Routes extends AirRoutesEdgeType[Int, Airport, Int, Route, Int, Airport] { self =>
      override def key(e: Route): EdgeKey[Int, Airport, Int, Route, Int, Airport]                      = EdgeKey(e.id, self) //todo get rid of repetition?
      override val ct: AirRoutesConnectionType[Int, Airport, Int, Airport] = AirportConnection
    }

    implicit case object ContinentContainsAirport
        extends AirRoutesEdgeType[Int, Continent, Int, Contains, Int, Airport] {self =>
      override def key(e: Contains): EdgeKey[Int, Continent, Int, Contains, Int, Airport]                  = EdgeKey(e.id, self) //todo get rid of repetition?
      override val ct: AirRoutesConnectionType[Int, Continent, Int, Airport] = ContinentAirport
    }

    implicit case object CountryContainsAirport extends AirRoutesEdgeType[Int, Country, Int, Contains, Int, Airport] { self =>
      override def key(e: Contains): EdgeKey[Int, Country, Int, Contains, Int, Airport]                = EdgeKey(e.id, self) //todo get rid of repetition?
      override val ct: AirRoutesConnectionType[Int, Country, Int, Airport] = CountryAirport
    }
  }
}

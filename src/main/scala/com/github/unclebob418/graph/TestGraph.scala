package com.github.unclebob418.graph

import com.github.unclebob418.graph.AirRoutesEdgeType._
import com.github.unclebob418.graph.AirRoutesVertexType._
import com.github.unclebob418.graph.traversal.interpreter.DescriptionInterpreter

//based on https://github.com/krlawrence/graph/tree/master/sample-data

sealed trait AirRoutesVertexType[VK, V] extends VertexType[VK, V]
object AirRoutesVertexType {
  //vertices
  sealed case class Airport(id: Int, code: String, icao: String, desc: String)
  sealed case class Country(id: Int, code: String, desc: String)
  sealed case class Continent(id: Int, code: String, desc: String)

  implicit case object Airports extends AirRoutesVertexType[Int, Airport] { self =>
    override def key(v: Airport): VertexKey[Int, Airport] = VertexKey(v.id)
  }
  implicit case object Countries extends AirRoutesVertexType[Int, Country] {
    override def key(v: Country): VertexKey[Int, Country] = VertexKey(v.id)
  }
  implicit case object Continents extends AirRoutesVertexType[Int, Continent] {
    override def key(v: Continent): VertexKey[Int, Continent] = VertexKey(v.id)
  }
}
sealed trait AirRoutesConnectionType[IK, IV, OK, OV] extends ConnectionType[IK, IV, OK, OV]
object AirRoutesConnectionType {
  implicit case object AirportAirport   extends AirRoutesConnectionType[Int, Airport, Int, Airport]
  implicit case object ContinentAirport extends AirRoutesConnectionType[Int, Continent, Int, Airport]
  implicit case object CountryAirport   extends AirRoutesConnectionType[Int, Country, Int, Airport]
}

sealed trait AirRoutesEdgeType[IK, IV, EK, E, OK, OV] extends EdgeType[IK, IV, EK, E, OK, OV]
object AirRoutesEdgeType {
  //edges
  sealed case class Route(id: Int, distance: Int)
  sealed case class Contains(id: Int)

  implicit case object Routes extends AirRoutesEdgeType[Int, Airport, Int, Route, Int, Airport] {
    override def key(e: Route): EdgeKey[Int, Route] = EdgeKey(e.id)
  }

  implicit case object ContinentAirport extends AirRoutesEdgeType[Int, Continent, Int, Contains, Int, Airport] {
    override def key(e: Contains): EdgeKey[Int, Contains] = EdgeKey(e.id) //todo get rid of repetition?
  }

  implicit case object CountryAirport extends AirRoutesEdgeType[Int, Country, Int, Contains, Int, Airport] {
    override def key(e: Contains): EdgeKey[Int, Contains] = EdgeKey(e.id) //todo get rid of repetition?
  }
}

object AirRoutesSchema extends GraphSchema {
  override type VTs[K, V]                  = AirRoutesVertexType[K, V]
  override type CTs[IK, IV, OK, OV]        = AirRoutesConnectionType[IK, IV, OK, OV]
  override type ETs[IK, IV, EK, E, OK, OV] = AirRoutesEdgeType[IK, IV, EK, E, OK, OV]
}

object Test extends App {

  val syd      = Airport(1, "SYD", "YSSY", "Sydney Kingsford Smith")
  val mel      = Airport(2, "MEL", "YMML", "Melbourne International Airport")
  val aus      = Country(1, "AUS", "Australia")
  val as       = Continent(1, "OC", "Oceana")
  val route1   = Route(1, 500)
  val route2   = Route(2, 500)
  val contains = Contains(1)

  val g =
    (Graph.empty(AirRoutesSchema)
      flatMap (_.addV(syd))
      flatMap (_.addV(mel))
      flatMap (_.addV(aus))
      flatMap (_.addV(as))
      flatMap (_.addE(syd, route1, mel))
      flatMap (_.addE(mel, route2, syd))
      flatMap (_.addE(aus, contains, syd))
      flatMap (_.addE(aus, contains, mel))
      flatMap (_.addE(as, contains, syd))
      flatMap (_.addE(as, contains, syd))).head

  val x1 = g.t
    .V(Countries)
    .has(_.desc == "Australia")
    .outV(Airports)
    .has(_.code == "SYD")
    .outE(Routes)
    .has(_.distance > 200)
    //.interpret(DescriptionInterpreter)

  val x2 = g.t
    .E(Routes)
    .has(_.id == 1)
    .outV
    //.interpret(DescriptionInterpreter)

  println(s"x1 = $x1")
  println(s"x2 = $x2")
}

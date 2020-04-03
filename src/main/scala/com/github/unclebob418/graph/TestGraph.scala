package com.github.unclebob418.graph

//based on https://github.com/krlawrence/graph/tree/master/sample-data
import com.github.unclebob418.graph.AirRoutesSchema.AirRoutesVertexTypes.{ Airports, Continents, Countries }
import com.github.unclebob418.graph.AirRoutesSchema.{ Airport, Contains, Continent, Country, Route }

object AirRoutesSchema extends GraphSchema {
  override type VT[K, V] = AirRoutesVertexTypes[K, V]
  override type ET       = AirRoutesEdgeTypes

  sealed trait AirRoutesVertexTypes[K, V] extends VertexType[K, V]
  object AirRoutesVertexTypes {

    implicit case object Airports extends AirRoutesVertexTypes[Int, Airport] {
      override def key(v: Airport): VertexKey[Int, Airport] = VertexKey(v.id)
    }
    implicit case object Countries extends AirRoutesVertexTypes[Int, Country] {
      override def key(v: Country): VertexKey[Int, Country] = VertexKey(v.id)
    }
    implicit case object Continents extends AirRoutesVertexTypes[Int, Continent] {
      override def key(v: Continent): VertexKey[Int, Continent] = VertexKey(v.id)
    }
  }

  sealed trait AirRoutesEdgeTypes extends EdgeType[AirRoutesVertexTypes]
  object AirRoutesEdgeTypes {

    implicit case object Route extends AirRoutesEdgeTypes {
      override type K   = Int
      override type E   = Route
      override type In  = AirRoutesVertexTypes[Int, Airport]
      override type Out = AirRoutesVertexTypes[Int, Airport]

      override def key(e: Route): EdgeKey[Int, Route] =
        EdgeKey(e.id) //todo get rid of repetition? do we need option to be flexible?
    }

    implicit case object ContinentAirport extends AirRoutesEdgeTypes {
      override type K   = Int
      override type In  = AirRoutesVertexTypes[Int, Continent]
      override type E   = Contains
      override type Out = AirRoutesVertexTypes[Int, Airport]

      override def key(e: Contains): EdgeKey[K, Contains] = EdgeKey(e.id) //todo get rid of repetition?
    }

    implicit case object CountryAirport extends AirRoutesEdgeTypes {
      override type K   = Int
      override type In  = AirRoutesVertexTypes[Int, Country]
      override type E   = Contains
      override type Out = AirRoutesVertexTypes[Int, Airport]

      override def key(e: Contains): EdgeKey[K, Contains] = EdgeKey(e.id) //todo get rid of repetition?
    }

    /*todo try to get this to fail compliation;
         only valid vertices should be allowed on an edge*/
    implicit case object Invalid extends AirRoutesEdgeTypes {
      override type K   = String
      override type In  = AirRoutesVertexTypes[String, String]
      override type E   = String
      override type Out = AirRoutesVertexTypes[String, String]

      override def key(e: String): EdgeKey[K, String] = EdgeKey(e)
    }

  }
  //vertices
  sealed case class Airport(id: Int, code: String, icao: String, desc: String)
  sealed case class Country(id: Int, code: String, desc: String)
  sealed case class Continent(id: Int, code: String, desc: String)
  //edges
  sealed case class Route(id: Int, distance: Int)
  sealed case class Contains(id: Int)
}

object Test extends App {

  val syd      = Airport(1, "SYD", "YSSY", "Sydney Kingsford Smith")
  val mel      = Airport(2, "MEL", "YMML", "Melbourne International Airport")
  val aus      = Country(1, "AUS", "Australia")
  val as       = Continent(1, "AS", "Australasia")
  val route1   = Route(1, 500)
  val route2   = Route(2, 500)
  val contains = Contains(1)

  val g = (
    Some(Graph.empty(AirRoutesSchema))
      flatMap (_.addV(syd))
      flatMap (_.addV(mel))
      flatMap (_.addV(aus))
      flatMap (_.addV(as))
      flatMap (_.addE(syd, route1, mel))
      flatMap (_.addE(mel, route2, syd))
      flatMap (_.addE(aus, contains, syd))
      flatMap (_.addE(aus, contains, mel))
      flatMap (_.addE(as, contains, syd))
      flatMap (_.addE(as, contains, syd))
  ).head

  println("g.getVs(Airports)")
  println(g.getVs(Airports))
  println("g.getVs(Countries)")
  println(g.getVs(Countries))
  println("g.getVs(Continents)")
  println(g.getVs(Continents))

  println("g.es")
  println(g.es)

}

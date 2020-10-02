package zio.stm.graph

import zio.stm.STM
import zio.stm.graph.AirRoutesSchema.AirRoutesEdgeType.{ CountryContainsAirport, Routes }
import zio.stm.graph.AirRoutesSchema.AirRoutesVertexType.{ Airports, Continents, Countries }
import zio.stm.graph.GraphSpec.{ aus, ausContainsMel, ausContainsSyd, mel, oc, ocContainsAus, route1, route2, syd }
import zio.stm.graph.traversal.Traversal.Source

object TraverseExamples extends App {

  val graph = STM.atomically {
    for {
      g <- Graph.make(AirRoutesSchema)
      _ <- g.addV(syd)
      _ <- g.addV(mel)
      _ <- g.addV(aus)
      _ <- g.addV(oc)
      _ <- g.addE(syd, route1, mel)
      _ <- g.addE(mel, route2, syd)
      _ <- g.addE(oc, ocContainsAus, aus)
      _ <- g.addE(aus, ausContainsSyd, syd)
      _ <- g.addE(aus, ausContainsMel, mel)
    } yield g
  }

  val t1 =
    graph.map(
      _.V(Continents)                       //all continents
        .has(_.value.map(_.code == "OC"))   //oceana only
        .outV(Countries)                    //all countries in oceana
        .outE(CountryContainsAirport)       //all "contains" between all countries in oceana and their airports
        .out                                // don't need to specify the type of the out vertex because it's known to be a route
        .inE(Routes)                        // all incoming routes to all airports in all countries in oceana
        .has(_.value.map(_.distance < 500)) // with a distance < 500
    )

  val x = Source.fromIterable(List(1, 2, 3), AirRoutesSchema).key(Countries)

  //val x = t1.toString
  //.count

  //SimpleInterpreter.interpret(t1, g)

  //val r1 = DescriptionInterpreter.interpret(t1)

  /*val t2 = g
    .E(Routes)
    .outV
    .has(_.id == 2)*/
  //val r2 = DescriptionInterpreter.interpret(t2)

  //println(s"r1 = $r1")
  //println(s"r2 = $r2")

}

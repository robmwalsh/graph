package zio.stm.graph

import mercator.Ops
import zio.{ Cause, IO, ZIO }
import zio.stm.STM
import zio.stm.graph.AirRoutesSchema.AirRoutesEdgeType.Routes
import zio.stm.graph.AirRoutesSchema.AirRoutesVertexType.{ Airports, Continents, Countries }
import zio.stm.graph.AirRoutesSchema._
import zio.stm.graph.GraphError.{ VertexExists, VertexMissing }
import zio.test.Assertion._
import zio.test._

object GraphSpec extends DefaultRunnableSpec {
  val syd            = Airport(1, "SYD", "YSSY", "Sydney Kingsford Smith")
  val mel            = Airport(2, "MEL", "YMML", "Melbourne International Airport")
  val aus            = Country(1, "AUS", "Australia")
  val oc             = Continent(1, "OC", "Oceana")
  val route1         = Route(1, 500)
  val route2         = Route(2, 500)
  val ocContainsAus  = Contains(1)
  val ausContainsSyd = Contains(2)
  val ausContainsMel = Contains(3)

  def spec = suite("GraphSpec")(
    suite("vertices")(
      suite("addV")(
        testM("addV adds verticies to the graph") {
          for {
            g <- STM.atomically {
                  for {
                    g <- Graph.make(AirRoutesSchema)
                    _ <- g.addV(syd)
                  } yield g
                }
            containsSyd <- g.containsV(Airports(1)).commit
          } yield assert(containsSyd)(isTrue)
        },
        testM("addV will fail if vertex already exists in the graph") {
          for {
            g <- STM.atomically {
                  for {
                    g <- Graph.make(AirRoutesSchema)
                    _ <- g.addV(syd)
                  } yield g
                }
            fail <- g.addV(syd).commit.run
          } yield assert(fail.untraced)(
            fails(equalTo(VertexExists(s"Vertex ${Airports(1)} is already present in the graph")))
          )
        }
      ),
      testM("containsV checks presence of vertices in the graph") {
        for {
          g <- STM.atomically {
                for {
                  g <- Graph.make(AirRoutesSchema)
                  _ <- g.addV(syd)
                  _ <- g.addV(oc)
                } yield g
              }
          containsSyd <- g.containsV(Airports(1)).commit
          containsMel <- g.containsV(Airports(2)).commit
          containsOc  <- g.containsV(Continents(1)).commit
        } yield {
          assert(containsSyd)(isTrue) &&
          assert(containsMel)(isFalse) &&
          assert(containsOc)(isTrue)
        }
      },
      testM("getV will get a vertex from the graph if present") {
        for {
          g <- STM.atomically {
                for {
                  g <- Graph.make(AirRoutesSchema)
                  _ <- g.addV(syd)
                  _ <- g.addV(oc)
                } yield g
              }
          sydney    <- g.getV(Airports(1)).commit
          melbourne <- g.getV(Airports(2)).commit.run
          oceana    <- g.getV(Continents(1)).commit
        } yield {
          assert(sydney)(equalTo(syd)) &&
          assert(melbourne.untraced)(fails(equalTo(GraphError.Generic(s"${Airports(2)} not found")))) &&
          assert(oceana)(equalTo(oc))
        }
      }
    ),
    suite("edges")(
      suite("addE")(
        testM("addE adds edges to the graph") {
          for {
            g <- STM.atomically {
                  for {
                    g <- Graph.make(AirRoutesSchema)
                    _ <- g.addV(syd)
                    _ <- g.addV(mel)
                    _ <- g.addE(syd, route1, mel)
                    _ <- g.addE(mel, route2, syd)
                  } yield g
                }
            containsRoute1 <- g.containsE(Routes(1)).commit
            containsRoute2 <- g.containsE(Routes(2)).commit
          } yield {
            assert(containsRoute1)(isTrue ?? "contains route 1") &&
            assert(containsRoute2)(isTrue ?? "contains route 2")
          }
        },
        testM("addE will fail if any associated vertex is missing from the graph") {
          for {
            g <- STM.atomically {
                  for {
                    g <- Graph.make(AirRoutesSchema)
                    _ <- g.addV(syd)
                  } yield g
                }
            missingIn   <- g.addE(mel, route1, syd).commit.run
            missingOut  <- g.addE(syd, route1, mel).commit.run
            missingBoth <- g.addE(aus, ausContainsMel, mel).commit.run
          } yield {
            assert(missingIn.untraced)(fails(equalTo(VertexMissing("in vertex is missing")))) &&
            assert(missingOut.untraced)(fails(equalTo(VertexMissing("out vertex is missing")))) &&
            assert(missingBoth.untraced)(fails(equalTo(VertexMissing("both vertices are missing"))))
          }
        },
        testM("addE will fail if edge is already in the graph") {
          for {
            g <- STM.atomically {
                  for {
                    g <- Graph.make(AirRoutesSchema)
                    _ <- g.addV(syd)
                  } yield g
                }
            fail <- g.addV(syd).commit.run
          } yield assert(fail.untraced)(
            failsCause(
              containsCause(
                Cause.fail(VertexExists(s"Vertex ${Airports(1)} is already present in the graph"))
              )
            )
          )
        },
        testM("containsE checks presence of edges in the graph") {
          for {
            g <- STM.atomically {
                  for {
                    g <- Graph.make(AirRoutesSchema)
                    _ <- g.addV(syd)
                    _ <- g.addV(mel)
                    _ <- g.addE(syd, route1, mel)
                  } yield g
                }
            containsRoute1 <- g.containsE(Routes(1)).commit
            containsRoute2 <- g.containsE(Routes(2)).commit
          } yield {
            assert(containsRoute1)(isTrue ?? "contains route 1") &&
            assert(containsRoute2)(isFalse ?? "contains route 2")
          }
        }
      ),
      testM("inEs and outEs should be equivalent") { //todo check all edges are in Cs

        for {
          graph <- STM.atomically {
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
          in <- graph.state.get.commit.map(_.in.toList.flatMap {
                 case (in, outK) =>
                   outK.map { case (e, out) => (in.toString, e.toString, out.toString) }
               }
            .sorted
          )
          out <- graph.state.get.commit.map(_.out.toList.flatMap {
                  case (out, inK) =>
                    inK.map { case (e, in) => (in.toString, e.toString, out.toString) }
                }
            .sorted
          )
        } yield assert(in)(equalTo(out))
      }
    )
  )
}
//.flatMap((in, outE) => outE.flatMap((e, out) => (in, e, out)))

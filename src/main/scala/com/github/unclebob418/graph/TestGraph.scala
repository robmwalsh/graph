package com.github.unclebob418.graph

import java.util.UUID

sealed trait TestVertexSchema[V] extends VSchema[V]
object TestVertexSchema {
  implicit case object SInt    extends TestVertexSchema[Int]
  implicit case object SString extends TestVertexSchema[String]
}
sealed trait TestEdgeSchema[F, E, T]
object TestEdgeSchema {
  implicit case object IntStringString extends TestEdgeSchema[Int, String, String]
  implicit case object StringStringInt extends TestEdgeSchema[String, String, Int]
}

case class TestGraph(
  vs: Map[UUID, Vertex],
  inEs: Map[UUID, Set[Edge]],
  outEs: Map[UUID, Set[Edge]]
) extends Graph {
  override type VertexSchema[V]     = TestVertexSchema[V]
  override type EdgeSchema[F, T, E] = TestEdgeSchema[F, T, E]
}

object Test {
  val g = TestGraph(Map(), Map(), Map())
  g.addV(UUID.randomUUID(), 5)
  g.addV(UUID.randomUUID(), "hello")
  //g.addV(UUID.randomUUID(), true) //shouldn't work, doesn't work :)
  val v: Vertex = Vertex(UUID.randomUUID(), "hi")
  //val s: String = v.value
  //g.addV("hello", 7)//shouldn't work
}

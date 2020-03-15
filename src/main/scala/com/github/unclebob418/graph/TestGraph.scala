package com.github.unclebob418.graph

import java.util.UUID

sealed trait TestVertexSchema[I, V] extends VSchema[I, V]
object TestVertexSchema {
  implicit case object SIntInt extends TestVertexSchema[Int, Int]
  implicit case object SIntString extends TestVertexSchema[Int, String]
  implicit case object SStringString extends TestVertexSchema[String, String]
}
sealed trait TestEdgeSchema[F, E, T]
object TestEdgeSchema {
  implicit case object IntStringString extends TestEdgeSchema[Int, String, String]
  implicit case object StringStringInt    extends TestEdgeSchema[String, String, Int]
}

case class TestGraph(
  vs: Map[Id.VertexId[_], Vertex[_, _]],
  inEs: Map[Id.EdgeId[_], Set[Edge[_, _]]],
  outEs: Map[Id.EdgeId[_], Set[Edge[_, _]]]
) extends Graph {
  override type VertexSchema[I, V]  = TestVertexSchema[I, V]
  override type EdgeSchema[F, T, E] = TestEdgeSchema[F, T, E]
}

object Test {
  val g = TestGraph(Map(), Map(), Map())
  g.addV(5, 10)
  g.addV(7, "hello")
  g.addV("hello", "world")
  //g.addV("hello", 7)//shouldn't work
}

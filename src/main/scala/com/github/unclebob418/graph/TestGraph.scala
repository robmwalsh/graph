package com.github.unclebob418.graph

import java.util.UUID

sealed trait TestVertexSchema[I, V] extends VSchema[I, V]
object TestVertexSchema {
  implicit case object SInt extends TestVertexSchema[Int,Int] {
    override def id(v: Int): Int = v
  }
  implicit case object SString extends TestVertexSchema[String] {
  }
}
sealed trait TestEdgeSchema[F, E, T]
object TestEdgeSchema {
  implicit case object IntStringString extends TestEdgeSchema[Int, String, String]
  implicit case object IntStringInt    extends TestEdgeSchema[Int, String, Int]
}

case class TestGraph(
  vs: Map[Id.VertexId[_], Vertex[_,_]],
  inEs: Map[UUID, Set[Edge[_, _]]],
  outEs: Map[UUID, Set[Edge[_, _]]]
) extends Graph {
  override type VertexSchema[A]     = TestVertexSchema[A]
  override type EdgeSchema[F, T, E] = TestEdgeSchema[F, T, E]
}

object Test {
  val g = TestGraph(Map(), Map(), Map())
  g.addV(10)
  g.addV("hello")
  //g.addV(false)//shouldn't work
}

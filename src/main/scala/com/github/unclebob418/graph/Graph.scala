package com.github.unclebob418.graph

import java.util.UUID

import com.github.unclebob418.graph.TestVertexSchema.{ SInt, SString }

abstract class Graph {
  type VertexSchema[_]
  type EdgeSchema[_, _, _]
  val vs: Map[Id.VertexId[_], Vertex[_]]
  val inEs: Map[UUID, Set[Edge[_, _]]]
  val outEs: Map[UUID, Set[Edge[_, _]]]

  def addV[V](v: V)(implicit ev: VertexSchema[V]): Option[Graph] = ???
  //[F]rom -> [E]dge -> [T]o
  def addE[F, E, T](e: E)(implicit ev: EdgeSchema[F, E, T]): Option[Graph] = ???

}
sealed trait VSchema[A] {
  type Id
  //def id[A](a: A => Id): Id
}
sealed trait TestVertexSchema[A] extends VSchema[A]
object TestVertexSchema {
  implicit case object SInt extends TestVertexSchema[Int] {
    override type Id = String
    //override def id[A, B <: String](a: Int): String = a.toString
  }
  implicit case object SString extends TestVertexSchema[String] {
    override type Id = String
    //override def id[String](a: String => String): String
  }
}
sealed trait TestEdgeSchema[F, E, T]
object TestEdgeSchema {
  implicit case object IntStringString extends TestEdgeSchema[Int, String, String]
  implicit case object IntStringInt    extends TestEdgeSchema[Int, String, Int]
}

case class TestGraph(
  vs: Map[Id.VertexId[_], Vertex[_]],
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

trait Edge[F, T] {

  def from: Id.VertexId[F]
  def to: Id.VertexId[T]
}

sealed trait Id[A] {
  type IdType
}
object Id {
  final case class VertexId[A](v: A) extends Id[A] {
    override type IdType = A
  }
  final case class EdgeId[A](v: A) extends Id[A] {
    override type IdType = A
  }
}
final case class Vertex[A](v: A) {
  type V = A
}

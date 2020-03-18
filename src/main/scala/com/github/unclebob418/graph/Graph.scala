package com.github.unclebob418.graph

import java.util.UUID

final case class Graph[VS[_] <: VertexSchema[_], ES[_, _, _] <: ESchema[_, _, _]](
  vs: VertexMap[VS],
  inEs: Map[Any, Set[Any]],
  outEs: Map[Any, Set[Any]]
) {

  def addV[V](key: VertexKey[V], value: V)(implicit vType: VS[V]): Option[Graph[VS, ES]] =
    Some(copy(vs.put(key, value)))

  def addE[F, E, T](e: E)(implicit ev: ES[F, E, T]): Option[Graph[VS, ES]] = ???

  def getAll[V](implicit vType: VS[V]): Option[Map[VertexKey[V], V]] = vs.getAll[V]
}
object Graph {
  def empty[VS[_] <: VertexSchema[_], ES[_, _, _] <: ESchema[_, _, _]]: Graph[VS, ES] =
    Graph[VS, ES](VertexMap.empty, Map(), Map())
}

trait Edge {
  def from: UUID
  def to: UUID
}

sealed abstract class Vertex {
  type V
  val id: UUID
  val value: V
}
object Vertex {
  def apply[I0, V0](id0: UUID, value0: V0): Vertex = new Vertex {
    type I = I0
    type V = V0
    val id    = id0
    val value = value0
  }
}

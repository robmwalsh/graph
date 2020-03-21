package com.github.unclebob418.graph

import java.util.UUID

final case class Graph[VS[_, _] <: VertexSchema[_, _], ES[_, _, _] <: ESchema[_, _, _]](
  vs: VertexMap[VS],
  inEs: Map[Any, Set[Any]],
  outEs: Map[Any, Set[Any]]
) {

  def addV[K, V](key: VertexKey[K, V], value: V)(implicit vType: VS[K, V]): Graph[VS, ES] =
    copy(vs.put(key, value)) //todo validate Some(A)

  def addE[F, E, T](e: E)(implicit ev: ES[F, E, T]): Option[Graph[VS, ES]] = ???

  def getV[K, V](vk: VertexKey[K, V])(implicit vType: VS[K, V]): Option[V] =
    vs.get(vk)

  def getAll[K, V](implicit vType: VS[K, V]): Option[Map[VertexKey[K, V], V]] = vs.getAll[K, V]
}
object Graph {
  def empty[VS[_,_] <: VertexSchema[_, _], ES[_, _, _] <: ESchema[_, _, _]]: Graph[VS, ES] =
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

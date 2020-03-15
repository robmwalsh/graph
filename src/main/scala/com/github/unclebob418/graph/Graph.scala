package com.github.unclebob418.graph

import java.util.UUID

case class Graph[VS[_] <: VSchema[_], ES[_, _, _] <: ESchema[_, _, _]](
  vs: Map[Any, Any],
  inEs: Map[Any, Set[Any]],
  outEs: Map[Any, Set[Any]]
) {

  def addV[V](id: UUID, value: V)(implicit ev: VS[V]): Option[Graph[VS, ES]] = ???
    //Some(copy(vs + (id -> Vertex(id, value))))

  def addE[F, E, T](e: E)(implicit ev: ES[F, E, T]): Option[Graph[VS, ES]] = ???

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

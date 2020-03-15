package com.github.unclebob418.graph

import java.util.UUID

abstract class Graph { self =>
  //[I]d -> [V]ertex
  type VertexSchema[_]
  //[F]rom -> [E]dge -> [T]o
  type EdgeSchema[_, _, _]
  val vs: Map[UUID, Vertex]
  val inEs: Map[UUID, Set[Edge]]
  val outEs: Map[UUID, Set[Edge]]

  def addV[V](id: UUID, value: V)(implicit ev: VertexSchema[V]): Option[Graph] =
    Some(update(vs + (id -> Vertex(id, value))))

  def addE[F, E, T](e: E)(implicit ev: EdgeSchema[F, E, T]): Option[Graph] = ???

  private def update(
    vs0: Map[UUID, Vertex] = self.vs,
    inEs0: Map[UUID, Set[Edge]] = self.inEs,
    outEs0: Map[UUID, Set[Edge]] = self.outEs
  ): Graph = new Graph {
    override type VertexSchema[A]  = self.VertexSchema[A]
    override type EdgeSchema[A, B, C] = self.EdgeSchema[A, B, C]
    val vs: Map[UUID, Vertex]             = vs0
    val inEs: Map[UUID, Set[Edge]]  = inEs0
    val outEs: Map[UUID, Set[Edge]] = outEs0
  }

}
trait VSchema[V]

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

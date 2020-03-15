package com.github.unclebob418.graph

abstract class Graph { self =>
  type VertexSchema[_, _]
  type EdgeSchema[_, _, _]
  val vs: Map[Id.VertexId[_], Vertex[_, _]]
  val inEs: Map[Id.EdgeId[_], Set[Edge[_, _]]]
  val outEs: Map[Id.EdgeId[_], Set[Edge[_, _]]]

  def addV[I, V](v: V)(implicit ev: VertexSchema[I, V]): Option[Graph] = ???
  //[F]rom -> [E]dge -> [T]o
  def addE[F, E, T](e: E)(implicit ev: EdgeSchema[F, E, T]): Option[Graph] = ???

  private def update(
    vs0: Map[Id.VertexId[_], Vertex[_, _]] = self.vs,
    inEs0: Map[Id.EdgeId[_], Set[Edge[_, _]]] = self.inEs,
    outEs0: Map[Id.EdgeId[_], Set[Edge[_, _]]] = self.outEs
  ): Graph = new Graph {
    override type VertexSchema[_, _]  = self.VertexSchema[_, _]
    override type EdgeSchema[_, _, _] = self.EdgeSchema[_, _, _]
    val vs: Map[Id.VertexId[_], Vertex[_, _]]     = vs0
    val inEs: Map[Id.EdgeId[_], Set[Edge[_, _]]]  = inEs0
    val outEs: Map[Id.EdgeId[_], Set[Edge[_, _]]] = outEs0
  }

}
trait VSchema[I, V] {
  def id(v: V): I
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
final case class Vertex[I, V0](id: I, v: V0) {
  type V = V0
}

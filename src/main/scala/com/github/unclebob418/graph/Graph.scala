package com.github.unclebob418.graph

import java.util.UUID
import zio._

//todo implement schema, limiting the types of objects that can be added and the types that can be joined
//todo make covariant? might need to track node/edge types with type tags
final case class Graph[V, E] private(vs: Map[UUID, Vertex[V]], inEs: Map[UUID, Edge[E]], outEs: Map[UUID, Edge[E]]) {

  def addV(v: Vertex[V]): Option[Graph[V, E]] =
    if (containsV(v)) None else Some(copy(vs = vs.updated(v.id, v)))

  def connect(from: UUID, to: UUID, e: E): Graph[V, E] = { //todo check for cycles
    val edge = Edge(from, to, e)
    copy(inEs = inEs.updated(to, edge), outEs = outEs.updated(from, edge))
  }

  def containsV(v: Vertex[V]): Boolean =
    vs.contains(v.id)

  def updateV(v: Vertex[V]): Option[Graph[V, E]] =
    if (containsV(v)) Some(copy(vs = vs.updated(v.id, v))) else None

  def upsertV(v: Vertex[V]): Graph[V, E] =
    copy(vs = vs.updated(v.id, v))

  def removeVbyId(id: UUID): Graph[V, E] = Graph(vs - id,
    inEs.filterNot(_._2.to == id),
    outEs.filterNot(_._2.from == id))
}

//todo protect constructors - Nodes & Verticies should only ever be constructed as they enter a graph -
// if it's not in there, it's not a node/edge
final case class Vertex[V](id: UUID, vertex: V)
final case class Edge[E](from: UUID, to: UUID, edge: E) //arc
// todo add line (undirected), hyperarc (directed one:many), hyperline(undirected set)
//node --| edge |-> node
//              |-> node
//node --| edge |-- node

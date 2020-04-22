package com.github.unclebob418.graph

import com.github.unclebob418.graph.traversal.Traversal.Source
import com.github.unclebob418.graph.traversal.Traversal.Source.GraphTraversalSource

//todo standardise naming and order of types here
trait Schema[GS <: GraphSchema] {
  val gs: GS
  type VTs[VK, V]                 = gs.VTs[VK, V]
  type CTs[IK, IV, OK, OV]        = gs.CTs[IK, IV, OK, OV]
  type ETs[IK, IV, EK, E, OK, OV] = gs.ETs[IK, IV, EK, E, OK, OV]
}

trait GraphSchema { self =>
  type VTs[VK, V] <: VertexType[VK, V]                               //vertex types
  type CTs[IK, IV, OK, OV] <: ConnectionType[IK, IV, OK, OV]         //allowed connections
  type ETs[IK, IV, EK, E, OK, OV] <: EdgeType[IK, IV, EK, E, OK, OV] //allowed edges

  //todo allow cycles if desired
  //todo allow certain types of cycles only?
  //todo composition of GraphSchemas (much easier now!)
}

sealed case class VertexKey[+K, +V](key: K)
//todo add type to key? sealed case class VertexKey[K, V, VT <: VertexType[K, V]](key: K)(implicit vType: VertexType[K, V])

sealed case class EdgeKey[K, +E](key: K)

trait VertexType[K, V] {
  def key(v: V): VertexKey[K, V]
}

trait ConnectionType[IK, IV, OK, OV]

trait EdgeType[IK, IV, EK, E, OK, OV] {
  def key(e: E): EdgeKey[EK, E]
}

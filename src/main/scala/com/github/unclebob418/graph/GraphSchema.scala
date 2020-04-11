package com.github.unclebob418.graph

trait GraphSchema {
  type VT[K, V] <: VertexType[K, V]
  type ET <: EdgeType[VT]

  //todo allow cycles if desired
  //todo allow certain types of cycles only?
  //todo composition of GraphSchemas?
}

//todo can K be invariant? should it be?
sealed case class VertexKey[+K, +V](key: K)
//todo add type to key? sealed case class VertexKey[K, V, VT <: VertexType[K, V]](key: K)(implicit vType: VertexType[K, V])

trait VertexType[K, V] {
  //get a key given a value
  //get a key given a value
  def key(v: V): VertexKey[K, V]
  //todo add type to key? def key[VT <: VertexType[K, V]](v: V)(implicit vType: VertexType[K, V])
}

sealed case class EdgeKey[K, +E](key: K)

trait EdgeType[VT[_, _] <: VertexType[_, _]] {
  type K
  type E
  type In <: VT[_, _]
  type Out <: VT[_, _]

  def key(e: E): EdgeKey[K, E]
}

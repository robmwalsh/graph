package com.github.unclebob418.graph

trait GraphSchema {
  type VT[K, V] <: VertexType[K, V]
  type ET <: EdgeType[VT]
}

sealed case class VertexKey[+K, +V0](key: K)

trait VertexType[K, V] {
  def key(v: V): VertexKey[K, V]
}

sealed case class EdgeKey[+K, +E0](key: K)

trait EdgeType[VT[_, _] <: VertexType[_, _]] {
  type K
  type E
  type In <: VT[_, _]
  type Out <: VT[_, _]

  def key(e: E): EdgeKey[K, E]
}

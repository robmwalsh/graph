package com.github.unclebob418.graph

trait GraphSchema {
  type VT[K, V] <: VertexType[K, V]
  type ET <: EdgeType[VT]
}

//todo `VertexKey` shouldn't actually contain a value, just needs to track the type
sealed case class VertexKey[+K, +V](key: K, v: V)

trait VertexType[K, V] {
  def key(v: V): VertexKey[K, V]
}
//todo `EdgeKey` shouldn't actually contain a value, just needs to track the type
sealed case class EdgeKey[K, E](key: K, v: E)

trait EdgeType[VT[_, _] <: VertexType[_, _]] {
  type K
  type E
  type EK = EdgeKey[K, E]
  type In <: VT[_, _]
  type Out <: VT[_, _]

  def key(e: E): EdgeKey[K, E]
}

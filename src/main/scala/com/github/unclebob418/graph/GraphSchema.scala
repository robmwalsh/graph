package com.github.unclebob418.graph


trait GraphSchema {
  type VT[K, V] <: VertexType[K,V]
  type ET <: EdgeType[VT]
}

trait VertexKey[+K, +V] {
  val key: K
}

trait VertexType[K, V]

trait EdgeKey[K, V] {
  val key: K
}

trait EdgeType[VT[_, _] <: VertexType[_, _]] {
  type In <: VT[_,_]
  type Out <: VT[_,_]
  type E
}

object ExampleGraph extends GraphSchema {

}
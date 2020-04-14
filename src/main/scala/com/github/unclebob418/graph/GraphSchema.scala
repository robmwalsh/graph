package com.github.unclebob418.graph

trait Schema[GS <: GraphSchema] {
  val gs: GS
  type ET       = gs.ET
  type VTs[K, V] = gs.VTs[K, V]
}

trait GraphSchema {
  type VTs[K, V] <: VertexType[K, V]
  type ET <: EdgeType[VTs]

  //todo allow cycles if desired
  //todo allow certain types of cycles only?
  //todo composition of GraphSchemas?
}

/*object GraphSchema {
  type Aux[VT[_, _] <: VertexType[K, V], ET <: EdgeType[VT], K, V] = GraphSchema {
    type VT = VertexType[K, V]
    type ET <: EdgeType[VT]
  }
}*/

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

trait EdgeType[VTs[_, _] <: VertexType[_, _]] {
  type K
  type E
  type In <: VTs[_, _]
  type Out <: VTs[_, _]

  def key(e: E): EdgeKey[K, E]
}
object EdgeType {
  //todo does this actaully help anywhere?
  type Aux[IVT <: VertexType[IK, IV], K0, E0, OVT <: VertexType[OK, OV], IK, IV, OK, OV] = GraphSchema {
    type In  = IVT
    type K   = K0
    type E   = E0
    type Out = OVT
  }
}

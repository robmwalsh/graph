package com.github.unclebob418.graph

import java.util.UUID

sealed trait Graph[GS <: GraphSchema] { self =>
  val vs: VertexMap[gs.VS]
  val es: EdgeMap[gs.VS]
  val gs: GS
  type VS[K, V] = gs.VS[K, V]
  def copy(vs0: VertexMap[gs.VS] = vs, es0: EdgeMap[gs.VS] = es): Graph[GS] = new Graph[GS] {
    val vs: VertexMap[gs.VS] = vs0
    val es: EdgeMap[gs.VS]   = es0
    val gs                   = self.gs
  }

  def addV[K, V](key: VertexKey[K, V], value: V)(implicit vType: VS[K, V]): Option[Graph[GS]] =
    Some(copy(vs.addV(key, value))) //todo validate Some(A)

  def addE[K, E0, IK, IV, OK, OV](inVK: VertexKey[IK, IV], edgeKey: EdgeKey[K, E0], e: E0, outVK: VertexKey[OK, OV])(
    implicit eType: gs.ES {
      type In  = gs.VS[IK, IV]
      type Out = gs.VS[OK, OV]
      type E   = E0
    }
  ): Option[Graph[GS]] = Some(copy(vs, es.addE(inVK, edgeKey, e, outVK)))

  def containsV[K, V](vk: VertexKey[K, V])(implicit vType: VS[K, V]): Boolean = vs.containsV(vk)

  def getV[K, V](vk: VertexKey[K, V])(implicit vType: VS[K, V]): Option[V] =
    vs.getV(vk)

  def getVs[K, V](implicit vType: VS[K, V]): Option[Map[VertexKey[K, V], V]] = vs.getAll[K, V]
}
object Graph {
  def empty[GS <: GraphSchema](implicit graphSchema0: GS) =
    new Graph[GS] {
      val vs          = VertexMap.empty[gs.VS]
      val es          = EdgeMap.empty[gs.VS]
      val gs = graphSchema0
    }
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

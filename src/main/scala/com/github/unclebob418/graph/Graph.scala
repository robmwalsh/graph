package com.github.unclebob418.graph

import java.util.UUID

object Graph {
  def empty[GS <: GraphSchema](implicit gs0: GS) =
    new Graph[GS] {
      val vs = VertexMap.empty[gs.VT]
      val es = EdgeMap.empty[gs.VT]
      val gs = gs0
    }
}

sealed trait Graph[GS <: GraphSchema] { self =>
  val vs: VertexMap[gs.VT]
  val es: EdgeMap[gs.VT]
  val gs: GS
  type VT[K, V] = gs.VT[K, V]
  def copy(vs0: VertexMap[gs.VT] = vs, es0: EdgeMap[gs.VT] = es): Graph[GS] = new Graph[GS] {
    val vs: VertexMap[gs.VT] = vs0
    val es: EdgeMap[gs.VT]   = es0
    val gs                   = self.gs
  }

  def addV[K, V](v: V)(implicit vType: VT[K, V]): Some[Graph[GS]] = Some(copy(vs.addV(vType.key(v), v)))
  def addV[K, V](key: VertexKey[K, V], value: V)(implicit vType: VT[K, V]): Option[Graph[GS]] =
    Some(copy(vs.addV(key, value))) //todo validate Some(A)

  def addE[K, E0, IK, IV, OK, OV](inV: IV, e: E0, outV: OV)(
    implicit eType: gs.ET {
    type In  = gs.VT[IK, IV]
    type Out = gs.VT[OK, OV]
    type E   = E0
  },
    iVType: VT[IK, IV],
    oVType: VT[OK, OV]
  ): Option[Graph[GS]] = addE(iVType.key(inV), e, oVType.key(outV))

  def addE[K, E0, IK, IV, OK, OV](inVK: VertexKey[IK, IV], e: E0, outVK: VertexKey[OK, OV])(
    implicit eType: gs.ET {
      type In  = gs.VT[IK, IV]
      type Out = gs.VT[OK, OV]
      type E   = E0
    }
  ): Option[Graph[GS]] = Some(copy(vs, es.addE(inVK, eType.key(e), e, outVK)))

  def containsV[K, V](vk: VertexKey[K, V])(implicit vType: VT[K, V]): Boolean = vs.containsV(vk)

  def getV[K, V](vk: VertexKey[K, V])(implicit vType: VT[K, V]): Option[V] =
    vs.getV(vk)

  def getVs[K, V](implicit vType: VT[K, V]): Option[Map[VertexKey[K, V], V]] = vs.getAll[K, V]
}


sealed abstract class Vertex {
  type K
  type V
}
object Vertex {
  def apply[K0, V0](id0: UUID, value0: V0): Vertex = new Vertex {
    type K = K0
    type V = V0
    val id    = id0
    val value = value0
  }
}

sealed case class Edge[K, E, IK, IV, OK, OV](
  inVK: VertexKey[IK, IV],
  edgeKey: EdgeKey[K, E],
  edge: E,
  outV: VertexKey[IK, IV]
)

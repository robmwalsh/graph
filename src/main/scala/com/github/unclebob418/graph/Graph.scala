package com.github.unclebob418.graph

import java.util.UUID

object Graph {
  def empty[GS <: GraphSchema](implicit gs0: GS) =
    new Graph[GS] {
      val vs = VertexMap.empty[VT]
      val es = EdgeMap.empty[VT]
      val gs = gs0
    }
}

sealed trait Graph[GS <: GraphSchema] { self =>
  type ET       = gs.ET
  type VT[K, V] = gs.VT[K, V]

  val vs: VertexMap[gs.VT]
  val es: EdgeMap[gs.VT]


  implicit val gs: GS

  def addV[K, V](v: V)(implicit vType: VT[K, V]): Some[Graph[GS]] = Some(copy(vs.addV(vType.key(v), v)))
  def addV[K, V](key: VertexKey[K, V], value: V)(implicit vType: VT[K, V]): Option[Graph[GS]] =
    Some(copy(vs.addV(key, value))) //todo validate Some(A)

  def addE[K, E0, IK, IV, OK, OV](inV: IV, e: E0, outV: OV)(
    implicit eType: ET {
      type In  = VT[IK, IV]
      type Out = VT[OK, OV]
      type E   = E0
    },
    iVType: VT[IK, IV],
    oVType: VT[OK, OV]
  ): Option[Graph[GS]] = addE(iVType.key(inV), e, oVType.key(outV))

  def addE[K, E0, IK, IV, OK, OV](inVK: VertexKey[IK, IV], e: E0, outVK: VertexKey[OK, OV])(
    implicit eType: ET {
      type In  = VT[IK, IV]
      type Out = VT[OK, OV]
      type E   = E0
    }
  ): Option[Graph[GS]] = Some(copy(vs, es.addE(inVK, eType.key(e), e, outVK))) //ij compiler issue

  def containsV[K, V](vk: VertexKey[K, V])(implicit vType: VT[K, V]): Boolean = vs.containsV(vk)

  def copy(vs0: VertexMap[VT] = vs, es0: EdgeMap[VT] = es): Graph[GS] = new Graph[GS] {
    val vs: VertexMap[VT] = vs0 //ij compiler issue
    val es: EdgeMap[VT]   = es0 //ij compiler issue
    val gs                = self.gs
  }

  def getV[K, V](vk: VertexKey[K, V])(implicit vType: VT[K, V]): Option[V] =
    vs.getV(vk)

  def getVs[K, V](implicit vType: VT[K, V]): Option[Map[VertexKey[K, V], V]] = vs.getAll[K, V]
}

sealed abstract class Vertex {
  type K
  type V
}
object Vertex {
  def apply[K0, V0](id0: K0, value0: V0): Vertex = new Vertex {
    type K = K0
    type V = V0
    val id: K    = id0
    val value: V = value0
  }
}

sealed case class Edge[K, E, IK, IV, OK, OV](
  inVK: VertexKey[IK, IV],
  edgeKey: EdgeKey[K, E],
  edge: E,
  outV: VertexKey[IK, IV]
)

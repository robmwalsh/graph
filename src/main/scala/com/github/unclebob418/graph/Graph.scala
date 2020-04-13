package com.github.unclebob418.graph

import com.github.unclebob418.graph.VTraversal._
object Graph {

  def empty[GS <: GraphSchema](implicit gs0: GS): Option[Graph[GS]] =
    Some(new Graph[GS] {
      val vMap = Map.empty[Any, Any]
      val eMap = Map.empty[Any, Any]
      val tMap = Map.empty[Any, Map[Any, Any]]
      val gs   = gs0
    })
}

sealed trait Graph[GS <: GraphSchema] { self =>
  type ET       = gs.ET
  type VT[K, V] = gs.VT[K, V]

  val vMap: Map[Any, Any]           //[(Type, VK), VV] // vertex map, stores all verticies
  val eMap: Map[Any, Any]           //[(Type, EK), EV] // edge map, stores all edges
  val tMap: Map[Any, Map[Any, Any]] //[Type, Map[K, V]] // tmap, stores all verticies and edges indexed by type

  val gs: GS

  def V[K, V](vType: VT[K, V]): VertexSource[K, V, GS] = VertexSource(self)(gs, vType)

  def E[IK, IV, OK, OV, K0, E0](eType: ET {
    type In  = VT[IK, IV]
    type K   = K0
    type E   = E0
    type Out = VT[OK, OV]
  }) = ???

  def addV[K, V](v: V)(implicit vType: VT[K, V]): Some[Graph[GS]] = {
    //todo validate
    val vertexKey            = vType.key(v)
    val vertex: Vertex[K, V] = Vertex(vertexKey, v)
    Some(
      copy(
        vMap0 = vMap + ((vType, vertexKey) -> vertex),
        tMap0 = tMap.get(vType) match {
          case Some(subMap) =>
            tMap + (vType -> (subMap + (vertexKey -> vertex)))
          case None =>
            tMap + (vType -> Map(vertexKey -> vertex))
        }
      )
    )
  }

  def addE[K, E0, IK, IV, OK, OV](inV: IV, e: E0, outV: OV)(
    implicit eType: ET {
      type In  = VT[IK, IV]
      type Out = VT[OK, OV]
      type E   = E0
    },
    iVType: VT[IK, IV],
    oVType: VT[OK, OV]
  ): Option[Graph[GS]] = addE(iVType.key(inV), e, oVType.key(outV))

  def addE[K, E0, IK, IV, OK, OV](inVK: VertexKey[IK, IV], edge: E0, outVK: VertexKey[OK, OV])(
    implicit eType: ET {
      type In  = VT[IK, IV]
      type Out = VT[OK, OV]
      type E   = E0
    },
    iVType: VT[IK, IV],
    oVType: VT[OK, OV]
  ): Option[Graph[GS]] = {
    //todo add to typemap
    val inVOpt  = vMap.get((iVType, inVK)).asInstanceOf[Option[Vertex[IV, IK]]]
    val outVOpt = vMap.get(oVType, outVK).asInstanceOf[Option[Vertex[OV, OK]]]
    val edgeKey = eType.key(edge)
    inVOpt match {
      case Some(inV) =>
        outVOpt match {
          case Some(outV) =>
            Some(
              copy(
                vMap0 = vMap + ((iVType, inVK) -> inV.addOutE(edge, outVK, eType)) + ((oVType, outVK) -> outV
                  .addInE(edge, inVK, eType)),
                eMap0 = eMap + (edgeKey -> edge),
                tMap0 = tMap.get(eType) match {
                  case Some(subMap) =>
                    tMap + (eType -> (subMap + (edgeKey -> edge)))
                  case None =>
                    tMap + (eType -> Map(edgeKey -> edge))
                }
              )
            )
          case _ => None
        }
      case _ => None
    }

  }

  def containsV[K, V](vk: VertexKey[K, V])(implicit vType: VT[K, V]): Boolean = vMap.contains((vType, vk))

  def copy(vMap0: Map[Any, Any] = vMap, eMap0: Map[Any, Any] = eMap, tMap0: Map[Any, Map[Any, Any]] = tMap): Graph[GS] =
    new Graph[GS] {
      val vMap   = vMap0
      val eMap   = eMap0
      val tMap   = tMap0
      val gs: GS = self.gs
    }
  /*
  def pathTo[SK, SV, TK, TV](start: VertexKey[SK, SV], target: VertexKey[TK, TV]) = {
    def loop[CK, CV](current: VertexKey[CK, CV], target: VertexKey[TK, TV], graph: Graph[GS]) = ???
    ???
  }
   */
  def getV[K, V](vk: VertexKey[K, V])(implicit vType: VT[K, V]): Option[Vertex[K, V]] =
    vMap.get((vType, vk)).asInstanceOf[Option[Vertex[K, V]]]

  def getVs[K, V](vType: VT[K, V]): Option[Map[VertexKey[K, V], Vertex[K, V]]] =
    tMap.get(vType).asInstanceOf[Option[Map[VertexKey[K, V], Vertex[K, V]]]]
  /*
  def getE[K, V](ek: EdgeKey[K, V]) = es.getE(ek)*/

  //def removeV[K, V](v: V)(implicit vType: VT[K, V]): Some[Graph[GS]] = Some(copy(vs.removeV(vType.key(v), es.removeEs)))
}

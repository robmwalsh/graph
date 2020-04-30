package com.github.unclebob418.graph

import com.github.unclebob418.graph.traversal.Traversal.GraphTraversalSource

object Graph {
  def empty[GS <: GraphSchema](implicit gs0: GS): Option[Graph[GS]] =
    Some(new Graph[GS] {
      val vMap = Map.empty[Any, Any]
      val eMap = Map.empty[Any, Any]
      val tMap = Map.empty[Any, Map[Any, Any]]
      val gs: GS = gs0
    })
}

sealed trait Graph[GS <: GraphSchema] extends Schema[GS] { self =>
  val vMap: Map[Any, Any]           //[(Type, VK), VV] // vertex map, stores all verticies
  val eMap: Map[Any, Any]           //[(Type, EK), EV] // edge map, stores all edges
  val tMap: Map[Any, Map[Any, Any]] //[Type, Map[K, V]] // tmap, stores all verticies and edges indexed by type

  def t: GraphTraversalSource[GS] = GraphTraversalSource(self.gs)
  def addV[K, V](v: V)(implicit vType: VTs[K, V]): Some[Graph[GS]] = {
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

  def addE[K0, E0, IK, IV, OK, OV](inV: IV, e: E0, outV: OV)(
    implicit eType: ETs[IK, IV, K0, E0, OK, OV],
    iVType: VTs[IK, IV],
    oVType: VTs[OK, OV]
  ): Option[Graph[GS]] = addE(iVType.key(inV), e, oVType.key(outV))

  //todo refactor. this is terrible.
  def addE[K0, E0, IK, IV, OK, OV](inVK: VertexKey[IK, IV], edge: E0, outVK: VertexKey[OK, OV])(
    implicit eType: ETs[IK, IV, K0, E0, OK, OV],
    iVType: VTs[IK, IV],
    oVType: VTs[OK, OV]
  ): Option[Graph[GS]] = {
    //todo add to typemap
    val inVOpt  = vMap.get((iVType, inVK)).asInstanceOf[Option[Vertex[IV, IK]]]
    val outVOpt = vMap.get(oVType, outVK).asInstanceOf[Option[Vertex[OV, OK]]]
    val edgeKey = eType.key(edge)

    inVOpt.flatMap(
      inV =>
        outVOpt.flatMap(
          outV => {
            val inVUpd  = inV.addOutE(edge, outVK, eType)
            val outVUpd = outV.addInE(edge, inVK, eType)
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
            ).flatMap(
              eAdded =>
                Some(eAdded.copy(tMap0 = eAdded.tMap.get(iVType) match {
                  case Some(subMap) =>
                    tMap + (iVType -> (subMap + (inVK -> inVUpd)))
                  case None =>
                    tMap + (iVType -> Map(inVK -> inVUpd))
                }))
                  .flatMap(
                    iVAdded =>
                      Some(iVAdded.copy(tMap0 = iVAdded.tMap.get(oVType) match {
                        case Some(subMap) =>
                          tMap + (oVType -> (subMap + (outVK -> outVUpd)))
                        case None =>
                          tMap + (oVType -> Map(outVK -> outVUpd))
                      }))
                  )
            )
          }
        )
    )
  }

  def containsV[K, V](vk: VertexKey[K, V])(implicit vType: VTs[K, V]): Boolean = vMap.contains((vType, vk))

  def copy(vMap0: Map[Any, Any] = vMap, eMap0: Map[Any, Any] = eMap, tMap0: Map[Any, Map[Any, Any]] = tMap): Graph[GS] =
    new Graph[GS] {
      val vMap: Map[Any, Any] = vMap0
      val eMap: Map[Any, Any] = eMap0
      val tMap: Map[Any, Map[Any, Any]] = tMap0
      val gs: GS = self.gs
    }

  //todo make private
  private[graph] def getV[VK, V](vk: VertexKey[VK, V])(implicit vType: VTs[VK, V]): Option[Vertex[VK, V]] =
    vMap.get((vType, vk)).asInstanceOf[Option[Vertex[VK, V]]]

  //todo make private
  private[graph] def getVs[VK, V](vType: VertexType[VK, V]): Map[VertexKey[VK, V], Vertex[VK, V]] =
    tMap
      .get(vType)
      .asInstanceOf[Option[Map[VertexKey[VK, V], Vertex[VK, V]]]]
      .getOrElse(Map.empty[VertexKey[VK, V], Vertex[VK, V]])

  //todo make private
  private[graph] def getEs[IK, IV, EK, E, OK, OV](
    eType: EdgeType[IK, IV, EK, E, OK, OV]
  ): Map[EdgeKey[EK, E], Edge[IK, IV, EK, E, OK, OV]] =
    tMap
      .get(eType)
      .asInstanceOf[Option[Map[EdgeKey[EK, E], Edge[IK, IV, EK, E, OK, OV]]]]
      .getOrElse(Map.empty[EdgeKey[EK, E], Edge[IK, IV, EK, E, OK, OV]])

}

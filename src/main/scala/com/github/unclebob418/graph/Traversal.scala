package com.github.unclebob418.graph

sealed trait Traversal[GS <: GraphSchema] extends Schema[GS]

//a traversal that ends at a vertex of type K0, V0
//todo pass self along
sealed trait VertexTraversal[K0, V0, GS <: GraphSchema] extends Traversal[GS] { self =>

  val vType: VertexType[K0, V0]

  def has(p: V0 => Boolean): VertexTraversal[K0, V0, GS] = VertexTraversal.Has(p)(self.vType, self.gs)

  def inV[IV, IK](iVType: VTs[IK, IV])(
    implicit ct: CTs[IK, IV, K0, V0]
  ): VertexTraversal[IK, IV, GS] = VertexTraversal.VTraversal(iVType, self.gs)

  def outV[OV, OK](oVType: VTs[OK, OV])(
    implicit ct: CTs[K0, V0, OK, OV]
  ): VertexTraversal[OK, OV, GS] = VertexTraversal.VTraversal(oVType, self.gs)

  def outE[EK, E, OV, OK](eType: ETs[K0, V0, EK, E, OK, OV]): EdgeTraversal.ETraversal[K0, V0, OK, OV, EK, E, GS] =
    EdgeTraversal.ETraversal(eType)(self.gs)
}

object VertexTraversal {
  sealed case class VertexSource[K, V, GS <: GraphSchema] private (graph: Graph[GS], vType: VertexType[K, V])
      extends VertexTraversal[K, V, GS] {
    val gs: GS = graph.gs
  }
  sealed case class VTraversal[K, V, GS <: GraphSchema] private (vType: VertexType[K, V], gs: GS)
      extends VertexTraversal[K, V, GS]

  sealed case class Has[K, V, GS <: GraphSchema](p: V => Boolean)(val vType: VertexType[K, V], val gs: GS)
      extends VertexTraversal[K, V, GS]
}

sealed trait EdgeTraversal[IK, IV, OK, OV, K0, E0, GS <: GraphSchema] extends Traversal[GS] { self =>
  val eType: EdgeType[IK, IV, K0, E0, OK, OV]

  def has(p: E0 => Boolean): EdgeTraversal.Has[IK, IV, OK, OV, K0, E0, GS] = EdgeTraversal.Has(p)(self.gs)(self.eType)

  /*def inV(iVType: VTs[IK, IV])(
    implicit ct: CTs[IK, IV, K0, ]
  ): VertexTraversal[IK, IV, GS] = VertexTraversal.VTraversal(iVType, self.gs)
   */
  def outV(implicit oVType: VTs[OK, OV]): VertexTraversal[OK, OV, GS] =
    VertexTraversal.VTraversal(oVType, self.gs)
}
//def has(p: E0 => Boolean): ETraversal.Has[IK, IV, OK, OV, K0, E0, GS] = ETraversal.Has(p)(self.gs, self.eType)

object EdgeTraversal {

  sealed case class EdgeSource[IK, IV, OK, OV, K0, E0, GS <: GraphSchema] private (graph0: Graph[GS])(
    val eType: EdgeType[IK, IV, K0, E0, OK, OV]
  ) extends EdgeTraversal[IK, IV, OK, OV, K0, E0, GS] {
    override val gs: GS = graph0.gs
  }

  sealed case class Has[IK, IV, OK, OV, K0, E0, GS <: GraphSchema] private (p: E0 => Boolean)(val gs: GS)(
    implicit val eType: EdgeType[IK, IV, K0, E0, OK, OV]
  ) extends EdgeTraversal[IK, IV, OK, OV, K0, E0, GS]

  sealed case class ETraversal[IK, IV, OK, OV, K0, E0, GS <: GraphSchema] private (
    val eType: EdgeType[IK, IV, K0, E0, OK, OV]
  )(val gs: GS)
      extends EdgeTraversal[IK, IV, OK, OV, K0, E0, GS]
}

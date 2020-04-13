package com.github.unclebob418.graph

import com.github.unclebob418.graph.VTraversal.VertexTraversal

sealed trait Traversal[GS <: GraphSchema] {

}

//a traversal that ends at a vertex of type K, V
sealed trait VTraversal[K, V, GS <: GraphSchema] extends Traversal[GS] { self =>
  type ET         = gs.ET
  type VT[K0, V0] = gs.VT[K0, V0]

  val gs: GS


  val vType: VertexType[K, V]

  def has(p: V => Boolean): VTraversal[K, V, GS] = VTraversal.Has(p)(self.gs, self.vType)

  def outV[OV, OK](oVType: VT[OK, OV])(
    implicit eType: ET {
      type In  = gs.VT[K, V]
      type Out = gs.VT[OK, OV]
    }
  ): VertexTraversal[OK, OV, GS] = VertexTraversal(self.gs, oVType)
}

object VTraversal {
  sealed case class VertexSource[K, V, GS <: GraphSchema] private (graph: Graph[GS])(val gs: GS, val vType: VertexType[K, V])             extends VTraversal[K, V, GS]
  sealed case class VertexTraversal[K, V, GS](gs: GS, vType: VertexType[K, V])              extends VTraversal[K, V, GS]
  sealed case class Has[K, V, GS](p: V => Boolean)(val gs: GS, val vType: VertexType[K, V]) extends VTraversal[K, V, GS]
}

/*sealed trait ETraversal[IK, IV, OK, OV, K0, E0, GS] extends Traversal[GS] { self =>
  val eType: ET {
    type In  = VT[IK, IV]
    type K   = K0
    type E   = E0
    type Out = VT[OK, OV]
  }


  def has(p: E0 => Boolean): ETraversal.Has = ETraversal.Has(p)(self.gs)
}

object ETraversal {

  //sealed case class EdgeSource[K, E, GS](eType: ET)(implicit val gs: GS)   extends VTraversal[K, E, GS]
  sealed case class Has[K, E, GS](p: E => Boolean)(implicit val gs: GS) extends ETraversal[K, E, GS]
}*/

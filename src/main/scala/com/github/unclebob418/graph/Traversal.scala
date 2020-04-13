package com.github.unclebob418.graph

import com.github.unclebob418.graph.VTraversal.VertexTraversal

sealed trait Traversal[GS <: GraphSchema] {
  type ET         = gs.ET
  type VT[K0, V0] = gs.VT[K0, V0]

  val gs: GS
}

//a traversal that ends at a vertex of type K, V
sealed trait VTraversal[K0, V, GS <: GraphSchema] extends Traversal[GS] { self =>

  val vType: VertexType[K0, V]

  def has(p: V => Boolean): VTraversal[K0, V, GS] = VTraversal.Has(p)(self.gs, self.vType)

  def inV[IV, IK](iVType: VT[IK, IV])(
    implicit eType: ET {
      type In  = VT[IK, IV]
      type Out = VT[K0, V]
    }
  ): VertexTraversal[IK, IV, GS] = VertexTraversal(self.gs, iVType)

  def outV[OV, OK](oVType: VT[OK, OV])(
    implicit eType: ET {
      type In  = VT[K0, V]
      type Out = VT[OK, OV]
    }
  ): VertexTraversal[OK, OV, GS] = VertexTraversal(self.gs, oVType)

  /* def inE(eType: ET {
    type Out = VT[OK, OV]
  } )*/
}

object VTraversal {
  sealed case class VertexSource[K, V, GS <: GraphSchema] private (graph: Graph[GS])(
    val gs: GS,
    val vType: VertexType[K, V]
  ) extends VTraversal[K, V, GS]
  sealed case class VertexTraversal[K, V, GS <: GraphSchema](gs: GS, vType: VertexType[K, V])
      extends VTraversal[K, V, GS]
  sealed case class Has[K, V, GS <: GraphSchema](p: V => Boolean)(val gs: GS, val vType: VertexType[K, V])
      extends VTraversal[K, V, GS]
}

/*
sealed trait ETraversal[IK, IV, OK, OV, K0, E0, GS <: GraphSchema] extends Traversal[GS] { self =>
  val eType: ET {
    type In  = gs.VT[IK, IV]
    type K   = K0
    type E   = E0
    type Out = gs.VT[OK, OV]
  }

  def has(p: E0 => Boolean): ETraversal.Has[IK, IV, OK, OV, K0, E0, GS] = ETraversal.Has(p)(self.gs, self.eType)
}

object ETraversal {

  sealed case class EdgeSource[IK, IV, OK, OV, K0, E0, GS <: GraphSchema](graph: Graph[GS])(
    val gs: GS,
    val eType: self.gs.ET {
      type In  = self.gs.VT[IK, IV]
      type K   = K0
      type E   = E0
      type Out = self.gs.VT[OK, OV]
    }
  ) extends ETraversal[IK, IV, OK, OV, K0, E0, GS] { self =>
  }
  sealed case class Has[IK, IV, OK, OV, K0, E0, GS <: GraphSchema](p: E0 => Boolean)(val gs: GS, val eType: self.gs.ET {
    type In  = self.gs.VT[IK, IV]
    type K   = K0
    type E   = E0
    type Out = self.gs.VT[OK, OV]
  }) extends ETraversal[IK, IV, OK, OV, K0, E0, GS] { self =>
  }
}
*/

package com.github.unclebob418.graph

import com.github.unclebob418.graph.VTraversal.VertexTraversal

sealed trait Traversal[GS <: GraphSchema] extends Schema[GS]

//a traversal that ends at a vertex of type K, V
sealed trait VTraversal[K0, V0, GS <: GraphSchema] extends Traversal[GS] { self =>

  val vType: VertexType[K0, V0]

  def has(p: V0 => Boolean): VTraversal[K0, V0, GS] = VTraversal.Has(p)(self.gs, self.vType)

  def inV[IV, IK](iVType: VTs[IK, IV])(
    implicit eType: ET {
      type In  = VTs[IK, IV]
      type Out = VTs[K0, V0]
    }
  ): VertexTraversal[IK, IV, GS] = VertexTraversal(self.gs, iVType)

  def outV[OV, OK](oVType: VTs[OK, OV])(
    implicit eType: ET {
      type In  = VTs[K0, V0]
      type Out = VTs[OK, OV]
    }
  ): VertexTraversal[OK, OV, GS] = VertexTraversal(self.gs, oVType)

  /* def inE(eType: ET {
    type Out = VT[OK, OV]
  } )*/
}

object VTraversal {
  sealed case class VertexSource[K, V, GS <: GraphSchema] private (graph: Graph[GS],
    val vType: VertexType[K, V]
  ) extends VTraversal[K, V, GS] {
    val gs: GS = graph.gs
  }
  sealed case class VertexTraversal[K, V, GS <: GraphSchema](gs: GS, vType: VertexType[K, V])
      extends VTraversal[K, V, GS]
  sealed case class Has[K, V, GS <: GraphSchema](p: V => Boolean)(val gs: GS, val vType: VertexType[K, V])
      extends VTraversal[K, V, GS]
}


sealed trait ETraversal[IK, IV, OK, OV, K0, E0, GS <: GraphSchema] extends Traversal[GS] { self =>
  val eType: ET {
    type In  = VTs[IK, IV]
    type K   = K0
    type E   = E0
    type Out = VTs[OK, OV]
  }
  //def has(p: E0 => Boolean): ETraversal.Has[IK, IV, OK, OV, K0, E0, GS] = ETraversal.Has(p)(self.gs, self.eType)
}

object ETraversal {

  sealed case class EdgeSource[IK, IV, OK, OV, K0, E0, GS <: GraphSchema](graph0: Graph[GS])(
    val eType: graph0.gs.ET {
      type In  = graph0.gs.VTs[IK, IV]
      type K   = K0
      type E   = E0
      type Out = graph0.gs.VTs[OK, OV]
    }
  ) extends ETraversal[IK, IV, OK, OV, K0, E0, GS]

  /*sealed case class Has[IK, IV, OK, OV, K0, E0, GS <: GraphSchema](p: E0 => Boolean)(val gs: GS, val eType: self.gs.ET {
    type In  = self.gs.VT[IK, IV]
    type K   = K0
    type E   = E0
    type Out = self.gs.VT[OK, OV]
  }) extends ETraversal[IK, IV, OK, OV, K0, E0, GS] { self =>
  }*/
}

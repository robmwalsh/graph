package com.github.unclebob418.graph

import com.github.unclebob418.graph.Traversal.TraversalStep.EdgeTraversal.ETraversal
import com.github.unclebob418.graph.Traversal.TraversalStep.VertexTraversal
import com.github.unclebob418.graph.Traversal.TraversalStep.VertexTraversal.VTraversal

sealed trait Traversal[GS <: GraphSchema] extends Schema[GS]
object Traversal {

  /**
   * The source of a traversal of a Graph[GS] (Empty traversal)
   */
  trait TraversalSource[GS <: GraphSchema] extends Traversal[GS] { self =>
    def V[K, V](vType: VTs[K, V]): VertexTraversal[K, V, GS] =
      VTraversal(vType)(self)

    def E[IK, IV, OK, OV, K0, E0](eType: ETs[IK, IV, K0, E0, OK, OV]): ETraversal[IK, IV, OK, OV, K0, E0, GS] =
      ETraversal(eType)(self)
  }
  object TraversalSource {

    sealed case class Anonymous[GS <: GraphSchema](gs: GS) extends TraversalSource[GS]

    sealed case class GraphTraversalSource[GS <: GraphSchema] private (graph: Graph[GS]) extends TraversalSource[GS] {
      val gs: GS = graph.gs
    }
  }
  sealed trait TraversalStep[GS <: GraphSchema] extends Traversal[GS] {
    val tail: Traversal[GS]
    val gs: GS = tail.gs
  }
  object TraversalStep {

    /**
     * a traversal that ends at a VertexType[VK, V]
     * @tparam GS the graph schema
     */
    sealed trait VertexTraversal[VK, V, GS <: GraphSchema] extends TraversalStep[GS] { self =>

      val vType: VertexType[VK, V]

      def has(p: V => Boolean): VertexTraversal[VK, V, GS] = VertexTraversal.Has(p)(self.vType)(self)

      def inV[IV, IK](iVType: VTs[IK, IV])(
        implicit ct: CTs[IK, IV, VK, V]
      ): VertexTraversal[IK, IV, GS] = VertexTraversal.VTraversal(iVType)(self)

      def outV[OV, OK](oVType: VTs[OK, OV])(
        implicit ct: CTs[VK, V, OK, OV]
      ): VertexTraversal[OK, OV, GS] = VertexTraversal.VTraversal(oVType)(self)

      def outE[EK, E, OV, OK](eType: ETs[VK, V, EK, E, OK, OV]): EdgeTraversal.ETraversal[VK, V, OK, OV, EK, E, GS] =
        EdgeTraversal.ETraversal(eType)(self)
    }

    object VertexTraversal {

      sealed case class VTraversal[K, V, GS <: GraphSchema] private (vType: VertexType[K, V])(
        val tail: Traversal[GS]
      ) extends VertexTraversal[K, V, GS]

      sealed case class Has[K, V, GS <: GraphSchema](p: V => Boolean)(val vType: VertexType[K, V])(
        val tail: Traversal[GS]
      ) extends VertexTraversal[K, V, GS]
    }

    /**
     * A traversal of a Graph[GS] that ends at an `EdgeType[IK, IV, VK, E, OK, OV]`
     * @tparam GS the graph schema
     */
    sealed trait EdgeTraversal[IK, IV, OK, OV, VK, E, GS <: GraphSchema] extends TraversalStep[GS] { self =>
      val eType: EdgeType[IK, IV, VK, E, OK, OV]

      def has(p: E => Boolean): EdgeTraversal.Has[IK, IV, OK, OV, VK, E, GS] =
        EdgeTraversal.Has(p)(self)(self.eType)

      def inV(implicit iVType: VTs[IK, IV]): VertexTraversal[IK, IV, GS] =
        VertexTraversal.VTraversal(iVType)(self)

      def outV(implicit oVType: VTs[OK, OV]): VertexTraversal[OK, OV, GS] =
        VertexTraversal.VTraversal(oVType)(self)
    }

    object EdgeTraversal {

      sealed case class Has[IK, IV, OK, OV, K0, E0, GS <: GraphSchema] private (p: E0 => Boolean)(
        val tail: Traversal[GS]
      )(
        implicit val eType: EdgeType[IK, IV, K0, E0, OK, OV]
      ) extends EdgeTraversal[IK, IV, OK, OV, K0, E0, GS]

      sealed case class ETraversal[IK, IV, OK, OV, K0, E0, GS <: GraphSchema] private (
        val eType: EdgeType[IK, IV, K0, E0, OK, OV]
      )(
        val tail: Traversal[GS]
      ) extends EdgeTraversal[IK, IV, OK, OV, K0, E0, GS]
    }
  }
}

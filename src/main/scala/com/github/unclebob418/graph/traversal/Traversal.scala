package com.github.unclebob418.graph.traversal

import com.github.unclebob418.graph._
import com.github.unclebob418.graph.traversal.Traversal.Source.GraphTraversalSource
import com.github.unclebob418.graph.traversal.Traversal.Step.EdgeTraversal.ETraversal
import com.github.unclebob418.graph.traversal.Traversal.Step.VertexTraversal
import com.github.unclebob418.graph.traversal.Traversal.Step.VertexTraversal.{ VSource, VTraversal }

sealed trait Traversal[+VK, +V, +EK, +E, GS <: GraphSchema] extends Schema[GS]
object Traversal {

  /**
   * The source of a traversal of a Graph[GS] (Empty traversal)
   */
  trait Source[GS <: GraphSchema] extends Traversal[Nothing, Nothing, Nothing, Nothing, GS] { self =>
    def V[VK, V](vType: VTs[VK, V]): VertexTraversal[VK, V, GS] =
      VTraversal(vType, self)

    def E[IK, IV, EK, E, OK, OV](eType: ETs[IK, IV, EK, E, OK, OV]): ETraversal[IK, IV, EK, E, OK, OV, GS] =
      ETraversal(eType, self)
  }
  object Source {

    sealed case class GraphTraversalSource[GS <: GraphSchema] private (graph: Graph[GS]) extends Source[GS] {
      val gs: GS = graph.gs
    }
  }
  sealed trait Step[+VK, +V, +EK, +E, GS <: GraphSchema] extends Traversal[VK, V, EK, E, GS] { self =>
    type TailVK
    type TailV
    type TailEK
    type TailE
    val tail: Traversal[TailVK, TailV, TailEK, TailE, GS]
    val gs: GS = tail.gs

    //def interpret[A](i: TraversalInterpreter[A]): A = i.interpret(self)
  }
  object Step {

    /**
     * a traversal that ends at a VertexType[VK, V]
     * @tparam GS the graph schema
     */
    sealed trait VertexTraversal[VK, V, GS <: GraphSchema] extends Step[VK, V, Nothing, Nothing, GS] { self =>

      val vType: VertexType[VK, V]

      def has(p: V => Boolean): VertexTraversal.Has[VK, V, GS] = VertexTraversal.Has(p, self)

      def inV[IV, IK](iVType: VTs[IK, IV])(
        implicit ct: CTs[IK, IV, VK, V]
      ): VertexTraversal[IK, IV, GS] = VertexTraversal.VTraversal(iVType, self)

      def outV[OV, OK](oVType: VTs[OK, OV])(
        implicit ct: CTs[VK, V, OK, OV]
      ): VertexTraversal[OK, OV, GS] = VertexTraversal.VTraversal(oVType, self)

      def outE[EK, E, OV, OK](eType: ETs[VK, V, EK, E, OK, OV]): EdgeTraversal.ETraversal[VK, V, EK, E, OK, OV, GS] =
        EdgeTraversal.ETraversal(eType, self)
    }

    object VertexTraversal {
      sealed trait VSource[VK, V, GS <: GraphSchema] extends VertexTraversal[VK, V, GS] {
        override val tail: GraphTraversalSource[GS]
        type TailVK = Nothing
        type TailV  = Nothing
        type TailEK = Nothing
        type TailE  = Nothing
      }
      object VSource {
        def apply[VK, V, GS <: GraphSchema](
          vType0: VertexType[VK, V],
          tail1: GraphTraversalSource[GS]
        ): VSource[VK, V, GS] = new VSource[VK, V, GS] {
          val vType: VertexType[VK, V]       = vType0
          val tail: GraphTraversalSource[GS] = tail1: GraphTraversalSource[GS]
        }
      }

      sealed trait VTraversal[VK, V, GS <: GraphSchema] extends VertexTraversal[VK, V, GS]
      object VTraversal {
        def apply[TailVK0, TailV0, TailEK0, TailE0, K, V, GS <: GraphSchema](
          vType0: VertexType[K, V],
          tail1: Traversal[TailVK0, TailV0, TailEK0, TailE0, GS]
        ): VTraversal[K, V, GS] = new VTraversal[K, V, GS] {
          val vType = vType0
          override type TailVK = TailVK0
          override type TailV  = TailV0
          override type TailEK = TailEK0
          override type TailE  = TailE0
          val tail = tail1
        }
      }

      sealed trait Has[VK, V, GS <: GraphSchema] extends VertexTraversal[VK, V, GS] {
        override type TailVK = VK
        override type TailV  = V
        override type TailEK = Nothing
        override type TailE  = Nothing
        override val tail: VertexTraversal[VK, V, GS]
        val p: V => Boolean

      }
      object Has {
        def apply[K, V, GS <: GraphSchema](p0: V => Boolean, tail0: VertexTraversal[K, V, GS]): Has[K, V, GS] =
          new Has[K, V, GS] {
            val vType                                    = tail0.vType
            override val tail: VertexTraversal[K, V, GS] = tail0
            val p                                        = p0
          }
      }
    }

    //todo edgetype + vertex type down
    /**
     * A traversal of a Graph[GS] that ends at an `EdgeType[IK, IV, VK, E, OK, OV]`
     * @tparam GS the graph schema
     */
    sealed trait EdgeTraversal[IK, IV, EK, E, OK, OV, GS <: GraphSchema] extends Step[Nothing, Nothing, EK, E, GS] {
      self =>
      val eType: EdgeType[IK, IV, EK, E, OK, OV]

      def has(p: E => Boolean): EdgeTraversal.Has[IK, IV, EK, E, OK, OV, GS] =
        EdgeTraversal.Has(p, self)

      def inV(implicit iVType: VTs[IK, IV]): VertexTraversal[IK, IV, GS] =
        VertexTraversal.VTraversal(iVType, self)

      def outV(implicit oVType: VTs[OK, OV]): VertexTraversal[OK, OV, GS] =
        VertexTraversal.VTraversal(oVType, self)
    }

    object EdgeTraversal {

      sealed trait ESource[IK, IV, EK, E, OK, OV, GS <: GraphSchema] extends EdgeTraversal[IK, IV, EK, E, OK, OV, GS] {
        override val tail: GraphTraversalSource[GS]
        type TailVK = Nothing
        type TailV  = Nothing
        type TailEK = Nothing
        type TailE  = Nothing
      }
      object ESource {
        def apply[IK, IV, EK, E, OK, OV, GS <: GraphSchema](
          eType0: EdgeType[IK, IV, EK, E, OK, OV],
          gts: GraphTraversalSource[GS]
        ): ESource[IK, IV, EK, E, OK, OV, GS] =
          new ESource[IK, IV, EK, E, OK, OV, GS] {
            override val tail: GraphTraversalSource[GS]         = gts
            override val eType: EdgeType[IK, IV, EK, E, OK, OV] = eType0
          }
      }

      sealed trait ETraversal[IK, IV, EK, E, OK, OV, GS <: GraphSchema] extends EdgeTraversal[IK, IV, EK, E, OK, OV, GS]
      object ETraversal {
        def apply[TailVK0, TailV0, TailEK0, TailE0, IK, IV, EK, E, OK, OV, GS <: GraphSchema](
          eType0: EdgeType[IK, IV, EK, E, OK, OV],
          tail0: Traversal[TailVK0, TailV0, TailEK0, TailE0, GS]
        ): ETraversal[IK, IV, EK, E, OK, OV, GS] = new ETraversal[IK, IV, EK, E, OK, OV, GS] {
          override val eType: EdgeType[IK, IV, EK, E, OK, OV] = eType0
          override type TailVK = TailVK0
          override type TailV  = TailV0
          override type TailEK = TailEK0
          override type TailE  = TailE0
          override val tail: Traversal[TailVK0, TailV0, TailEK0, TailE0, GS] = tail0
        }
      }

      sealed trait Has[IK, IV, EK, E, OK, OV, GS <: GraphSchema] extends EdgeTraversal[IK, IV, EK, E, OK, OV, GS] {
        override type TailVK = Nothing
        override type TailV  = Nothing
        override type TailEK = EK
        override type TailE  = E
        override val tail: EdgeTraversal[IK, IV, EK, E, OK, OV, GS]
        val p: E => Boolean
      }
      object Has {
        def apply[IK, IV, EK, E, OK, OV, GS <: GraphSchema](
          p0: E => Boolean,
          tail0: EdgeTraversal[IK, IV, EK, E, OK, OV, GS]
        ): Has[IK, IV, EK, E, OK, OV, GS] =
          new Has[IK, IV, EK, E, OK, OV, GS] {
            override val tail: EdgeTraversal[IK, IV, EK, E, OK, OV, GS] = tail0
            override val eType: EdgeType[IK, IV, EK, E, OK, OV]         = tail0.eType
            override val p                                              = p0
          }

      }
    }
  }

}

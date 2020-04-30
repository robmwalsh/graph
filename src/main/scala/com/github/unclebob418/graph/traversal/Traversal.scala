package com.github.unclebob418.graph.traversal

import com.github.unclebob418.graph._
import com.github.unclebob418.graph.traversal.Traversal.Step.Aggregate.Count
import com.github.unclebob418.graph.traversal.Traversal.Step.EdgeTraversal.ESource
import com.github.unclebob418.graph.traversal.Traversal.Step.VertexTraversal.VSource

sealed trait Traversal[+K, +V, GS <: GraphSchema] extends Schema[GS]

object Traversal {

  /**
   * The source of a traversal of a Graph[GS] (Empty traversal)
   */
  sealed case class GraphTraversalSource[GS <: GraphSchema] private (gs: GS) extends Traversal[Nothing, Nothing, GS] {
    self =>

    def V[VK, V](vType: VTs[VK, V]): VSource[VK, V, GS] =
      VSource(vType, self)

    def E[IK, IV, EK, E, OK, OV](eType: ETs[IK, IV, EK, E, OK, OV]): ESource[IK, IV, EK, E, OK, OV, GS] =
      ESource(eType, self)
  }

  sealed trait Step[K, V, GS <: GraphSchema] extends Traversal[K, V, GS] { self =>
    type TK
    type TV

    type Result <: TraversalResult[K, V]

    val tail: Traversal[TK, TV, GS]
    val gs: GS

    def foldLeft[A](z: A)(f: (A, Traversal.Step[_, _, GS]) => A): A =
      self.tail match {
        case _: GraphTraversalSource[_] => f(z, self)
        case step: Step[TK, TV, GS]     => step.foldLeft(f(z, self))(f)
      }
    def foldRight[A](z: A)(f: (A, Traversal.Step[_, _, GS]) => A): A =
      foldLeft((a: A) => a)((g, a) => b => g(f(b, a)))(z)

    def count: Count[GS] = Count(self)
  }

  object Step {

    /**
     * a traversal that ends at a VertexType[VK, V]
     * @tparam GS the graph schema
     */
    sealed trait VertexTraversal[VK, V, GS <: GraphSchema] extends Step[VK, V, GS] { self =>

      override type Result = TraversalResult.VertexList[VK, V]

      val vType: VertexType[VK, V]

      def has(p: V => Boolean): VertexTraversal.VHas[VK, V, GS] = VertexTraversal.VHas(p, self)

      def inV[IV, IK](iVType: VTs[IK, IV])(
        implicit ct: CTs[IK, IV, VK, V]
      ): VertexTraversal.VTraversal[IK, IV, GS] = VertexTraversal.VTraversal(iVType, self)

      def outV[OV, OK](oVType: VTs[OK, OV])(
        implicit ct: CTs[VK, V, OK, OV]
      ): VertexTraversal.VTraversal[OK, OV, GS] = VertexTraversal.VTraversal(oVType, self)

      def outE[EK, E, OV, OK](eType: ETs[VK, V, EK, E, OK, OV]): EdgeTraversal.ETraversal[VK, V, EK, E, OK, OV, GS] =
        EdgeTraversal.ETraversal(eType, self)
    }

    object VertexTraversal {
      sealed trait VSource[VK, V, GS <: GraphSchema] extends VertexTraversal[VK, V, GS] {
        override val tail: GraphTraversalSource[GS]
        override type TK = Nothing
        override type TV = Nothing

      }
      object VSource {
        def apply[VK, V, GS <: GraphSchema](
          vType0: VertexType[VK, V],
          tail1: GraphTraversalSource[GS]
        ): VSource[VK, V, GS] = new VSource[VK, V, GS] {
          val vType: VertexType[VK, V]       = vType0
          val tail: GraphTraversalSource[GS] = tail1: GraphTraversalSource[GS]
          override val gs: GS                = tail.gs
        }
      }

      sealed trait VTraversal[VK, V, GS <: GraphSchema] extends VertexTraversal[VK, V, GS]
      object VTraversal {
        def apply[TK0, TV0, VK, V, GS <: GraphSchema](
          vType0: VertexType[VK, V],
          tail0: Traversal.Step[TK0, TV0, GS]
        ): VTraversal[VK, V, GS] = new VTraversal[VK, V, GS] {
          val vType: VertexType[VK, V] = vType0
          override type TK = TK0
          override type TV = TV0
          val tail: Step[TK, TV, GS] = tail0
          override val gs: GS        = tail.gs
        }
      }

      sealed trait VHas[VK, V, GS <: GraphSchema] extends VertexTraversal[VK, V, GS] {
        override type TK = VK
        override type TV = V
        override val tail: VertexTraversal[VK, V, GS]
        val p: V => Boolean
      }
      object VHas {
        def apply[K, V, GS <: GraphSchema](p0: V => Boolean, tail0: VertexTraversal[K, V, GS]): VHas[K, V, GS] =
          new VHas[K, V, GS] {
            val vType: VertexType[K, V]                  = tail0.vType
            override val tail: VertexTraversal[K, V, GS] = tail0
            override val gs: GS                          = tail.gs
            val p: V => Boolean                          = p0
          }
      }
    }

    //todo edgetype + vertex type down
    /**
     * A traversal of a Graph[GS] that ends at an `EdgeType[IK, IV, VK, E, OK, OV]`
     * @tparam GS the graph schema
     */
    sealed trait EdgeTraversal[IK, IV, EK, E, OK, OV, GS <: GraphSchema] extends Step[EK, E, GS] {
      self =>
      override type Result = TraversalResult.EdgeList[EK, E]
      val eType: EdgeType[_, _, EK, E, _, _]

      def has(
        p: E => Boolean
      ): EdgeTraversal.EHas[IK, IV, EK, E, OK, OV, GS] =
        EdgeTraversal.EHas(p, self)

      def inV[IK0 >: IK, IV0 >: IV](implicit iVType: VTs[IK0, IV0]): VertexTraversal.VTraversal[IK0, IV0, GS] =
        VertexTraversal.VTraversal(iVType, self)

      def outV[OK0 >: OK, OV0 >: OV](implicit oVType: VTs[OK0, OV0]): VertexTraversal.VTraversal[OK0, OV0, GS] =
        VertexTraversal.VTraversal(oVType, self)
    }

    object EdgeTraversal {

      sealed trait ESource[IK, IV, EK, E, OK, OV, GS <: GraphSchema] extends EdgeTraversal[IK, IV, EK, E, OK, OV, GS] {
        override val tail: GraphTraversalSource[GS]
        type TK = Nothing
        type TV = Nothing
      }
      object ESource {
        def apply[IK, IV, EK, E, OK, OV, GS <: GraphSchema](
          eType0: EdgeType[IK, IV, EK, E, OK, OV],
          gts: GraphTraversalSource[GS]
        ): ESource[IK, IV, EK, E, OK, OV, GS] =
          new ESource[IK, IV, EK, E, OK, OV, GS] {
            override val tail: GraphTraversalSource[GS]         = gts
            override val eType: EdgeType[IK, IV, EK, E, OK, OV] = eType0
            override val gs: GS                                 = tail.gs
          }
      }

      sealed trait ETraversal[IK, IV, EK, E, OK, OV, GS <: GraphSchema] extends EdgeTraversal[IK, IV, EK, E, OK, OV, GS]
      object ETraversal {
        def apply[TK0, TV0, IK, IV, EK, E, OK, OV, GS <: GraphSchema](
          eType0: EdgeType[IK, IV, EK, E, OK, OV],
          tail0: Traversal[TK0, TV0, GS]
        ): ETraversal[IK, IV, EK, E, OK, OV, GS] = new ETraversal[IK, IV, EK, E, OK, OV, GS] {
          override val eType: EdgeType[IK, IV, EK, E, OK, OV] = eType0
          override type TK = TK0
          override type TV = TV0
          override val tail: Traversal[TK, TV, GS] = tail0 //todo can this be
          override val gs: GS                      = tail.gs
        }
      }

      sealed trait EHas[IK, IV, EK, E, OK, OV, GS <: GraphSchema] extends EdgeTraversal[IK, IV, EK, E, OK, OV, GS] {
        override type TK = EK
        override type TV = E
        override val tail: EdgeTraversal[IK, IV, EK, E, OK, OV, GS]
        val p: E => Boolean
      }
      object EHas {
        def apply[IK, IV, EK, E, OK, OV, GS <: GraphSchema](
          p0: E => Boolean,
          tail0: EdgeTraversal[IK, IV, EK, E, OK, OV, GS]
        ): EHas[IK, IV, EK, E, OK, OV, GS] =
          new EHas[IK, IV, EK, E, OK, OV, GS] {
            override val tail: EdgeTraversal[IK, IV, EK, E, OK, OV, GS] = tail0
            override val gs: GS                                         = tail.gs
            override val eType: EdgeType[_, _, EK, E, _, _]             = tail0.eType
            override val p: E => Boolean                                = p0
          }
      }
    }

    sealed trait Aggregate[A, GS <: GraphSchema] extends Step[Any, A, GS]
    object Aggregate {
      sealed trait Count[GS <: GraphSchema] extends Aggregate[Int, GS]
      object Count {
        def apply[K,V, GS <: GraphSchema](tail0: Step[K, V, GS]): Count[GS] = new Count[GS]{
          override type TK = K
          override type TV = V
          override type Result = TraversalResult.Aggregate[Int]
          override val tail: Step[TK, TV, GS] = tail0
          override val gs: GS = tail0.gs
        }
      }
    }

  }
}

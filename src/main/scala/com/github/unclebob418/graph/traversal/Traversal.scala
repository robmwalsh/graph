package com.github.unclebob418.graph.traversal

import com.github.unclebob418.graph._
import com.github.unclebob418.graph.traversal.Traversal.Source.GraphTraversalSource
import com.github.unclebob418.graph.traversal.Traversal.Step.EdgeTraversal.ETraversal
import com.github.unclebob418.graph.traversal.Traversal.Step.VertexTraversal
import com.github.unclebob418.graph.traversal.Traversal.Step.VertexTraversal.VTraversal

sealed trait Traversal[+E, +V, GS <: GraphSchema] extends Schema[GS]
object Traversal {

  /**
   * The source of a traversal of a Graph[GS] (Empty traversal)
   */
  trait Source[GS <: GraphSchema] extends Traversal[Nothing, Nothing, GS] { self =>
    def V[K, V](vType: VTs[K, V]): VertexTraversal[K, V, GS] =
      VTraversal(vType, self)

    def E[IK, IV, OK, OV, K0, E0](eType: ETs[IK, IV, K0, E0, OK, OV]): ETraversal[IK, IV, OK, OV, K0, E0, GS] =
      ETraversal(eType, self)
  }
  object Source {

    sealed case class GraphTraversalSource[GS <: GraphSchema] private (graph: Graph[GS])
        extends Source[GS] {
      val gs: GS = graph.gs
    }
  }
  sealed trait Step[+E, +V, GS <: GraphSchema] extends Traversal[E, V, GS] { self =>
    type TailE
    type TailV
    val tail: Traversal[TailE, TailV, GS]
    val gs: GS = tail.gs

    //def interpret[A](i: TraversalInterpreter[A]): A = i.interpret(self)
  }
  object Step {

    /**
     * a traversal that ends at a VertexType[VK, V]
     * @tparam GS the graph schema
     */
    sealed trait VertexTraversal[VK, V, GS <: GraphSchema] extends Step[Nothing, V, GS] { self =>

      val vType: VertexType[VK, V]

      def has(p: V => Boolean): VertexTraversal[VK, V, GS] = VertexTraversal.Has(p, vType, self)

      def inV[IV, IK](iVType: VTs[IK, IV])(
        implicit ct: CTs[IK, IV, VK, V]
      ): VertexTraversal[IK, IV, GS] = VertexTraversal.VTraversal(iVType, self)

      def outV[OV, OK](oVType: VTs[OK, OV])(
        implicit ct: CTs[VK, V, OK, OV]
      ): VertexTraversal[OK, OV, GS] = VertexTraversal.VTraversal(oVType, self)

      def outE[EK, E, OV, OK](eType: ETs[VK, V, EK, E, OK, OV]): EdgeTraversal.ETraversal[VK, V, OK, OV, EK, E, GS] =
        EdgeTraversal.ETraversal(eType, self)
    }

    object VertexTraversal {
      sealed trait VSource[K, V, GS <: GraphSchema] extends VertexTraversal[K, V, GS] {
        override val tail: GraphTraversalSource[GS]
        override type TailE = Nothing
        override type TailV = Nothing
      }
      object VSource{
        def apply[TailE1, TailV1, K, V, GS <: GraphSchema](vType0: VertexType[K, V], tail1 : GraphTraversalSource[GS]): VSource[K,V, GS] = new VSource[K,V, GS]{
          val vType = vType0
          val tail = tail1 : GraphTraversalSource[GS]
        }
      }

      sealed trait VTraversal[K, V, GS <: GraphSchema] extends VertexTraversal[K, V, GS]
      object VTraversal{
        def apply[TailE1, TailV1, K, V, GS <: GraphSchema](vType0: VertexType[K, V], tail1 : Traversal[TailE1, TailV1, GS]): VTraversal[K,V, GS] = new VTraversal[K,V, GS]{
          val vType = vType0
          override type TailE = TailE1
          override type TailV = TailV1
          val tail = tail1
        }
      }

      sealed trait Has[K, V, GS <: GraphSchema] extends VertexTraversal[K, V, GS] {
        override type TailV = V
        override type TailE = Nothing
        override val tail: VertexTraversal[K, V, GS]

      }
      object Has{
        def apply[K, V, GS <: GraphSchema](tail1 : VertexTraversal[K, V, GS], p1 : V => Boolean): Has[K,V, GS] = new Has[K,V, GS]{
          val vType = tail1.vType
          override val tail: VertexTraversal[K, V, GS] = tail1
          val p = p1
        }
      }
    }

    //todo edgetype + vertex type down
    /**
     * A traversal of a Graph[GS] that ends at an `EdgeType[IK, IV, VK, E, OK, OV]`
     * @tparam GS the graph schema
     */
    sealed trait EdgeTraversal[IK, IV, OK, OV, EK, E, GS <: GraphSchema] extends Step[GS] { self =>
      val eType: EdgeType[IK, IV, EK, E, OK, OV]

      def has(p: E => Boolean): EdgeTraversal.Has[IK, IV, OK, OV, EK, E, GS] =
        EdgeTraversal.Has(p, eType, self)

      def inV(implicit iVType: VTs[IK, IV]): VertexTraversal[IK, IV, GS] =
        VertexTraversal.VTraversal(iVType, self)

      def outV(implicit oVType: VTs[OK, OV]): VertexTraversal[OK, OV, GS] =
        VertexTraversal.VTraversal(oVType, self)
    }

    object EdgeTraversal {

      sealed case class ESource[IK, IV, OK, OV, K0, E0, GS <: GraphSchema] private (
        eType: EdgeType[IK, IV, K0, E0, OK, OV],
        val tail: Traversal[GS]
      ) extends EdgeTraversal[IK, IV, OK, OV, K0, E0, GS]

      sealed case class ETraversal[IK, IV, OK, OV, K0, E0, GS <: GraphSchema] private (
        eType: EdgeType[IK, IV, K0, E0, OK, OV],
        val tail: Traversal[GS]
      ) extends EdgeTraversal[IK, IV, OK, OV, K0, E0, GS]

      sealed case class Has[IK, IV, OK, OV, K0, E0, GS <: GraphSchema] private (
        p: E0 => Boolean,
        eType: EdgeType[IK, IV, K0, E0, OK, OV],
        tail: Traversal[GS]
      ) extends EdgeTraversal[IK, IV, OK, OV, K0, E0, GS]

    }
  }
}

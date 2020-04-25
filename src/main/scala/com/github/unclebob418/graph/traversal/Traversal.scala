package com.github.unclebob418.graph.traversal

import com.github.unclebob418.graph._
import com.github.unclebob418.graph.traversal.Traversal.Step.EdgeTraversal.ESource
import com.github.unclebob418.graph.traversal.Traversal.Step.VertexTraversal.VSource

sealed trait Traversal[+VK, +V, +IK, +IV, +EK, +E, +OK, +OV, GS <: GraphSchema] extends Schema[GS] { self =>
  def foldLeft[VK0 >: VK, V0 >: V, IK0 >: IK, IV0 >: IV, EK0 >: EK, E0 >: E, OK0 >: OK, OV0 >: OV, A](
    ts: Traversal[VK0, V0, IK0, IV0, EK0, E0, OK0, OV0, GS]
  )(z: A)(f: (A, Traversal[_, _, _, _, _, _, _, _, GS]) => A): A = ts match {
    case step: Traversal.Step[VK, V, IK, IV, EK, E, OK, OV, GS] => foldLeft(step.tail)(f(z, ts))(f)
    case _: Traversal.GraphTraversalSource[_]                            => z
  }
  def foldRight[VK0 >: VK, V0 >: V, IK0 >: IK, IV0 >: IV, EK0 >: EK, E0 >: E, OK0 >: OK, OV0 >: OV, A](
    ts: Traversal[VK0, V0, IK0, IV0, EK0, E0, OK0, OV0, GS]
  )(z: A)(
    f: (A, Traversal[_, _, _, _, _, _, _, _, GS]) => A
  ) =
    foldLeft(ts)((a: A) => a)((g, a) => b => g(f(b, a)))(z)
}

object Traversal {

  /**
   * The source of a traversal of a Graph[GS] (Empty traversal)
   */
  sealed case class GraphTraversalSource[GS <: GraphSchema] private (graph: Graph[GS])
      extends Traversal[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, GS] { self =>
    val gs: GS = graph.gs

    def V[VK, V](vType: VTs[VK, V]) =
      VSource(vType, self)

    def E[IK, IV, EK, E, OK, OV](eType: ETs[IK, IV, EK, E, OK, OV]) =
      ESource(eType, self)
  }

  sealed trait Step[+VK, +V, +IK, +IV, +EK, +E, +OK, +OV, GS <: GraphSchema]
      extends Traversal[VK, V, IK, IV, EK, E, OK, OV, GS] { self =>
    type TailVK
    type TailV
    type TailIK
    type TailIV
    type TailEK
    type TailE
    type TailOK
    type TailOV
    val tail: Traversal[TailVK, TailV, TailIK, TailIV, TailEK, TailE, TailOK, TailOV, GS]
    val gs: GS

    //def interpret[A](i: TraversalInterpreter[A]): A = i.interpret(self)
  }
  object Step {

    /**
     * a traversal that ends at a VertexType[VK, V]
     * @tparam GS the graph schema
     */
    sealed trait VertexTraversal[VK, V, GS <: GraphSchema]
        extends Step[VK, V, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, GS] { self =>

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
        override type TailVK = Nothing
        override type TailV  = Nothing
        override type TailIK = Nothing
        override type TailIV = Nothing
        override type TailEK = Nothing
        override type TailE  = Nothing
        override type TailOK = Nothing
        override type TailOV = Nothing
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
        def apply[TailVK0, TailV0, TailIK0, TailIV0, TailEK0, TailE0, TailOK0, TailOV0, VK, V, GS <: GraphSchema](
          vType0: VertexType[VK, V],
          tail1: Traversal[TailVK0, TailV0, TailIK0, TailIV0, TailEK0, TailE0, TailOK0, TailOV0, GS]
        ): VTraversal[VK, V, GS] = new VTraversal[VK, V, GS] {
          val vType = vType0
          override type TailVK = TailVK0
          override type TailV  = TailV0
          override type TailIK = TailIK0
          override type TailIV = TailIV0
          override type TailEK = TailEK0
          override type TailE  = TailE0
          override type TailOK = TailOK0
          override type TailOV = TailOV0
          val tail            = tail1
          override val gs: GS = tail.gs
        }
      }

      sealed trait Has[VK, V, GS <: GraphSchema] extends VertexTraversal[VK, V, GS] {
        override type TailVK = VK
        override type TailV  = V
        override type TailIK = Nothing
        override type TailIV = Nothing
        override type TailEK = Nothing
        override type TailE  = Nothing
        override type TailOK = Nothing
        override type TailOV = Nothing
        override val tail: VertexTraversal[VK, V, GS]
        val p: V => Boolean
      }
      object Has {
        def apply[K, V, GS <: GraphSchema](p0: V => Boolean, tail0: VertexTraversal[K, V, GS]): Has[K, V, GS] =
          new Has[K, V, GS] {
            val vType                                    = tail0.vType
            override val tail: VertexTraversal[K, V, GS] = tail0
            override val gs: GS                          = tail.gs
            val p                                        = p0
          }
      }
    }

    //todo edgetype + vertex type down
    /**
     * A traversal of a Graph[GS] that ends at an `EdgeType[IK, IV, VK, E, OK, OV]`
     * @tparam GS the graph schema
     */
    sealed trait EdgeTraversal[IK, IV, EK, E, OK, OV, GS <: GraphSchema]
        extends Step[Nothing, Nothing, IK, IV, EK, E, OK, OV, GS] {
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
        type TailIK = Nothing
        type TailIV = Nothing
        type TailEK = Nothing
        type TailE  = Nothing
        type TailOK = Nothing
        type TailOV = Nothing
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
        def apply[
          TailVK0,
          TailV0,
          TailIK0,
          TailIV0,
          TailEK0,
          TailE0,
          TailOK0,
          TailOV0,
          IK,
          IV,
          EK,
          E,
          OK,
          OV,
          GS <: GraphSchema
        ](
          eType0: EdgeType[IK, IV, EK, E, OK, OV],
          tail0: Traversal[TailVK0, TailV0, TailIK0, TailIV0, TailEK0, TailE0, TailOK0, TailOV0, GS]
        ): ETraversal[IK, IV, EK, E, OK, OV, GS] = new ETraversal[IK, IV, EK, E, OK, OV, GS] {
          override val eType: EdgeType[IK, IV, EK, E, OK, OV] = eType0
          override type TailVK = TailVK0
          override type TailV  = TailV0
          override type TailIK = TailIK0
          override type TailIV = TailIV0
          override type TailEK = TailEK0
          override type TailE  = TailE0
          override type TailOK = TailOK0
          override type TailOV = TailOV0
          override val tail: Traversal[TailVK0, TailV0, TailIK0, TailIV0, TailEK0, TailE0, TailOK0, TailOV0, GS] = tail0
          override val gs: GS                                                                                    = tail.gs
        }
      }

      sealed trait Has[IK, IV, EK, E, OK, OV, GS <: GraphSchema] extends EdgeTraversal[IK, IV, EK, E, OK, OV, GS] {
        override type TailVK = Nothing
        override type TailV  = Nothing
        type TailIK          = IK
        type TailIV          = IV
        type TailEK          = EK
        type TailE           = E
        type TailOK          = OK
        type TailOV          = OV
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
            override val gs: GS                                         = tail.gs
            override val eType: EdgeType[IK, IV, EK, E, OK, OV]         = tail0.eType
            override val p                                              = p0
          }

      }
    }
  }

}

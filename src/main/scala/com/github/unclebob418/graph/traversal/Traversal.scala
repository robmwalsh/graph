package com.github.unclebob418.graph.traversal

import com.github.unclebob418.graph._
import com.github.unclebob418.graph.traversal.Traversal.Step.EdgeTraversal.{ EHas, ESource, ETraversal }
import com.github.unclebob418.graph.traversal.Traversal.Step.VertexTraversal
import com.github.unclebob418.graph.traversal.Traversal.Step.VertexTraversal.{ VHas, VSource, VTraversal }

sealed trait Traversal[+VK, +V, +IK, +IV, +EK, +E, +OK, +OV, GS <: GraphSchema] extends Schema[GS]

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

    def foldLeft[VK0 >: VK, V0 >: V, IK0 >: IK, IV0 >: IV, EK0 >: EK, E0 >: E, OK0 >: OK, OV0 >: OV, A](
      z: A
    )(f: (A, Traversal.Step[_, _, _, _, _, _, _, _, GS]) => A): A =
      self.tail match {
        case _: GraphTraversalSource[_] => f(z, self)
        case step: Step[TailVK, TailV, TailIK, TailIV, TailEK, TailE, TailOK, TailOV, GS] =>
          step.foldLeft(f(z, self))(f)
      }
    def foldRight[VK0 >: VK, V0 >: V, IK0 >: IK, IV0 >: IV, EK0 >: EK, E0 >: E, OK0 >: OK, OV0 >: OV, A](
      z: A
    )(f: (A, Traversal.Step[_, _, _, _, _, _, _, _, GS]) => A) =
      foldLeft((a: A) => a)((g, a) => b => g(f(b, a)))(z)
  }

  /*
    case vSource: VSource[TailVK, TailV, GS]                                 => vSource.foldLeft(f(z, self))(f)
        case traversal: VTraversal[TailVK, TailV, GS]                            => traversal.foldLeft(f(z, self))(f)
        case vHas: VHas[TailVK, TailV, GS]                                       => vHas.foldLeft(f(z, self))(f)
        case eSource: ESource[TailIK, TailIV, TailEK, TailE, TailOK, TailOV, GS] => eSource.foldLeft(f(z, self))(f)
        case traversal: ETraversal[TailIK, TailIV, TailEK, TailE, TailOK, TailOV, GS] =>
          traversal.foldLeft(f(z, self))(f)
        case eHas: EHas[TailIK, TailIV, TailEK, TailE, TailOK, TailOV, GS] => eHas.foldLeft(f(z, self))(f)
        case _: GraphTraversalSource[GS]                                   => f(z, self)
   */
  object Step {

    /**
     * a traversal that ends at a VertexType[VK, V]
     * @tparam GS the graph schema
     */
    sealed trait VertexTraversal[VK, V, GS <: GraphSchema]
        extends Step[VK, V, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, GS] { self =>

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
          tail1: Traversal.Step[TailVK0, TailV0, TailIK0, TailIV0, TailEK0, TailE0, TailOK0, TailOV0, GS]
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

      sealed trait VHas[VK, V, GS <: GraphSchema] extends VertexTraversal[VK, V, GS] {
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
      object VHas {
        def apply[K, V, GS <: GraphSchema](p0: V => Boolean, tail0: VertexTraversal[K, V, GS]): VHas[K, V, GS] =
          new VHas[K, V, GS] {
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

      def has(p: E => Boolean): EdgeTraversal.EHas[IK, IV, EK, E, OK, OV, GS] =
        EdgeTraversal.EHas(p, self)

      def inV(implicit iVType: VTs[IK, IV]): VertexTraversal.VTraversal[IK, IV, GS] =
        VertexTraversal.VTraversal(iVType, self)

      def outV(implicit oVType: VTs[OK, OV]): VertexTraversal.VTraversal[OK, OV, GS] =
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

      sealed trait EHas[IK, IV, EK, E, OK, OV, GS <: GraphSchema] extends EdgeTraversal[IK, IV, EK, E, OK, OV, GS] {
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
      object EHas {
        def apply[IK, IV, EK, E, OK, OV, GS <: GraphSchema](
          p0: E => Boolean,
          tail0: EdgeTraversal[IK, IV, EK, E, OK, OV, GS]
        ): EHas[IK, IV, EK, E, OK, OV, GS] =
          new EHas[IK, IV, EK, E, OK, OV, GS] {
            override val tail: EdgeTraversal[IK, IV, EK, E, OK, OV, GS] = tail0
            override val gs: GS                                         = tail.gs
            override val eType: EdgeType[IK, IV, EK, E, OK, OV]         = tail0.eType
            override val p                                              = p0
          }

      }
    }
  }

}

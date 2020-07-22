package com.github.unclebob418.graph.traversal

import com.github.unclebob418.graph.Key.{ EdgeKey, VertexKey }
import com.github.unclebob418.graph.Type.{ EdgeType, VertexType }
import com.github.unclebob418.graph.traversal.Traversal.{ Filter, Step }
import com.github.unclebob418.graph.traversal.Traversal.Filter.Has
import com.github.unclebob418.graph.traversal.Traversal.FlatMap.{ Edge2Vertex, Vertex2Edge, Vertex2Vertex }
import com.github.unclebob418.graph.traversal.Traversal.Source.{ Anonymous, EdgeSource }
import com.github.unclebob418.graph.{ Graph, GraphSchema, Key, Schema }

//the state of a traversal at a particular step
sealed trait Traverser[V] {
  type K
  type Path[_]
  val path: HList.Aux[V] //how we got here, some sort of HList
  val location: Key[K, V]
  def value: V
}

//a traversal A => C
sealed trait Traversal[-A, C, GS <: GraphSchema] extends Schema[GS] { self =>
  type IK; type K; type OK
  type IV; type V; type OV

  def outV[OK0, OV0](oVType: VTs[OK0, OV0])(
    implicit ct0: CTs[K, V, OK0, OV0],
    ev: C =:= VertexKey[K, V]
  ) =
    new Vertex2Vertex.Out[A, VertexKey[K, V], VertexKey[OK0, OV0], GS] {
      type IK = Nothing; type K = OK0; type OK = Nothing
      type IV = Nothing; type V = OV0; type OV = Nothing

      override val from: Traversal[A, VertexKey[self.K, self.V], GS] =
        self.asInstanceOf[Traversal[A, VertexKey[self.K, self.V], GS]]
      override val gs: GS = self.gs
    }

  def inV[IK0, IV0](iVType: VTs[IK0, IV0])(
    implicit ct0: CTs[IK0, IV0, K, V],
    ev: C <:< VertexKey[K, V]
  ) =
    new Vertex2Vertex.In[A, VertexKey[K, V], VertexKey[IK0, IV0], GS] {
      type IK = Nothing; type K = IK0; type OK = Nothing
      type IV = Nothing; type V = IV0; type OV = Nothing
      override val from: Traversal[A, VertexKey[self.K, self.V], GS] =
        self.asInstanceOf[Traversal[A, VertexKey[self.K, self.V], GS]]
      override val gs: GS = self.gs
    }

  def outE[K0, V0, OK0, OV0](eType: ETs[K, V, K0, V0, OK0, OV0])(
    implicit ev: C <:< VertexKey[K, V]
  ) =
    new Vertex2Edge.Out[A, VertexKey[K, V], EdgeKey[K, V, K0, V0, OK0, OV0], GS, K, V] {
      override type IK = self.K; type K = K0; type OK = OK0
      override type IV = self.V; type V = V0; type OV = OV0

      override val from: Traversal[A, VertexKey[self.K, self.V], GS] =
        self.asInstanceOf[Traversal[A, VertexKey[self.K, self.V], GS]]
      override val gs: GS = self.gs
    }

  def inE[IK0, IV0, K0, V0](eType: ETs[IK0, IV0, K0, V0, K, V])(
    implicit ev: C <:< VertexKey[K, V]
  ) =
    new Vertex2Edge.In[A, VertexKey[K, V], EdgeKey[IK0, IV0, K0, V0, K, V], GS, K, V] {
      override type IK = IK0; type K = K0; type OK = self.K
      override type IV = IV0; type V = V0; type OV = self.V

      override val from: Traversal[A, VertexKey[self.K, self.V], GS] =
        self.asInstanceOf[Traversal[A, VertexKey[self.K, self.V], GS]]
      override val gs: GS = self.gs
    }

  def out(
    implicit oVType: VTs[OK, OV],
    ev: C <:< EdgeKey[IK, IV, K, V, OK, OV]
  ) =
    new Edge2Vertex.Out[A, EdgeKey[IK, IV, K, V, OK, OV], VertexKey[OK, OV], GS, OK, OV] {
      override type IK = Nothing; type K = self.OK; type OK = Nothing
      override type IV = Nothing; type V = self.OV; type OV = Nothing

      override val from: Traversal[A, EdgeKey[self.IK, self.IV, self.K, self.V, self.OK, self.OV], GS] =
        self.asInstanceOf[Traversal[A, EdgeKey[self.IK, self.IV, self.K, self.V, self.OK, self.OV], GS]]
      override val gs: GS = self.gs
    }

  def in(
    implicit iVType: VTs[IK, IV],
    ev: C <:< EdgeKey[IK, IV, K, V, OK, OV]
  ) =
    new Edge2Vertex.In[A, EdgeKey[IK, IV, K, V, OK, OV], VertexKey[IK, IV], GS, IK, IV] {
      override type IK = Nothing; type K = self.IK; type OK = Nothing
      override type IV = Nothing; type V = self.IV; type OV = Nothing

      override val from: Traversal[A, EdgeKey[self.IK, self.IV, self.K, self.V, self.OK, self.OV], GS] =
        self.asInstanceOf[Traversal[A, EdgeKey[self.IK, self.IV, self.K, self.V, self.OK, self.OV], GS]]
      override val gs: GS = self.gs
    }

  def id(implicit ev: C <:< VertexKey[K, V]) =
    new Traversal.Map.Id[A, VertexKey[K, V], K, GS] {
      override type IK = Nothing; type K = self.K; type OK  = Nothing
      override type IV = Nothing; type V = Nothing; type OV = Nothing

      val gs: GS = self.gs
      override val from: Traversal[A, VertexKey[self.K, self.V], GS] =
        self.asInstanceOf[Traversal[A, VertexKey[self.K, self.V], GS]]
    }

  def value(implicit ev: C <:< VertexKey[K, V]) =
    new Traversal.Map.Value[A, VertexKey[K, V], V, GS] {

      val gs: GS = self.gs
      override val from: Traversal[A, VertexKey[self.K, self.V], GS] =
        self.asInstanceOf[Traversal[A, VertexKey[self.K, self.V], GS]]
    }

  def map[D](p: C => D): Traversal.Map[A, C, D, GS] = ???

  def has(p0: self.type => Traversal[C, Boolean, GS]): Filter.Has[A, C, GS] =
    new Filter.Has[A, C, GS] {
      override type IK = self.IK; type K = self.K; type OK = self.OV
      override type IV = self.IV; type V = self.V; type OV = self.OK
      override val p: Traversal[C, Boolean, GS] = p0(self)
      override val gs: GS                       = self.gs
    }

}
object Traversal {

  //a source of traversers
  sealed trait Source[A, GS <: GraphSchema] extends Traversal[Any, A, GS] {
    //val graph: Graph[GS]
    val gs: GS
  }
  object Source {
    sealed trait VertexSource[VK0, V0, GS <: GraphSchema] extends Source[VertexKey[VK0, V0], GS] {
      val vt: VertexType[VK0, V0]
      val graph: Graph[GS]
      //todo Aux all the things
    }
    object VertexSource {
      def apply[VK0, V0, GS <: GraphSchema](graph0: Graph[GS], vt0: VertexType[VK0, V0]) =
        new VertexSource[VK0, V0, GS] {
          override type IK = Nothing; type K = VK0; type OK = Nothing
          override type IV = Nothing; type V = V0; type OV  = Nothing

          override val vt: VertexType[VK0, V0] = vt0
          override val graph: Graph[GS]        = graph0
          override val gs: GS                  = graph0.gs

        }
    }

    sealed trait EdgeSource[IK0, IV0, K0, V0, OK0, OV0, GS <: GraphSchema]
        extends Source[EdgeKey[IK0, IV0, K0, V0, OK0, OV0], GS] {
      val graph: Graph[GS]
      val et: EdgeType[IK0, IV0, K0, V0, OK0, OV0]
    }

    object EdgeSource {
      def apply[IK0, IV0, K0, V0, OK0, OV0, GS <: GraphSchema](
        graph0: Graph[GS],
        et0: EdgeType[IK0, IV0, K0, V0, OK0, OV0]
      ) = new EdgeSource[IK0, IV0, K0, V0, OK0, OV0, GS] {
        override type IK = IK0; type K = K0; type OK = OK0
        override type IV = IV0; type V = V0; type OV = OV0

        override val et: EdgeType[IK0, IV0, K0, V0, OK0, OV0] = et0
        override val graph: Graph[GS]                         = graph0
        override val gs: GS                                   = graph0.gs

      }
    }

    sealed trait Anonymous[A, GS <: GraphSchema] extends Source[A, GS] {
      //todo how do I get a GS?
      //todo how do I get an A?
    }
    object Anonymous {}

  }
  sealed trait Step[-A, B, C, GS <: GraphSchema] extends Traversal[A, C, GS] {
    val from: Traversal[A, B, GS]
  }

  //transforms a traverser of B of a traversal A => B to a traverser of C to yield a traversal A => C
  sealed trait Map[-A, B, C, GS <: GraphSchema] extends Step[A, B, C, GS]
  object Map {
    //maps to value
    trait Value[-A, B <: Key[_, C], C, GS <: GraphSchema] extends Map[A, B, C, GS]
    //maps to id
    trait Id[-A, B <: Key[C, _], C, GS <: GraphSchema] extends Map[A, B, C, GS]
    //map to type?
    //trait Map[-A, B, C, GS <: GraphSchema] extends Map[A, B, C, GS]
  }

  //transforms a traverser of B of a traversal A => B to 0 or more traversers of C to yield a traversal A => C
  trait FlatMap[-A, B, C, GS <: GraphSchema] extends Step[A, B, C, GS]
  object FlatMap {

    trait Move[-A, B <: Key[_, _], C <: Key[_, _], GS <: GraphSchema] extends FlatMap[A, B, C, GS]

    sealed trait Vertex2Edge[-A, B <: VertexKey[_, _], C <: EdgeKey[_, _, _, _, _, _], GS <: GraphSchema]
        extends Move[A, B, C, GS]
    object Vertex2Edge {

      sealed trait In[-A, B <: VertexKey[OK0, OV0], C <: EdgeKey[_, _, _, _, OK0, OV0], GS <: GraphSchema, OK0, OV0]
          extends Vertex2Edge[A, B, C, GS]

      sealed trait Out[-A, B <: VertexKey[IK0, IV0], C <: EdgeKey[IK0, IV0, _, _, _, _], GS <: GraphSchema, IK0, IV0]
          extends Vertex2Edge[A, B, C, GS]
    }

    trait Vertex2Vertex[-A, B <: VertexKey[_, _], C <: VertexKey[_, _], GS <: GraphSchema] extends Move[A, B, C, GS]
    object Vertex2Vertex {
      sealed trait In[-A, B <: VertexKey[_, _], C <: VertexKey[_, _], GS <: GraphSchema]
          extends Vertex2Vertex[A, B, C, GS]

      sealed trait Out[-A, B <: VertexKey[_, _], C <: VertexKey[_, _], GS <: GraphSchema]
          extends Vertex2Vertex[A, B, C, GS]
    }

    sealed trait Edge2Vertex[-A, B <: EdgeKey[_, _, _, _, _, _], C <: VertexKey[_, _], GS <: GraphSchema]
        extends Move[A, B, C, GS]

    object Edge2Vertex {

      sealed trait In[-A, B <: EdgeKey[IK, IV, _, _, _, _], C <: VertexKey[IK, IV], GS <: GraphSchema, IK, IV]
          extends Edge2Vertex[A, B, C, GS] //edge's in is vertex's out

      sealed trait Out[-A, B <: EdgeKey[_, _, _, _, OK, OV], C <: VertexKey[OK, OV], GS <: GraphSchema, OK, OV]
          extends Edge2Vertex[A, B, C, GS]
    }
  }

  trait Filter[-A, B, GS <: GraphSchema] extends Traversal[A, B, GS]
  object Filter {

    sealed trait Has[-A, B, GS <: GraphSchema] extends Filter[A, B, GS] {
      val p: Traversal[B, Boolean, GS]
      val gs: GS
    }
  }
}

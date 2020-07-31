package com.github.unclebob418.graph.traversal

import com.github.unclebob418.graph.Key.{ EdgeKey, VertexKey }
import com.github.unclebob418.graph.Type.{ EdgeType, VertexType }
import com.github.unclebob418.graph.traversal.Traversal.Step
import com.github.unclebob418.graph.traversal.Traversal.Step.FlatMap
import com.github.unclebob418.graph.traversal.Traversal.Step.FlatMap.Move.{ Edge2Vertex, Vertex2Edge, Vertex2Vertex }
import com.github.unclebob418.graph.traversal.TraversalType._
import com.github.unclebob418.graph.{ Graph, GraphSchema, Key, Schema, Type }
import zio.stream.ZStream

import scala.annotation.implicitNotFound

//the state of a traversal at a particular step
sealed trait Traverser[V] {
  type K
  type Path[_]
  val path: HList.Aux[V] //how we got here, some sort of HList... maybe start with a string?
  val location: Key[K, V]

  def value: V
}

//a traversal A => C
sealed trait Traversal[-A, C, GS <: GraphSchema] extends Schema[GS] { self =>
  type IK; type K; type OK
  type IV; type V; type OV

  val traversalType: TraversalType[IK, IV, K, V, OK, OV]

  def outV[OK0, OV0](oVType: VTs[OK0, OV0])(
    implicit @implicitNotFound("Couldn't find a connection from ${C} to VTs[${IK0},${IV0}], check the schema ${GS}")
    ct0: CTs[K, V, OK0, OV0],
    ev: C =:= VertexKey[K, V]
  ) =
    new Vertex2Vertex.Out[A, VertexKey[K, V], VertexKey[OK0, OV0], GS] {
      type IK = Nothing; type K = OK0; type OK = Nothing
      type IV = Nothing; type V = OV0; type OV = Nothing
      val traversalType: VertexTraversal[IK, IV, K, V, OK, OV] = VertexTraversal(oVType)

      val from: Traversal[A, VertexKey[self.K, self.V], GS] =
        self.asInstanceOf[Traversal[A, VertexKey[self.K, self.V], GS]]
      val gs: GS = self.gs
    }

  def inV[IK0, IV0](iVType: VTs[IK0, IV0])(
    implicit @implicitNotFound("Couldn't find a connection from ${C} to VTs[${IK0},${IV0}], check the schema ${GS}")
    ct0: CTs[IK0, IV0, K, V],
    ev: C <:< VertexKey[K, V]
  ) =
    new Vertex2Vertex.In[A, VertexKey[K, V], VertexKey[IK0, IV0], GS] {
      type IK = Nothing; type K = IK0; type OK = Nothing
      type IV = Nothing; type V = IV0; type OV = Nothing
      val traversalType: VertexTraversal[IK, IV, K, V, OK, OV] = VertexTraversal(iVType)

      val from: Traversal[A, VertexKey[self.K, self.V], GS] =
        self.asInstanceOf[Traversal[A, VertexKey[self.K, self.V], GS]]
      val gs: GS = self.gs
    }

  def outE[K0, V0, OK0, OV0](eType: ETs[K, V, K0, V0, OK0, OV0])(
    implicit ev: C <:< VertexKey[K, V]
  ) =
    new Vertex2Edge.Out[A, VertexKey[K, V], EdgeKey[K, V, K0, V0, OK0, OV0], GS, K, V] {
      type IK = self.K; type K = K0; type OK = OK0
      type IV = self.V; type V = V0; type OV = OV0
      val traversalType: EdgeTraversal[IK, IV, K, V, OK, OV] = EdgeTraversal(eType)

      val from: Traversal[A, VertexKey[self.K, self.V], GS] =
        self.asInstanceOf[Traversal[A, VertexKey[self.K, self.V], GS]]
      val gs: GS = self.gs
    }

  def inE[IK0, IV0, K0, V0](eType: ETs[IK0, IV0, K0, V0, K, V])(
    implicit ev: C <:< VertexKey[K, V]
  ) =
    new Vertex2Edge.In[A, VertexKey[K, V], EdgeKey[IK0, IV0, K0, V0, K, V], GS, K, V] {
      type IK = IK0; type K = K0; type OK = self.K
      type IV = IV0; type V = V0; type OV = self.V
      val traversalType: EdgeTraversal[IK, IV, K, V, OK, OV] = EdgeTraversal(eType)

      val from: Traversal[A, VertexKey[self.K, self.V], GS] =
        self.asInstanceOf[Traversal[A, VertexKey[self.K, self.V], GS]]
      val gs: GS = self.gs
    }

  def out(
    implicit ev: C <:< EdgeKey[IK, IV, K, V, OK, OV],
    oVType: VTs[OK, OV]
  ) =
    new Edge2Vertex.Out[A, EdgeKey[IK, IV, K, V, OK, OV], VertexKey[OK, OV], GS, OK, OV] {
      type IK = Nothing; type K = self.OK; type OK = Nothing
      type IV = Nothing; type V = self.OV; type OV = Nothing
      val traversalType: VertexTraversal[IK, IV, K, V, OK, OV] = VertexTraversal(oVType)

      val from: Traversal[A, EdgeKey[self.IK, self.IV, self.K, self.V, self.OK, self.OV], GS] =
        self.asInstanceOf[Traversal[A, EdgeKey[self.IK, self.IV, self.K, self.V, self.OK, self.OV], GS]]
      val gs: GS = self.gs
    }

  def in(
    implicit ev: C <:< EdgeKey[IK, IV, K, V, OK, OV],
    iVType: VTs[IK, IV]
  ) =
    new Edge2Vertex.In[A, EdgeKey[IK, IV, K, V, OK, OV], VertexKey[IK, IV], GS, IK, IV] {
      type IK = Nothing; type K = self.IK; type OK = Nothing
      type IV = Nothing; type V = self.IV; type OV = Nothing
      val traversalType: VertexTraversal[IK, IV, K, V, OK, OV] = VertexTraversal(iVType)

      val from: Traversal[A, EdgeKey[self.IK, self.IV, self.K, self.V, self.OK, self.OV], GS] =
        self.asInstanceOf[Traversal[A, EdgeKey[self.IK, self.IV, self.K, self.V, self.OK, self.OV], GS]]
      val gs: GS = self.gs
    }

  def value(implicit ev: C <:< Key[K, V]) =
    new Step.Map.ToGraphComponent[A, Key[K, V], V, GS] {
      type IK = Nothing; type K = Nothing; type OK = Nothing
      type IV = Nothing; type V = self.V; type OV  = Nothing
      val traversalType: Value[IK, IV, K, V, OK, OV] = Value()
      val from: Traversal[A, Key[self.K, self.V], GS] =
        self.asInstanceOf[Traversal[A, Key[self.K, self.V], GS]]
      val gs: GS = self.gs
    }

  def id(implicit ev: C <:< Key[K, V]) =
    map(ev(_).key)

  //convert a value to a vertex key
  def key[V0](vType: VTs[C, V0]) = new Step.Map.ToValue[A, C, VertexKey[C, V0], GS] {
    type IK = Nothing; type K = C; type OK  = Nothing
    type IV = Nothing; type V = V0; type OV = Nothing

    val f             = VertexKey(_, vType)
    override val from = self

    val traversalType: VertexTraversal[IK, IV, K, V, OK, OV] = VertexTraversal(vType)
    override val gs: GS                                      = self.gs
  }

  //convert a value to an edge key
  def key[IK0, IV0, V0, OK0, OV0](eType: ETs[IK0, IV0, C, V0, OK0, OV0]) =
    new Step.Map.ToValue[A, C, EdgeKey[IK0, IV0, C, V0, OK0, OV0], GS] {
      type IK = IK0; type K = C; type OK  = OK0
      type IV = IV0; type V = V0; type OV = OV0

      val f             = EdgeKey(_, eType)
      override val from = self

      val traversalType: EdgeTraversal[IK, IV, K, V, OK, OV] = EdgeTraversal(eType)
      override val gs: GS                                    = self.gs
    }

  def map[D](p0: C => D) = new Step.Map.ToValue[A, C, D, GS] {
    type IK = Nothing; type K = Nothing; type OK = Nothing
    type IV = Nothing; type V = self.V; type OV  = Nothing

    override val f: C => D = p0

    override val from: Traversal[A, C, GS]         = self
    val traversalType: Value[IK, IV, K, V, OK, OV] = Value()
    override val gs: GS                            = self.gs
  }

  def filter[D](p0: C => Boolean) =
    new Step.Map.Filter[A, C, GS] {
      type IK = self.IK; type K = self.K; type OK = self.OK
      type IV = self.IV; type V = self.V; type OV = self.OV
      override val from: Traversal[A, C, GS] = self
      val traversalType                      = self.traversalType
      val p: C => Boolean                    = p0
      val gs: GS                             = self.gs
    }

  def flatMap[D](p0: self.type => Traversal[C, D, GS]) =
    new FlatMap.ToValues[A, C, D, GS] {
      type IK = Nothing; type K = Nothing; type OK = Nothing
      type IV = Nothing; type V = D; type OV       = Nothing
      override val from: Traversal[A, C, GS] = self
      val traversalType                      = TraversalType.Value()
      val p: Traversal[C, D, GS]             = p0(self)
      val gs: GS                             = self.gs
    }

  def has(p0: self.type => Traversal[C, Boolean, GS]) =
    new FlatMap.Has[A, C, GS] {
      type IK = self.IK; type K = self.K; type OK = self.OK
      type IV = self.IV; type V = self.V; type OV = self.OV
      override val from: Traversal[A, C, GS] = self
      val traversalType                      = self.traversalType
      val p: Traversal[C, Boolean, GS]       = p0(self)
      val gs: GS                             = self.gs
    }

}
object Traversal {

  def unapply[A, C, GS <: GraphSchema](arg: Traversal[A, C, GS]): Option[TraversalType[Any, Any, Any, Any, Any, Any]] =
    arg match {
      case traversal: Traversal[A, C, GS] => Some(traversal.traversalType)
      case _                              => None
    }

  //a source of traversers
  sealed trait Source[A, GS <: GraphSchema] extends Traversal[Any, A, GS] {
    val gs: GS
  }
  object Source {

    def fromIterable[A, GS <: GraphSchema](iterable: Iterable[A], schema: GS) =
      FromStream(ZStream.fromIterable(iterable), schema)

    def fromStream[A, GS <: GraphSchema](stream: ZStream[Any, Nothing, A], schema: GS) =
      FromStream(stream, schema)

    trait FromStream[A, GS <: GraphSchema] extends Source[A, GS] {
      val stream: ZStream[Any, Nothing, A]
    }
    object FromStream {
      private[graph] def apply[A, GS <: GraphSchema](stream0: ZStream[Any, Nothing, A], schema: GS) =
        new FromStream[A, GS] {
          type IK = Nothing; type K = Nothing; type OK = Nothing
          type IV = Nothing; type V = A; type OV       = Nothing
          val gs: GS                                     = schema
          val traversalType: Value[IK, IV, K, V, OK, OV] = Value()
          val stream: ZStream[Any, Nothing, A]           = stream0
        }

      def unapply(arg: Traversal[_, _, _]): Option[ZStream[Any, Nothing, Any]] = arg match {
        case fromStream: FromStream[_, _] => Some(fromStream.stream)
        case _                            => None
      }
    }

    sealed trait VertexSource[VK0, V0, GS <: GraphSchema] extends Source[VertexKey[VK0, V0], GS] {
      val vt: VertexType[VK0, V0]
      //todo Aux all the things
    }
    object VertexSource {
      def apply[VK0, V0, GS <: GraphSchema](vt0: VertexType[VK0, V0], graph0: Graph[GS]) =
        new VertexSource[VK0, V0, GS] {
          type IK = Nothing; type K = VK0; type OK = Nothing
          type IV = Nothing; type V = V0; type OV  = Nothing

          val vt: VertexType[VK0, V0]                                                     = vt0
          val gs: GS                                                                      = graph0.gs
          val traversalType: VertexTraversal[Nothing, Nothing, VK0, V0, Nothing, Nothing] = VertexTraversal(vt0)
        }

      def unapply(arg: VertexSource[_, _, _]): Option[VertexType[Any, Any]] = arg match {
        case vertexSource: VertexSource[_, _, _] => Some(vertexSource.vt.untyped)
        case _                                   => None
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
      ) =
        new EdgeSource[IK0, IV0, K0, V0, OK0, OV0, GS] {
          type IK = IK0; type K = K0; type OK = OK0
          type IV = IV0; type V = V0; type OV = OV0

        val et: EdgeType[IK0, IV0, K0, V0, OK0, OV0]                 = et0
        val graph: Graph[GS]                                         = graph0
        val gs: GS                                                   = graph0.gs
        val traversalType: EdgeTraversal[IK0, IV0, K0, V0, OK0, OV0] = EdgeTraversal(et0)
      }

      def unapply(arg: Traversal[_, _, _]): Option[EdgeType[Any, Any, Any, Any, Any, Any]] = arg match {
        case edgeSource: EdgeSource[_, _, _, _, _, _, _] => Some(edgeSource.et.untyped)
        case _                                           => None
      }
    }
  }
  sealed trait Step[-A, B, C, GS <: GraphSchema] extends Traversal[A, C, GS] {
    val from: Traversal[A, B, GS]
  }
  object Step {

    //transforms a traverser of B of a traversal A => B to a traverser of C to yield a traversal A => C
    sealed trait Map[-A, B, C, GS <: GraphSchema] extends Step[A, B, C, GS]
    object Map {

      //maps a key to a graph component
      sealed trait ToGraphComponent[-A, B <: Key[_, C], C, GS <: GraphSchema] extends Map[A, B, C, GS] {
        override val traversalType: Value[IK, IV, K, V, OK, OV]

      }

      /**
       * maps a value to another value
       * @tparam GS the graph schema
       */
      sealed trait ToValue[-A, B, C, GS <: GraphSchema] extends Map[A, B, C, GS] {
        val f: B => C
      }
      object ToValue {
        def unapply(arg: Traversal[_, _, _]): Option[Any => Any] = arg match {
          case tv: ToValue[_, _, _, _] => Some(tv.f.asInstanceOf[Any => Any])
          case _                       => None
        }
      }

      /**
       * keeps this object if the supplied predicate evaulates to true
       */
      sealed trait Filter[-A, B, GS <: GraphSchema] extends Map[A, B, B, GS] {
        val p: B => Boolean
      }
      object Filter {
        def unapply(arg: Traversal[_, _, _]): Option[Any => Boolean] = arg match {
          case filter: Filter[_, _, _] => Some(filter.p.asInstanceOf[Any => Boolean])
          case _                       => None
        }
      }
    }

    //transforms a traverser of B of a traversal A => B to 0 or more traversers of C to yield a traversal A => C
    sealed trait FlatMap[-A, B, C, GS <: GraphSchema] extends Step[A, B, C, GS]
    object FlatMap {

      sealed trait Move[-A, B <: Key[_, _], C <: Key[_, _], GS <: GraphSchema] extends FlatMap[A, B, C, GS] {
        override val traversalType: Location[IK, IV, K, V, OK, OV]
      }
      object Move {

        sealed trait Vertex2Edge[-A, B <: VertexKey[_, _], C <: EdgeKey[_, _, _, _, _, _], GS <: GraphSchema]
            extends Move[A, B, C, GS] {
          override val traversalType: EdgeTraversal[IK, IV, K, V, OK, OV]
        }

        object Vertex2Edge {

          sealed trait In[-A, B <: VertexKey[OK0, OV0], C <: EdgeKey[_, _, _, _, OK0, OV0], GS <: GraphSchema, OK0, OV0]
              extends Vertex2Edge[A, B, C, GS] //vertex's out is edge's in
          object In {
            def unapply(arg: Traversal[_, _, _]): Option[EdgeType[Any, Any, Any, Any, Any, Any]] = arg match {
              case in: In[_, _, _, _, _, _] => Some(in.traversalType.tType.untyped)
              case _                        => None
            }
          }

          sealed trait Out[-A, B <: VertexKey[IK0, IV0], C <: EdgeKey[IK0, IV0, _, _, _, _], GS <: GraphSchema, IK0, IV0]
              extends Vertex2Edge[A, B, C, GS] //edge's in is vertex's out
          object Out {
            def unapply(arg: Traversal[_, _, _]): Option[EdgeType[Any, Any, Any, Any, Any, Any]] = arg match {
              case out: In[_, _, _, _, _, _] => Some(out.traversalType.tType.untyped)
              case _                         => None
            }
          }
        }

        sealed trait Vertex2Vertex[-A, B <: VertexKey[_, _], C <: VertexKey[_, _], GS <: GraphSchema]
            extends Move[A, B, C, GS] {
          override val traversalType: VertexTraversal[IK, IV, K, V, OK, OV]
        }
        object Vertex2Vertex {
          sealed trait In[-A, B <: VertexKey[_, _], C <: VertexKey[_, _], GS <: GraphSchema]
              extends Vertex2Vertex[A, B, C, GS]
          object In {
            def unapply(arg: Traversal[_, _, _]): Option[VertexType[Any, Any]] = arg match {
              case in: In[_, _, _, _] => Some(in.traversalType.tType.untyped)
              case _                  => None
            }
          }

          sealed trait Out[-A, B <: VertexKey[_, _], C <: VertexKey[_, _], GS <: GraphSchema]
              extends Vertex2Vertex[A, B, C, GS]
          object Out {
            def unapply(arg: Traversal[_, _, _]): Option[VertexType[Any, Any]] = arg match {
              case out: Out[_, _, _, _] => Some(out.traversalType.tType.untyped)
              case _                    => None
            }
          }
        }

        sealed trait Edge2Vertex[-A, B <: EdgeKey[_, _, _, _, _, _], C <: VertexKey[_, _], GS <: GraphSchema]
            extends Move[A, B, C, GS] {
          override val traversalType: VertexTraversal[IK, IV, K, V, OK, OV]
        }

        object Edge2Vertex {

          sealed trait In[-A, B <: EdgeKey[IK, IV, _, _, _, _], C <: VertexKey[IK, IV], GS <: GraphSchema, IK, IV]
              extends Edge2Vertex[A, B, C, GS]
          object In {
            def unapply(arg: Traversal[_, _, _]): Option[VertexType[Any, Any]] = arg match {
              case in: In[_, _, _, _, _, _] => Some(in.traversalType.tType.untyped)
              case _                        => None
            }
          }
          sealed trait Out[-A, B <: EdgeKey[_, _, _, _, OK, OV], C <: VertexKey[OK, OV], GS <: GraphSchema, OK, OV]
              extends Edge2Vertex[A, B, C, GS]
          object Out {
            def unapply(arg: Traversal[_, _, _]): Option[VertexType[Any, Any]] = arg match {
              case out: Out[_, _, _, _, _, _] => Some(out.traversalType.tType.untyped)
              case _                          => None
            }
          }
        }
      }
      //maps a value to potentially multiple values
      sealed trait ToValues[-A, B, C, GS <: GraphSchema] extends FlatMap[A, B, C, GS]

      //keeps this object if any value in the supplied traversal evaulates to true
      sealed trait Has[-A, B, GS <: GraphSchema] extends FlatMap[A, B, B, GS] {
        val p: Traversal[B, Boolean, GS]
        val gs: GS
      }

    }
  }
}

sealed trait TraversalType[+IK, +IV, +K, +V, +OK, +OV]
object TraversalType {
  sealed trait Value[IK, IV, K, V, OK, OV] extends TraversalType[IK, IV, K, V, OK, OV]
  object Value {
    def apply[IK, IV, K, V, OK, OV](): Value[IK, IV, K, V, OK, OV] = new Value[IK, IV, K, V, OK, OV] {}
  }
  sealed trait Location[IK, IV, K, V, OK, OV] extends TraversalType[IK, IV, K, V, OK, OV] {
    val tType: Type[IK, IV, K, V, OK, OV]
  }
  object Location {
    def unapply(arg: TraversalType[_, _, _, _, _, _]): Option[Type[Any, Any, Any, Any, Any, Any]] = arg match {
      case location: Location[_, _, _, _, _, _] => Some(location.tType.asInstanceOf[Type[Any, Any, Any, Any, Any, Any]])
      case value: Value[_, _, _, _, _, _]       => None
    }
  }
  sealed case class VertexTraversal[IK, IV, K, V, OK, OV](tType: VertexType[K, V])
      extends Location[IK, IV, K, V, OK, OV]
  sealed case class EdgeTraversal[IK, IV, K, V, OK, OV](tType: EdgeType[IK, IV, K, V, OK, OV])
      extends Location[IK, IV, K, V, OK, OV]
}

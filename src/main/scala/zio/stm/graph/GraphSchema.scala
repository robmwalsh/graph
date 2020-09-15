package zio.stm.graph

import zio.stm.graph.Key.{EdgeKey, VertexKey}
import zio.stm.graph.Type.{EdgeType, VertexType}

import scala.annotation.implicitNotFound

//todo standardise naming and order of types here
trait Schema[GS <: GraphSchema] extends GraphSchema {
  val gs: GS
  override type VTs[VK, V]                 = gs.VTs[VK, V]
  override type CTs[IK, IV, OK, OV]        = gs.CTs[IK, IV, OK, OV]
  override type ETs[IK, IV, EK, E, OK, OV] = gs.ETs[IK, IV, EK, E, OK, OV]
}

trait GraphSchema { self =>

  @implicitNotFound("VertexType[${VK}, ${V}] is not allowed in the graph; check your schema.")
  type VTs[VK, V] <: VertexType[VK, V] //vertex types

  //todo should be able to derive inverse for connection type if needed
  //@implicitNotFound("VertexType[${IK}, ${IV}] can't be connected to VertexType[${OK}, ${OV}]; check your schema.")
  type CTs[IK, IV, OK, OV] <: ConnectionType[IK, IV, OK, OV] //allowed connections

  @implicitNotFound(
    "EdgeType[${EK}, ${E}] not allowed to connect VertexType[${IK}, ${IV}] to VertexType[${OK}, ${OV}]; check your schema."
  )
  type ETs[IK, IV, EK, E, OK, OV] <: EdgeType[IK, IV, EK, E, OK, OV] //allowed edges

  //todo allow cycles if desired/block cycles if desired
  //todo allow certain types of cycles only?
  //todo composition of GraphSchemas (much easier now!)
}

//todo add type to key? sealed case class VertexKey[K, V, VT <: VertexType[K, V]](key: K)(implicit vType: VertexType[K, V])
//a Key contains both the type and ID of a component - equivalent to a location on the graph
sealed trait Key[K, V] {
  val key: K
  val cType: Type[_, _, K, V, _, _]
}
object Key {
  //todo need a better name for c(omponent)Type, confused with c(onnection)Type
  sealed case class VertexKey[VK, V](key: VK, cType: VertexType[VK, V])                             extends Key[VK, V]
  sealed case class EdgeKey[IK, IV, EK, E, OK, OV](key: EK, cType: EdgeType[IK, IV, EK, E, OK, OV]) extends Key[EK, E]
}

sealed trait Type[+IK, +IV, +K, +V, +OK, +OV] { self =>
  val untyped: Type[Any, Any, Any, Any, Any, Any] = self.asInstanceOf[Type[Any, Any, Any, Any, Any, Any]]
}
object Type {

  trait VertexType[K, V] extends Type[Nothing, Nothing, K, V, Nothing, Nothing] { self =>

    def apply(k: K): VertexKey[K, V] = VertexKey(k, self)

    val key: V => K

    override val untyped: VertexType[Any, Any] = self.asInstanceOf[VertexType[Any, Any]]
  }
  object VertexType {
    def unapply[IK, IV, K, V, OK, OV](arg: Type[IK, IV, K, V, OK, OV]): Option[VertexType[K, V]] = arg match {
      case vertexType: VertexType[K, V] => Some(vertexType)
      case _                            => None
    }
  }

  trait EdgeType[IK, IV, EK, E, OK, OV] extends Type[IK, IV, EK, E, OK, OV] { self =>
    def key(e: E): EdgeKey[IK, IV, EK, E, OK, OV]
    //make sure an edge can't exist without a valid connection
    val ct: ConnectionType[IK, IV, OK, OV]
    override val untyped: EdgeType[Any, Any, Any, Any, Any, Any] =
      self.asInstanceOf[EdgeType[Any, Any, Any, Any, Any, Any]]
  }
  object EdgeType {
    def unapply[IK, IV, K, V, OK, OV](arg: Type[IK, IV, K, V, OK, OV]): Option[EdgeType[IK, IV, K, V, OK, OV]] =
      arg match {
        case edgeType: EdgeType[IK, IV, K, V, OK, OV] => Some(edgeType)
        case _                                        => None
      }
  }
}

trait ConnectionType[IK, IV, OK, OV] {
  self =>
  //make sure connection can't exist without valid vertex types
  val from: VertexType[IK, IV]
  val to: VertexType[OK, OV]
  def reverse: ConnectionType[OK, OV, IK, IV] = new ConnectionType[OK, OV, IK, IV] {
    override val from: VertexType[OK, OV] = self.to
    override val to: VertexType[IK, IV]   = self.from
  }
}

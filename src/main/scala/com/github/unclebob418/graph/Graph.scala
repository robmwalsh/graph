package com.github.unclebob418.graph

import java.util.UUID

final case class Graph[VS[_, _] <: VertexSchema[_, _], ES <: EdgeSchema[VS]](
  vs: VertexMap[VS],
  es: EdgeMap[VS, ES]
) extends Product
    with Serializable {

  def addV[K, V](key: VertexKey[K, V], value: V)(implicit vType: VS[K, V]): Option[Graph[VS, ES]] =
    Some(copy(vs.addV(key, value))) //todo validate Some(A)

  def addE[K, E0, IK, IV, OK, OV](inVK: VertexKey[IK, IV], edgeKey: EdgeKey[K, E0], e: E0, outVK: VertexKey[OK, OV])(
    implicit eType: ES {
      type In  = VS[IK, IV]
      type Out = VS[OK, OV]
      type E   = E0
    }
  ): Option[Graph[VS, ES]] = Some(copy(vs, es.addE(inVK, edgeKey, e, outVK)))

  def containsV[K, V](vk: VertexKey[K, V])(implicit vType: VS[K, V]): Boolean = vs.containsV(vk)

  def getV[K, V](vk: VertexKey[K, V])(implicit vType: VS[K, V]): Option[V] =
    vs.getV(vk)

  def getVs[K, V](implicit vType: VS[K, V]): Option[Map[VertexKey[K, V], V]] = vs.getAll[K, V]
}
object Graph {
  def empty[VS[_, _] <: VertexSchema[_, _], ES <: EdgeSchema[VS]]: Graph[VS, ES] =
    Graph[VS, ES](VertexMap.empty, EdgeMap.empty)
}

sealed abstract class Vertex {
  type V
  val id: UUID
  val value: V
}
object Vertex {
  def apply[I0, V0](id0: UUID, value0: V0): Vertex = new Vertex {
    type I = I0
    type V = V0
    val id    = id0
    val value = value0
  }
}

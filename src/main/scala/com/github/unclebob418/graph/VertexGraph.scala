package com.github.unclebob418.graph

import java.util.UUID

trait VertexKey[+V] extends Key[V] {
  val uuid:UUID
}

trait VertexSchema[V] {
  def key(uuid0: UUID = UUID.randomUUID()): VertexKey[V]
}

final case class VertexMap[VS[_] <: VertexSchema[_]] private(private val map: Map[Any, Map[Any, Any]]) {
  def put[K, V](key: VertexKey[V], value: V)(implicit vType: VS[V]): VertexMap[VS] =
    VertexMap[VS](map.get(vType) match {
      case Some(subMap) =>
        map + (vType -> (subMap + (key -> value)))
      case None =>
        map + (vType -> Map(key -> value))
    })

  def get[K, V](key: VertexKey[V])(implicit vType: VS[V]): Option[V] =
    map.get(vType).flatMap(_.get(key).asInstanceOf[Option[V]])

  def getAll[V](implicit vType: VS[V]): Option[Map[VertexKey[V], V]] =
    map.get(vType).asInstanceOf[Option[Map[VertexKey[V], V]]]
}

object VertexMap {
  def empty[VS[_] <: VertexSchema[_]]: VertexMap[VS] = VertexMap[VS](Map.empty[Any, Map[Any, Any]])
}
package com.github.unclebob418.graph

import java.util.UUID

trait Key[+Value]

final class PolyMap private (private val map: Map[Any, Any]) {
  def put[K, V](key: Key[V], value: V): PolyMap =
    new PolyMap(map + (key -> value))
  def get[K, V](key: Key[V]): Option[V] =
    map.get(key).asInstanceOf[Option[V]]
}

object PolyMap {
  def empty: PolyMap = new PolyMap(Map.empty)
}

trait ESchema[F, E, T]

trait VertexKey[+V] extends Key[V]

trait VSchema[V]

final case class VertexMap[VS[_] <: VSchema[_]] private (private val map: Map[Any, Map[Any, Any]]) {
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
  def empty[VS[_] <: VSchema[_]]: VertexMap[VS] = VertexMap[VS](Map.empty[Any, Map[Any, Any]])
}

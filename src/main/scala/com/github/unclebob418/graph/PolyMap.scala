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
final case class IntKey(uuid: UUID) extends VertexKey[Int]
final case class StringKey(uuid: UUID) extends VertexKey[String]

trait VSchema[V]

final class VertexMap[VS[_] <: VSchema[_]] private (private val map: Map[Any, Any]) {
  def put[K, V](key: VertexKey[V], value: V)(implicit ev: VS[V]): VertexMap[Nothing] =
    new VertexMap(map + (key -> value))
  def get[K, V](key: VertexKey[V])(implicit ev: VS[V]): Option[V] =
    map.get(key).asInstanceOf[Option[V]]
}
object VertexMap {

}
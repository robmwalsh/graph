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





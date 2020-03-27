package com.github.unclebob418.graph

trait VertexKey[+K, +V] {
  val key: K
}

trait VertexSchema[K, V] {}

final case class VertexMap[VS[_, _] <: VertexSchema[_, _]] private (
  private val vMap: Map[Any, Any],
  tMap: Map[Any, Map[Any, Any]]
) {

  def addV[K, V](key: VertexKey[K, V], value: V)(implicit vType: VS[K, V]): VertexMap[VS] =
    VertexMap[VS](
      vMap + (key -> value),
      tMap.get(vType) match {
        case Some(subMap) =>
          tMap + (vType -> (subMap + (key -> value)))
        case None =>
          tMap + (vType -> Map(key -> value))
      }
    )

  def containsV[K, V](vk: VertexKey[K, V])(implicit vType: VS[K, V]): Boolean =
    vMap.contains(vk)

  def getV[K, V](key: VertexKey[K, V])(implicit vType: VS[K, V]): Option[V] =
    vMap.get(vType).asInstanceOf[Option[V]]

  def getAll[K, V](implicit vType: VS[K, V]): Option[Map[VertexKey[K, V], V]] =
    tMap.get(vType).asInstanceOf[Option[Map[VertexKey[K, V], V]]]
}

object VertexMap {
  def empty[VS[_, _] <: VertexSchema[_, _]]: VertexMap[VS] =
    VertexMap[VS](Map.empty[Any, Any], Map.empty[Any, Map[Any, Any]])
}

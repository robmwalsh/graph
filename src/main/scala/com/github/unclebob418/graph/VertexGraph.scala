package com.github.unclebob418.graph

trait VertexKey[K, +V] {
  val key: K
}

trait VertexSchema[K, V] {


}

final case class VertexMap[VS[_, _] <: VertexSchema[_, _]] private (private val map: Map[Any, Map[Any, Any]]) {
  def put[K, V](key: VertexKey[K, V], value: V)(implicit vType: VS[K, V]): VertexMap[VS] =
    VertexMap[VS](map.get(vType) match {
      case Some(subMap) =>
        map + (vType -> (subMap + (key -> value)))
      case None =>
        map + (vType -> Map(key -> value))
    })

  def get[K, V](key: VertexKey[K, V])(implicit vType: VS[K, V]): Option[V] =
    map.get(vType).flatMap(_.get(key).asInstanceOf[Option[V]])

  def getAll[K, V](implicit vType: VS[K, V]): Option[Map[VertexKey[K, V], V]] =
    map.get(vType).asInstanceOf[Option[Map[VertexKey[K, V], V]]]
}

object VertexMap {
  def empty[VS[_, _] <: VertexSchema[_, _]]: VertexMap[VS] = VertexMap[VS](Map.empty[Any, Map[Any, Any]])
}

package com.github.unclebob418.graph

object VertexMap {
  def empty[VT[_, _] <: VertexType[_, _]]: VertexMap[VT] =
    VertexMap[VT](Map.empty[Any, Any], Map.empty[Any, Map[Any, Any]])
}

final case class VertexMap[VT[_, _] <: VertexType[_, _]] private(
  private val vMap: Map[Any, Any],
  tMap: Map[Any, Map[Any, Any]]
) {

  def addV[K, V](key: VertexKey[K, V], value: V)(implicit vType: VT[K, V]): VertexMap[VT] =
    VertexMap[VT](
      vMap + (key -> value),
      tMap.get(vType) match {
        case Some(subMap) =>
          tMap + (vType -> (subMap + (key -> value)))
        case None =>
          tMap + (vType -> Map(key -> value))
      }
    )

  def containsV[K, V](vk: VertexKey[K, V])(implicit vType: VT[K, V]): Boolean =
    vMap.contains(vk)

  def getV[K, V](key: VertexKey[K, V])(implicit vType: VT[K, V]): Option[V] =
    vMap.get(vType).asInstanceOf[Option[V]]

  def getAll[K, V](implicit vType: VT[K, V]): Option[Map[VertexKey[K, V], V]] =
    tMap.get(vType).asInstanceOf[Option[Map[VertexKey[K, V], V]]]
}



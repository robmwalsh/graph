package com.github.unclebob418.graph

object EdgeMap {
  def empty[VT[_, _] <: VertexType[_, _]]: EdgeMap[VT] =
    new EdgeMap[VT] {
      val es    = Map.empty[Any, Any]
      val inVs  = Map.empty[Any, Set[Any]]
      val outVs = Map.empty[Any, Set[Any]]
    }
}

sealed trait EdgeMap[VT[_, _] <: VertexType[_, _]] { self =>

  val es: Map[Any, Any]
  val inVs: Map[Any, Set[Any]]
  val outVs: Map[Any, Set[Any]]

  def addE[K, E0, IK, IV, OK, OV, GS <: GraphSchema](
    inVK: VertexKey[IK, IV],
    edgeKey: EdgeKey[K, E0],
    e: E0,
    outVK: VertexKey[OK, OV]
  )
  : EdgeMap[VT] = {
    val edge = Edge(inVK, edgeKey, e, outVK)
    copy(
      es + (edgeKey -> edge),
      inVs.get(inVK) match {
        case Some(set: Set[Any]) =>
          inVs + (inVK -> (set + edgeKey))
        case None =>
          inVs + (inVK -> Set(edgeKey))
      },
      outVs.get(outVK) match {
        case Some(set: Set[Any]) =>
          inVs + (outVK -> (set + edgeKey))
        case None =>
          inVs + (outVK -> Set(edgeKey))
      }
    )
  }
  def copy(
    es0: Map[Any, Any] = es,
    inVs0: Map[Any, Set[Any]] = inVs,
    outVs0: Map[Any, Set[Any]] = outVs
  ): EdgeMap[VT] =
    new EdgeMap[VT] {
      override val es: Map[Any, Any]         = Map.empty
      override val inVs: Map[Any, Set[Any]]  = Map.empty
      override val outVs: Map[Any, Set[Any]] = Map.empty
    }

  def getE[K, E](edgeKey: EdgeKey[K, E]): Option[E] =
    es.get(edgeKey).asInstanceOf[Option[E]]

  def inVs[K, V](vk: VertexKey[K, V])  = ???

  def outVs[K, V](vk: VertexKey[K, V]) = ???

  def removeEs[K, V](vk: VertexKey[K, V]) = ???
}

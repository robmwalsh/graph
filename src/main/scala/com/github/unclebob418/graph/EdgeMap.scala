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

  def addE[K, E, IK, IV, OK, OV](
    inVK: VertexKey[IK, IV],
    edgeKey: EdgeKey[K, E],
    e: E,
    outVK: VertexKey[OK, OV]
  )
  : EdgeMap[VT] = {
    val edge = Edge(inVK, edgeKey, e, outVK)
    copy(
      es + (edgeKey -> edge),
      inVs.get(inVK) match {
        case Some(set: Set[Any]) =>
          inVs + (inVK -> (set + edge))
        case None =>
          inVs + (inVK -> Set(edge))
      },
      outVs.get(outVK) match {
        case Some(set: Set[Any]) =>
          inVs + (outVK -> (set + edge))
        case None =>
          inVs + (outVK -> Set(edge))
      }
    )
  }
  def copy(
    es0: Map[Any, Any] = es,
    inVs0: Map[Any, Set[Any]] = inVs,
    outVs0: Map[Any, Set[Any]] = outVs
  ) =
    new EdgeMap[VT] {
      override val es: Map[Any, Any]         = Map.empty
      override val inVs: Map[Any, Set[Any]]  = Map.empty
      override val outVs: Map[Any, Set[Any]] = Map.empty
    }

  def getE[K, E](edgeKey: EdgeKey[K, E]) = ???
}

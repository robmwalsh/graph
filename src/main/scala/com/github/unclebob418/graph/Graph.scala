package com.github.unclebob418.graph

import com.github.unclebob418.graph.Key.VertexKey
import com.github.unclebob418.graph.Type.{EdgeType, VertexType}
import com.github.unclebob418.graph.traversal.Traversal
import com.github.unclebob418.graph.traversal.Traversal.Source

object Graph {
  def empty[GS <: GraphSchema](implicit gs0: GS): Option[Graph[GS]] =
    Some(new Graph[GS] {
      val cs     = Map.empty[Any, Any]
      val ts     = Map.empty[Any, Set[Any]]
      val in     = Map.empty[Any, Set[(Any, Any)]]
      val out    = Map.empty[Any, Set[(Any, Any)]]
      val gs: GS = gs0
    })
}

sealed trait Graph[GS <: GraphSchema] extends Schema[GS] { self =>
  //todo merge es & vs? means we can grab anything out of the graph
  // and would support unsafe traversals (which could be made safe by specifying a type)
  val cs: Map[Any, Any]      //           [VK, V]         - component map, stores all edges & vertices
  val ts: Map[Any, Set[Any]] //           [Type, Set[K]]  - type map, stores all vertex and edge keys indexed by type
  //                                      K = EK where K <: EdgeKey
  val in: Map[Any, Set[(Any, Any)]]  //   [K, Set[(EK, VK)]]  - stores incoming edge & vertex key for each key
  val out: Map[Any, Set[(Any, Any)]] //   [K, Set[(EK, VK)]]  - stores outgoing edge & vertex key for each key

  def V[K, V](vType: VTs[K, V]) =
    Traversal.Source.VertexSource(vType, self)

  def E[IK, IV, K, V, OK, OV](eType: ETs[IK, IV, K, V, OK, OV]) =
    Source.EdgeSource(self, eType)

  def addV[K, V](v: V)(implicit vType: VTs[K, V]): Some[Graph[GS]] = {
    //todo validate
    val vertexKey = vType.key(v)
    Some(
      copy(
        cs0 = cs + (vertexKey -> v),
        ts0 = ts.get(vType) match {
          case Some(set) =>
            ts + (vType -> (set + vertexKey))
          case None =>
            ts + (vType -> Set(vertexKey))
        }
      )
    )
  }

  def addE[K0, E0, IK, IV, OK, OV](inV: IV, e: E0, outV: OV)(
    implicit eType: ETs[IK, IV, K0, E0, OK, OV],
    iVType: VTs[IK, IV],
    oVType: VTs[OK, OV]
  ): Option[Graph[GS]] = addE(iVType.key(inV), e, oVType.key(outV))

  def addE[K0, E0, IK, IV, OK, OV](inVK: VertexKey[IK, IV], edge: E0, outVK: VertexKey[OK, OV])(
    implicit eType: ETs[IK, IV, K0, E0, OK, OV],
    iVType: VTs[IK, IV],
    oVType: VTs[OK, OV]
  ): Option[Graph[GS]] =
    if (cs.contains(inVK) && cs.contains(outVK)) {
      val edgeKey = eType.key(edge)
      Some(
        copy(
          cs0 = cs + (edgeKey -> (inVK, edge, outVK)),
          ts0 = ts.get(eType) match {
            case Some(set) => ts + (eType -> (set + edgeKey))
            case None      => ts + (eType -> Set(edgeKey))
          },
          in0 = (in.get(outVK) match {
            case Some(set) => in + (outVK -> set.incl((edgeKey, inVK)))
            case None      => in + (outVK -> Set((edgeKey, inVK)))
          }).asInstanceOf[Map[Any, Set[(Any, Any)]]]
            + (edgeKey -> Set((edgeKey, inVK))),
          out0 = (out.get(inVK) match {
            case Some(set) => out + (inVK -> set.incl((edgeKey, outVK)))
            case None      => out + (inVK -> Set((edgeKey, outVK)))
          }).asInstanceOf[Map[Any, Set[(Any, Any)]]]
            + (edgeKey -> Set((edgeKey, outVK)))
        )
      )
    } else None

  def containsV[K, V](vk: VertexKey[K, V])(implicit vType: VTs[K, V]): Boolean = cs.contains(vk)

  def copy(
    cs0: Map[Any, Any] = cs,
    ts0: Map[Any, Set[Any]] = ts,
    in0: Map[Any, Set[(Any, Any)]] = in,
    out0: Map[Any, Set[(Any, Any)]] = out
  ): Graph[GS] =
    new Graph[GS] {
      val cs: Map[Any, Any]              = cs0
      val ts: Map[Any, Set[Any]]         = ts0
      val in: Map[Any, Set[(Any, Any)]]  = in0
      val out: Map[Any, Set[(Any, Any)]] = out0
      val gs: GS                         = self.gs
    }

  //get a graph component
  private[graph] def getC[VK, V](ck: Key[VK, V]): Option[V] =
    cs.get(ck).asInstanceOf[Option[V]]

  private[graph] def getVs[VK, V](vType: VertexType[VK, V]): List[VK] =
    ts.get(vType)
      .asInstanceOf[Option[Set[VK]]]
      .getOrElse(Set.empty[VK]).toList

  private[graph] def getEs[IK, IV, EK, E, OK, OV](
    eType: EdgeType[IK, IV, EK, E, OK, OV]
  ): List[EK] =
    ts.get(eType)
      .asInstanceOf[Option[Set[EK]]]
      .getOrElse(Set.empty[EK]).toList
}

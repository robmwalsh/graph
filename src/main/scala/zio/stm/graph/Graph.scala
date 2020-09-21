package zio.stm.graph

import zio.stm.ZSTM.internal.TExit
import zio.stm.graph.Graph.GraphState
import zio.stm.graph.GraphError.{ VertexExists, VertexMissing }
import zio.stm.graph.Key.{ EdgeKey, VertexKey }
import zio.stm.graph.Type.{ EdgeType, VertexType }
import zio.stm.graph.traversal.Traversal.Source
import zio.stm.{ STM, TRef, USTM, ZSTM }

import scala.annotation.implicitNotFound

sealed trait Graph[GS <: GraphSchema] extends Schema[GS] {
  self =>
  //todo merge es & vs? means we can grab anything out of the graph
  // and would support unsafe traversals (which could be made safe by specifying a type)
  private [graph] val state: TRef[GraphState[GS]]

  def V[K, V](vType: VTs[K, V]) =
    Source.VertexSource(vType, self)

  def E[IK, IV, K, V, OK, OV](eType: ETs[IK, IV, K, V, OK, OV]) =
    Source.EdgeSource(self, eType)

  def addV[K, V](v: V)(implicit vType: VTs[K, V]): STM[GraphError, Unit] =
    new ZSTM((journal, _, _, _) => {
      val vertexKey = vType(vType.key(v))
      val oldstate  = state.unsafeGet(journal)
      if (oldstate.cs.contains(vertexKey)) {
        TExit.Fail(VertexExists(s"Vertex ${vertexKey} is already present in the graph"))
      } else {
        state.unsafeSet(
          journal,
          oldstate.copy(
            cs = oldstate.cs + (vertexKey -> v),
            ts = oldstate.ts.get(vType) match {
              case Some(set) =>
                oldstate.ts + (vType -> (set + vertexKey))
              case None =>
                oldstate.ts + (vType -> Set(vertexKey))
            }
          )
        )
        TExit.unit
      }
    })

  def addE[K0, E0, IK, IV, OK, OV](inV: IV, e: E0, outV: OV)(
    implicit @implicitNotFound(
      "EdgeType[${K0}, ${E0}] is not allowed to connect VertexType[${IK}, ${IV}] to VertexType[${OK}, ${OV}]; check your schema."
    ) eType: ETs[IK, IV, K0, E0, OK, OV],
    iVType: VTs[IK, IV],
    oVType: VTs[OK, OV]
  ): STM[GraphError, Unit] = addE(iVType(iVType.key(inV)), e, oVType(oVType.key(outV)))

  def addE[K0, E0, IK, IV, OK, OV](inVK: VertexKey[IK, IV], edge: E0, outVK: VertexKey[OK, OV])(
    implicit @implicitNotFound(
      "EdgeType[${K0}, ${E0}] is not allowed to connect VertexType[${IK}, ${IV}] to VertexType[${OK}, ${OV}]; check your schema."
    ) eType: ETs[IK, IV, K0, E0, OK, OV],
    iVType: VTs[IK, IV],
    oVType: VTs[OK, OV]
  ): STM[GraphError, Unit] =
    new ZSTM((journal, _, _, _) => {
      val oldstate = state.unsafeGet(journal)

      (oldstate.cs.contains(inVK), oldstate.cs.contains(outVK)) match {
        case (true, true) =>
          val edgeKey = eType(eType.key(edge))
          state.unsafeSet(
            journal,
            oldstate.copy(
              cs = oldstate.cs + (edgeKey -> (inVK, edge, outVK)),
              ts = oldstate.ts.get(eType) match {
                case Some(set) => oldstate.ts + (eType -> (set + edgeKey))
                case None      => oldstate.ts + (eType -> Set(edgeKey))
              },
              in = (oldstate.in.get(outVK) match {
                case Some(set) => oldstate.in + (outVK -> set.incl((edgeKey, inVK)))
                case None      => oldstate.in + (outVK -> Set((edgeKey, inVK)))
              }).asInstanceOf[Map[Any, Set[(Any, Any)]]],
              out = (oldstate.out.get(inVK) match {
                case Some(set) => oldstate.out + (inVK -> set.incl((edgeKey, outVK)))
                case None      => oldstate.out + (inVK -> Set((edgeKey, outVK)))
              }).asInstanceOf[Map[Any, Set[(Any, Any)]]]
            )
          )
          TExit.unit
        case (false, false) =>
          TExit.Fail(VertexMissing("both vertices are missing"))
        case (false, true) =>
          TExit.Fail(VertexMissing("in vertex is missing"))
        case (true, false) =>
          TExit.Fail(VertexMissing("out vertex is missing"))
      }
    })

  def containsE[IK, IV, K, V, OK, OV](
    ek: EdgeKey[IK, IV, K, V, OK, OV]
  )(implicit vType: ETs[IK, IV, K, V, OK, OV]): USTM[Boolean] =
    new ZSTM((journal, _, _, _) => TExit.Succeed(state.unsafeGet(journal).cs.contains(ek)))

  def containsV[K, V](vk: VertexKey[K, V])(implicit vType: VTs[K, V]): USTM[Boolean] =
    new ZSTM((journal, _, _, _) => TExit.Succeed(state.unsafeGet(journal).cs.contains(vk)))

  private[graph] def getC[VK, V](ck: Key[VK, V]): STM[GraphError, V] =
    new ZSTM(
      (journal, _, _, _) =>
        state
          .unsafeGet(journal)
          .cs
          .get(ck)
          .fold[TExit[GraphError, V]](
            TExit.Fail(GraphError.Generic(s"${ck} not found"))
          )(
            v => TExit.Succeed(v.asInstanceOf[V])
          )
    )

  private[graph] def getE[IK, IV, K, V, OK, OV](ek: EdgeKey[IK, IV, K, V, OK, OV]): STM[GraphError, V] =
    getC(ek)

  private[graph] def getV[VK, V](vk: VertexKey[VK, V]): STM[GraphError, V] =
    getC(vk)

  private[graph] def getVs[VK, V](vType: gs.VTs[VK, V]): STM[GraphError, Set[VK]] =
    new ZSTM(
      (journal, _, _, _) =>
        state
          .unsafeGet(journal)
          .ts
          .get(vType)
          .fold(
            TExit.Succeed(Set.empty[VK])
          )(
            vs => TExit.Succeed(vs.asInstanceOf[Set[VK]])
          )
    )

  private[graph] def getEs[IK, IV, EK, E, OK, OV](
    eType: EdgeType[IK, IV, EK, E, OK, OV]
  ): USTM[Set[EK]] =
    new ZSTM(
      (journal, _, _, _) =>
        state
          .unsafeGet(journal)
          .ts
          .get(eType)
          .fold(TExit.Succeed(Set.empty[EK]))(vs => TExit.Succeed(vs.asInstanceOf[Set[EK]]))
    )

  private[graph] def getState: USTM[GraphState[GS]] =
    new ZSTM((journal, _, _, _) => {
      val s = state.unsafeGet(journal)
      TExit.Succeed(s)
    })

}

object Graph {
  //todo use `TMap`s? better for contention, higher cost of extracting the state for longer computations
  sealed case class GraphState[GS <: GraphSchema](
    cs: Map[Any, Any],      //           [VK, V]         - component map, stores all edges & vertices
    ts: Map[Any, Set[Any]], //           [Type, Set[K]]  - type map, stores all vertex and edge keys indexed by type
    //                                   K = EK where K <: EdgeKey
    in: Map[Any, Set[(Any, Any)]],  //   [K, Set[(EK, VK)]]  - stores incoming edge & vertex key for each key
    out: Map[Any, Set[(Any, Any)]], //   [K, Set[(EK, VK)]]  - stores outgoing edge & vertex key for each key)
    gs: GS
  ) extends Schema[GS]

  def make[R, E, GS <: GraphSchema](schema: GS): ZSTM[Any, Nothing, Graph[GS]] =
    TRef
      .make(
        GraphState(
          Map.empty[Any, Any],
          Map.empty[Any, Set[Any]],
          Map.empty[Any, Set[(Any, Any)]],
          Map.empty[Any, Set[(Any, Any)]],
          schema
        )
      )
      .map(
        ref =>
          new Graph[GS] {
            override val state: TRef[GraphState[GS]] = ref
            override val gs: GS                      = schema
          }
      )
}

package com.github.unclebob418.graph.traversal.interpreter

import com.github.unclebob418.graph.{ Edge, GraphSchema, Vertex }
import com.github.unclebob418.graph.traversal.{ Traversal, TraversalResult }
import com.github.unclebob418.graph.traversal.Traversal.Step.{ EdgeTraversal, VertexTraversal }

object SimpleInterpreter {

  def interpret[A, K, V, GS <: GraphSchema](
                                                                     traversal: Traversal.Step[K,V, GS]
                                                                   ): List[String] = ???
   /* traversal.foldRight(List.empty[TraversalResult])(
      (z, t) =>
        t match {
          case t: VertexTraversal.VSource[VK, V, GS]                  =>
            val x: List[Vertex[Any, Any]] = t.tail.graph.getVs(t.vType).values.toList
            TraversalResult.VertexList(x) :: z
          case t: VertexTraversal.VTraversal[VK, V, GS]               => s"Vertex Traversal (${t.vType})" :: z
          case t: VertexTraversal.Has[VK, V, GS]                      => s"Vertex Has (${t.vType})" :: z
          case t: EdgeTraversal.ESource[IK, IV, EK, E, OK, OV, GS]    => TraversalResult.EdgeList(t.tail.graph.getEs(t.eType).values.toList) :: z
          case t: EdgeTraversal.ETraversal[IK, IV, EK, E, OK, OV, GS] => s"Edge Traversal (${t.eType})" :: z
          case t: EdgeTraversal.Has[IK, IV, EK, E, OK, OV, GS]        => s"Edge Has (${t.eType})" :: z
        }
    )*/

  //todo make tail-recursive
  /*def go[VK, V, IK, IV, EK, E, OK, OV, GS <: GraphSchema](
    traversal: Traversal[VK, V, IK, IV, EK, E, OK, OV, GS]
  )(partialResult: TraversalResult): TraversalResult =
    traversal match {
      case source: Traversal.GraphTraversalSource[GS] => TraversalResult.Empty
      case step: Traversal.Step[VK, V, IK, IV, EK, E, OK, OV, GS] =>
        step match {
          case t: VertexTraversal.VSource[VK, V, GS] =>
            val res =
              t.tail.graph
                .getVs[VK, V](t.vType)
                .values
                .toList
            TraversalResult.VertexList(res)
          case t: VertexTraversal.VTraversal[VK, V, GS] => ??? //go(t.tail).flatMap()
          case t: VertexTraversal.VHas[VK, V, GS] =>
            Right(go(t.tail).getOrElse(List.empty[Vertex[VK, V]]).filter(v => t.p(v.value)))
          case t: EdgeTraversal.ESource[IK, IV, EK, E, OK, OV, GS] =>
            val res = t.tail.graph
              .getEs[IK, IV, EK, E, OK, OV](t.eType)
              .values
              .toList
            Left(res)
          case t: EdgeTraversal.ETraversal[IK, IV, EK, E, OK, OV, GS] => ???
          case t: EdgeTraversal.EHas[IK, IV, EK, E, OK, OV, GS]        => ???
        }
    }*/
}

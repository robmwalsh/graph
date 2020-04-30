package com.github.unclebob418.graph.traversal.interpreter

import com.github.unclebob418.graph.{ Edge, Graph, GraphSchema, Vertex }
import com.github.unclebob418.graph.traversal.{ Traversal, TraversalResult }
import com.github.unclebob418.graph.traversal.Traversal.Step.{ EdgeTraversal, VertexTraversal }
import com.github.unclebob418.graph.traversal.TraversalResult.{ EdgeList, VertexList }

object SimpleInterpreter {

  def interpret[A, K, V, GS <: GraphSchema](traversal: Traversal.Step[K, V, GS]): List[String] = ???

  //todo make tail-recursive somehow
  /*def go[K, V, TK, TV, FK, FV, GS <: GraphSchema](
    traversal: Traversal[K, V, GS]
  )(graph: Graph[GS]): TraversalResult[K, V] => TraversalResult[FK, FV] = traversal match {
    case gts: Traversal.GraphTraversalSource[_]     => ??? //do we even need this?
    case source: VertexTraversal.VSource[K, V, GS] => VertexList(graph.getVs(source.vType).values.toList)

    case traversal: VertexTraversal.VTraversal[K, V, GS] => ???
    case has: VertexTraversal.VHas[K, V, GS] =>
      go(has.tail).compose {
        case TraversalResult.VertexList(vs) => VertexList(vs.filter(has.p.compose(_.value)))
        case _                              => VertexList(List.empty[Vertex[K, V]])
      }
    case source: EdgeTraversal.ESource[ik, iv, K, V, ok, ov, GS]       => ???
    case traversal: EdgeTraversal.ETraversal[ik, iv, K, V, ok, ov, GS] => ???
    case has: EdgeTraversal.EHas[ik, iv, K, V, ok, ov, GS] =>
      go(has.tail).compose {
        case TraversalResult.EdgeList(es) => EdgeList(es.filter(has.p.compose(_.edge)))
        case _                            => EdgeList(List.empty[Edge[Any, Any, K, V, Any, Any]])
      }
  }*/

}

package com.github.unclebob418.graph.interpreter

import com.github.unclebob418.graph.GraphSchema
import com.github.unclebob418.graph.Type.VertexType
import com.github.unclebob418.graph.traversal.TraversalType.{EdgeTraversal, VertexTraversal}
import com.github.unclebob418.graph.traversal.{Traversal, TraversalType}

object Interpreter {
  def interpret[A, GS <: GraphSchema](traversal: Traversal[Any, A, GS]): Iterable[A] = {
   /* traversal.traversalType match {
      case TraversalType.Value => ???
      case VertexTraversal(vType) => ???
      case EdgeTraversal(edgeType) => ???
    }
    */
   /* traversal match {
      case source: Traversal.Source[_, _] => source
      case step: Traversal.Step[_, _, _, _] =>
      case filter: Traversal.Filter[_, _, _] =>
    }*/
    ???
  }
}

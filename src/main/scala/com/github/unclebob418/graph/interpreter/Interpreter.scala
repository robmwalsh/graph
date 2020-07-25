package com.github.unclebob418.graph.interpreter

import com.github.unclebob418.graph.{ Graph, GraphSchema }
import com.github.unclebob418.graph.Type.VertexType
import com.github.unclebob418.graph.traversal.TraversalType.{ EdgeTraversal, VertexTraversal }
import com.github.unclebob418.graph.traversal.{ Traversal, TraversalType }
import zio.stream.ZStream

object Interpreter {
  def interpret[A, GS <: GraphSchema](traversal: Traversal[Any, A, GS]): Iterable[A] = {

    def go(traversal: Traversal[Any, A, GS]): ZStream[Graph[GS], Nothing, Any] = ???
    /* traversal match {
        case source: Traversal.Source[_, _] =>
        case step: Traversal.Step[_, _, _, _] =>
        case filter: Traversal.Filter[_, _, _] =>
      }*/
    ???
  }
}

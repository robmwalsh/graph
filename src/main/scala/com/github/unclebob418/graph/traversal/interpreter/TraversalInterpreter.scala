package com.github.unclebob418.graph.traversal.interpreter

import com.github.unclebob418.graph.GraphSchema
import com.github.unclebob418.graph.traversal.Traversal

trait TraversalInterpreter[A] {
  def interpret[GS <: GraphSchema](traversal: Traversal[GS]): A
}

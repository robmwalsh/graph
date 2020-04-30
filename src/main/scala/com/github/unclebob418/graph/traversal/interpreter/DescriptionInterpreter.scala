package com.github.unclebob418.graph.traversal.interpreter

import com.github.unclebob418.graph.GraphSchema
import com.github.unclebob418.graph.traversal.Traversal
import com.github.unclebob418.graph.traversal.Traversal.Step.Aggregate.Count
import com.github.unclebob418.graph.traversal.Traversal.Step.{EdgeTraversal, VertexTraversal}

object DescriptionInterpreter {

  def interpret[K, V, GS <: GraphSchema](
    traversal: Traversal.Step[K, V, GS]
  ): List[String] = traversal.foldLeft(List.empty[String])(
    (z, t) =>
      t match {
        case t: VertexTraversal.VSource[K, V, GS]                  => s"Vertex Source (${t.vType})" :: z
        case t: VertexTraversal.VTraversal[K, V, GS]               => s"Vertex Traversal (${t.vType})" :: z
        case t: VertexTraversal.VHas[K, V, GS]                     => s"Vertex Has (${t.vType})" :: z
        case t: EdgeTraversal.ESource[iv, ik, K, V, ov, ok, GS]    => s"Edge Source (${t.eType})" :: z
        case t: EdgeTraversal.ETraversal[iv, ik, K, V, ov, ok, GS] => s"Edge Traversal (${t.eType})" :: z
        case t: EdgeTraversal.EHas[iv, ik, K, V, ov, ok, GS]       => s"Edge Has (${t.eType})" :: z
        case _: Count[GS]                                          => s"Count Aggregate" :: z
      }
  )
}

package com.github.unclebob418.graph.traversal.interpreter

import com.github.unclebob418.graph.traversal.Traversal.Source
import com.github.unclebob418.graph.traversal.Traversal.Step.{ EdgeTraversal, VertexTraversal }
import com.github.unclebob418.graph.GraphSchema
import com.github.unclebob418.graph.traversal.Traversal

object DescriptionInterpreter  {

  def interpret[GS <: GraphSchema](traversal: Traversal[GS]): List[String] = go(traversal).reverse

  def go[GS <: GraphSchema](traversal: Traversal[GS]): List[String] =
    traversal match {
      case source: Traversal.Source[_] =>
        source match {
          case Source.Anonymous(_) =>
            "Anonymous Source" :: Nil
          case Source.GraphTraversalSource(graph) =>
            "GraphSource" :: Nil
        }
      case step: Traversal.Step[_] =>
        step match {
          case VertexTraversal.VSource(vType, tail) =>
            s"$vType vertex source" :: go(tail)
          case VertexTraversal.VTraversal(vType, tail) =>
            s"$vType vertex step" :: go(tail)
          case VertexTraversal.Has(p, vType, tail) =>
            s"$vType has vertex step" :: go(tail)
          case EdgeTraversal.ESource(eType, tail) =>
            s"$eType edge source" :: go(tail)
          case EdgeTraversal.ETraversal(eType, tail) =>
            s"$eType edge step" :: go(tail)
          case EdgeTraversal.Has(p, eType, tail) =>
            s"$eType has edge step" :: go(tail)
        }
    }
}

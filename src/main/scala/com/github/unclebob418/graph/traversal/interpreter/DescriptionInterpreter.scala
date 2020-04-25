package com.github.unclebob418.graph.traversal.interpreter

import com.github.unclebob418.graph.GraphSchema
import com.github.unclebob418.graph.traversal.Traversal
import com.github.unclebob418.graph.traversal.Traversal.Step.{EdgeTraversal, VertexTraversal}

object DescriptionInterpreter {

  def interpret[A, VK, V, IK, IV, EK, E, OK, OV, GS <: GraphSchema](
    traversal: Traversal[VK, V, IK, IV, EK, E, OK, OV, GS]
  ): List[String] =
    traversal.foldLeft(traversal)(List.empty[String])(
      (z, t) =>
        t match {
          case source: Traversal.GraphTraversalSource[_] =>
            "GraphSource" :: z

          case step: Traversal.Step[VK, V, IK, IV, EK, E, OK, OV, GS] =>
            step match {
              case t: VertexTraversal.VSource[VK, V, GS]                  => s"Vertex Source (${t.vType})" :: z
              case t: VertexTraversal.VTraversal[VK, V, GS]               => s"Vertex Traversal (${t.vType})" :: z
              case t: VertexTraversal.Has[VK, V, GS]                      => s"Vertex Has (${t.vType})" :: z
              case t: EdgeTraversal.ESource[IK, IV, EK, E, OK, OV, GS]    => s"Edge Source (${t.eType})" :: z
              case t: EdgeTraversal.ETraversal[IK, IV, EK, E, OK, OV, GS] => s"Edge Traversal (${t.eType})" :: z
              case t: EdgeTraversal.Has[IK, IV, EK, E, OK, OV, GS]        => s"Edge Has (${t.eType})" :: z
            }

        }
    )

}

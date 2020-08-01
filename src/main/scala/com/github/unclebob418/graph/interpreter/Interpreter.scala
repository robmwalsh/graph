package com.github.unclebob418.graph.interpreter

import com.github.unclebob418.graph.traversal.Traversal
import com.github.unclebob418.graph.traversal.Traversal.Step.FlatMap
import com.github.unclebob418.graph.traversal.Traversal.{ Source, Step }
import com.github.unclebob418.graph.{ Graph, GraphSchema }
import zio.stream.ZStream

/**
 * Source -> Stream<p/>
 * Step -> Transducer<p/>
 * Accumulators -> Sinks
 * */
object Interpreter {
  def toStream[A, B, GS <: GraphSchema](
    stream: ZStream[Any, Nothing, A],
    graph: Graph[GS],
    traversal: Traversal[Any, A, GS]
  ): ZStream[Any, Nothing, A] = {

    /*def go[B](
      traversal: Traversal[Any, B, GS],
      streamAcc: Any => ZStream[Any, Nothing, A]
    ): ZStream[Any, Nothing, A] =
      traversal match {
        case source: Source[a, _] => //stream
          source match {
            case Source.FromStream(stream) => ???
            case Source.VertexSource(vType) =>
              ZStream.fromIterable(graph.getVs(vType))
              ???
            case Source.EdgeSource(eType) =>
              ZStream.fromIterable(graph.getEs(eType))
              ???
          }
        case map: Step.MapStep[a, b, c, _] => //transducer
          map match {
            case _: Step.MapStep.ToGraphComponent[_, _, _, _] =>
              //streamAcc()
              ???

            case Step.MapStep.ToValue(f) => ???
            case Step.MapStep.Filter(p)  => ???
          }
        case flatMap: Step.FlatMap[a, b, c, _] => //transducer
          flatMap match {
            case FlatMap.Move.Edge2Vertex.In(vType)    => ???
            case FlatMap.Move.Edge2Vertex.Out(vType)   => ???
            case FlatMap.Move.Vertex2Edge.In(eType)    => ???
            case FlatMap.Move.Vertex2Edge.Out(eType)   => ???
            case FlatMap.Move.Vertex2Vertex.In(vType)  => ???
            case FlatMap.Move.Vertex2Vertex.Out(vType) => ???
          }
        //case filter: Traversal.Filter[_, _, _] => ???
      }*/
    ???
  }
}

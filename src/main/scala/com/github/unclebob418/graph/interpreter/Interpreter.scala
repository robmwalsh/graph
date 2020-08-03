package com.github.unclebob418.graph.interpreter

import com.github.unclebob418.graph.traversal.Traversal
import com.github.unclebob418.graph.traversal.Traversal.Step.{ Filter, FlatMap, MapStep }
import com.github.unclebob418.graph.traversal.Traversal.{ Source, Step }
import com.github.unclebob418.graph.{ Graph, GraphSchema }
import zio.stream.{ Transducer, ZStream, ZTransducer }

/**
 * Source -> Stream<p/>
 * Step -> Transducer<p/>
 * Accumulators -> Sinks
 **/
object Interpreter {
  def toTransducer[R <: Graph[GS], E, A, B, GS <: GraphSchema](
    graph: Graph[GS],
    traversal: Traversal[A, B, GS]
  ): (ZTransducer[R, E, A, B]) = {

    def stepsTransducer[P, O](
      step: Either[Traversal.Source[A, GS], Traversal.Step[A, P, O, GS]],
      transducer: ZTransducer[R, E, O, B]
    ): ZTransducer[R, E, P, B] =
      step match {
        case Left(source) => transducer.asInstanceOf[ZTransducer[R, E, P, B]] // A =:= O =:= P for source
        case Right(step) =>
          val next = step.from match {
            case source: Source[_, GS]       => Left(source.asInstanceOf[Source[A, GS]])
            case previous: Step[A, _, P, GS] => Right(previous.asInstanceOf[Step[A, Any, P, GS]])
          }
          step match {
            // case Traversal.Empty(stream, gs) => transducer.asInstanceOf[ZTransducer[R, E, P, B]] //P =:= O for `Empty`

            case mapStep: MapStep[A, P, O, GS] =>
              mapStep match {
                case component: MapStep.ToGraphComponent[_, _, _, _] => ???
                case toValue: MapStep.ToValue[A, P, O, GS] =>
                  stepsTransducer(next, Transducer.identity.map(toValue.f) >>> transducer)
              }
            case filter: Filter[A, P, GS]  => stepsTransducer(next, transducer.filterInput(filter.p))
            case map: FlatMap[A, P, O, GS] => ???
          }
      }
    ???
  }
  ???

}
/* step match {
        case Traversal.Empty(_, _) => transducer
        case map: MapStep[A, P, O, GS] =>
          map match {
            case _: Step.MapStep.ToGraphComponent[A, P, O, GS] => ???
            case toValue: Step.MapStep.ToValue[A, P, O, GS] =>
              toValue.from match {
                case source: Source[a, GS] => //if it's the source, its output is A
                 //need to deal with map case
                case fromStep: Step[A, p, P, GS] =>
                  stepsTransducer(fromStep, Transducer.identity.map(toValue.f) >>> transducer)
              }

            case filter: Filter[A, O, GS] =>
              filter.from match {
                case source: Source[A, GS] =>
                  (sourceStream(source), transducer) //need to deal with filter case
                case fromStep: Step[A, p, P, GS] =>
                  stepsTransducer(fromStep, filterInput(filter.p, transducer))
              }

          }
        case flatMap: FlatMap[a, b, c, _] => //transducer
          flatMap match {
            case Move.Edge2Vertex.In(vType)    => ???
            case Move.Edge2Vertex.Out(vType)   => ???
            case Move.Vertex2Edge.In(eType)    => ???
            case Move.Vertex2Edge.Out(eType)   => ???
            case Move.Vertex2Vertex.In(vType)  => ???
            case Move.Vertex2Vertex.Out(vType) => ???
          }
      }*/

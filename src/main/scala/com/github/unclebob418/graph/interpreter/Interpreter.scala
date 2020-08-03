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
                case component: MapStep.ToGraphComponent[_, _, _, _] => ??? //todo need graph layer
                case toValue: MapStep.ToValue[A, P, O, GS] =>
                  stepsTransducer(next, Transducer.identity.map(toValue.f) >>> transducer)
              }
            case filter: Filter[A, P, GS]  => stepsTransducer(next, transducer.filterInput(filter.p))
            case map: FlatMap[A, P, O, GS] => ???
          }
      }

    /*traversal match {
      case source: Source[_, GS] => ZTransducer.identity[A]
      case step: Step[A, p, o, GS] =>stepsTransducer(Right(step), ZTransducer.identity[B])
    }*/
    ???
  }
  ???

}
package com.github.unclebob418.graph.interpreter

import com.github.unclebob418.graph.traversal.Traversal
import com.github.unclebob418.graph.traversal.Traversal.Step.{Filter, FlatMap, MapStep}
import com.github.unclebob418.graph.traversal.Traversal.{Source, Step}
import com.github.unclebob418.graph.{Graph, GraphSchema}
import zio.stream.{Transducer, ZStream, ZTransducer}

/**
 * Source -> Stream<p/>
 * Step -> Transducer<p/>
 * Accumulators -> Sinks
 **/
object Interpreter {
  def toTransducer[R <: Graph[GS], E, A, B, GS <: GraphSchema](
    graph: Graph[GS],
    traversal: Traversal[A, B, GS]
  ): (ZStream[R, E, A], ZTransducer[R, E, A, B]) = {

    def sourceStream[O](source: Source[O, GS]): ZStream[R, E, O] = ???

    def stepsTransducer[P, O](
      step: Traversal.Step[A, P, O, GS],
      transducer: ZTransducer[R, E, O, B]
    ): ZTransducer[R, E, P, B] =
      step.from match {
        case previous: Step[A, p, P, GS] =>
          step match {
            case Traversal.Empty(stream, gs) => transducer.asInstanceOf[ZTransducer[R, E, P, B]] //P =:= O for `Empty`
            case mapStep: MapStep[A, P, O, GS] =>
              mapStep match {
                case component: MapStep.ToGraphComponent[_, _, _, _] => ???
                case toValue: MapStep.ToValue[_, _, _, _] => ???
                 // stepsTransducer(previous, Transducer.identity.map(toValue.f) >>> transducer)
              }
            case filter: Filter[_, _, _]   => ???
            case map: FlatMap[A, P, O, GS] => ???
          }
      }

    //case filter: Traversal.Filter[_, _, _] => ???
    /*traversal match {
      case source: Source[A, GS] => (sourceStream(source), ZTransducer.identity[A])
      case step: Step[_, _, _, _] => stepsTransducer(traversal, ZTransducer.identity[B])
    }*/
    ???
  }

  final def filterInput[R, E, I, O](p: I => Boolean, that: ZTransducer[R, E, I, O]): ZTransducer[R, E, I, O] =
    ZTransducer(that.push.map(push => is => push(is.map(_.filter(p)))))
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

package com.github.unclebob418.graph


//pluggable type tags?
trait VertexSchema[V]
trait EdgeSchema[E]
trait GraphSchema[V, E]

trait DefaultGraphSchema[V, E]

trait DefaultVertexSchema[V] extends VertexSchema[V]
object DefaultVertexSchema {

}
trait DefaultEdgeSchema[E] extends EdgeSchema[E]
object DefaultEdgeSchema
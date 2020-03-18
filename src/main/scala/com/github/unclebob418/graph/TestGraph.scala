package com.github.unclebob418.graph

import java.util.UUID

import com.github.unclebob418.graph.TestVertexSchema.{ IntKey, StringKey }

sealed trait TestVertexSchema[V] extends VertexSchema[V] {
  def key(uuid0: UUID = UUID.randomUUID()): VertexKey[V] = new VertexKey[V] {
    override val uuid: UUID = uuid0
  }
}
object TestVertexSchema {
  //todo merge these somehow
  implicit case object SInt                                  extends TestVertexSchema[Int]
  implicit case object SString                               extends TestVertexSchema[String]
  final case class IntKey(uuid: UUID = UUID.randomUUID())    extends VertexKey[Int]
  final case class StringKey(uuid: UUID = UUID.randomUUID()) extends VertexKey[String]

}
sealed trait TestEdgeSchema[F, E, T] extends ESchema[F, E, T]
object TestEdgeSchema {
  implicit case object IntStringString extends TestEdgeSchema[Int, String, String]
  implicit case object StringStringInt extends TestEdgeSchema[String, String, Int]
}

object Test extends App {
  val g = Graph
    .empty[TestVertexSchema, TestEdgeSchema]
    .addV(IntKey(), 5)
    .head
    .addV(StringKey(), "hello")
    .head
    .addV(StringKey(), "world")
    .head
  //g.addV(IntKey(), "not an int") //shouldn't work, doesn't work
  println(g.getAll[String])
}

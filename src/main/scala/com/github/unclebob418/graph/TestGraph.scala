package com.github.unclebob418.graph

import java.util.UUID

import com.github.unclebob418.graph.TestVertexSchema.{ IntKey, StringKey }

sealed trait TestVertexSchema[K, V] extends VertexSchema[K, V]
object TestVertexSchema {
  //todo merge these somehow
  implicit case object SInt                                  extends TestVertexSchema[UUID, Int] {}
  implicit case object SString                               extends TestVertexSchema[UUID, String]
  final case class IntKey(uuid: UUID = UUID.randomUUID())    extends VertexKey[UUID, Int]
  final case class StringKey(uuid: UUID = UUID.randomUUID()) extends VertexKey[UUID, String]

}
sealed trait TestEdgeSchema[F, E, T] extends ESchema[F, E, T]
object TestEdgeSchema {
  implicit case object IntStringString extends TestEdgeSchema[Int, String, String]
  implicit case object StringStringInt extends TestEdgeSchema[String, String, Int]
}

object Test extends App {
  val k1 = StringKey(UUID.nameUUIDFromBytes(Array[Byte](1)))
  val k2 = StringKey(UUID.nameUUIDFromBytes(Array[Byte](2)))
  val g = Graph
    .empty[TestVertexSchema, TestEdgeSchema]
    .addV(IntKey(), 5)
    .addV(k1, "hello")
    .addV(k2, "world")
  //g.addV(IntKey(), "not an int") //shouldn't work, doesn't work
  println(g.getAll[UUID, Int])
  println(g.getV(k1))
  val strings: Map[VertexKey[UUID, String], String] =  g.getAll[UUID, String].head
  println(strings)
  println(strings.get(k1))

}

package com.github.unclebob418.graph

import java.util.UUID

import com.github.unclebob418.graph.TestVertexSchema.{ IntKey, StringKey, TestVertexSchema }

object TestVertexSchema {
  sealed trait TestVertexSchema[K, V] extends VertexSchema[K, V]
  sealed trait TestVertexKey[K, V]    extends VertexKey[K, V]
  //todo merge these somehow
  implicit case object IntKey                            extends TestVertexSchema[UUID, Int]
  final case class IntKey(key: UUID = UUID.randomUUID()) extends TestVertexKey[UUID, Int]

  implicit case object StringKey                            extends TestVertexSchema[UUID, String]
  final case class StringKey(key: UUID = UUID.randomUUID()) extends TestVertexKey[UUID, String]

}

sealed trait IntSchema[K, V] extends VertexSchema[K, V]
object IntSchema {
  implicit case object IntKey       extends IntSchema[Int, Int]
  final case class IntKey(key: Int) extends VertexKey[Int, Int]
}

sealed trait StringSchema[K, V] extends VertexSchema[K, V]
object StringSchema {
  implicit case object StringKey          extends StringSchema[String, String]
  final case class StringKey(key: String) extends VertexKey[String, String]
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
  println(g.getAll(StringKey))
  println(g.getV(k1))
  val strings = g.getAll[UUID, String].head
  println(strings)
  println(strings.get(k1))

  val intGraph = Graph
    .empty[IntSchema, TestEdgeSchema]
    .addV(IntSchema.IntKey(10), 5)

  val stringGraph = Graph
    .empty[StringSchema, TestEdgeSchema]
    .addV(StringSchema.StringKey("hello"), "world")

/*  Doesn't work, would like it to
    val gIntString = Graph
    .empty[IntSchema[_, _] with StringSchema[_, _], TestVertexSchema]
    .addV(IntSchema.IntKey(10), 5)
    .addV(StringSchema.StringKey("hello"), "world")*/
}

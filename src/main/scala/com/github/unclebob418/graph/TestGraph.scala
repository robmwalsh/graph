package com.github.unclebob418.graph

import java.util.UUID

import com.github.unclebob418.graph.TestEdgeSchema.StringEKey
import com.github.unclebob418.graph.TestVertexSchema.{IntKey, StringKey, TestVertexSchema}

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

sealed trait TestEdgeSchema[F, E, T] extends EdgeSchema[F, E, T]
object TestEdgeSchema {
  sealed trait TestEdgeKey[K, V]                             extends EdgeKey[K, V]
  implicit case object IntStringString                       extends TestEdgeSchema[Int, String, String]
  implicit case object StringStringString                      extends TestEdgeSchema[StringKey, String, StringKey]
  final case class StringEKey(key: UUID = UUID.randomUUID()) extends TestEdgeKey[UUID, String]
}



object Test extends App {
  val k1  = StringKey(UUID.nameUUIDFromBytes(Array[Byte](1)))
  val k2  = StringKey(UUID.nameUUIDFromBytes(Array[Byte](2)))
  val ek1 = StringEKey(UUID.nameUUIDFromBytes(Array[Byte](3)))
  val g = (Some(
    Graph
      .empty[TestVertexSchema, TestEdgeSchema]
  ) flatMap (_.addV(IntKey(), 5))
    flatMap (_.addV(k1, "hello"))
    flatMap (_.addV(k2, "world"))
    flatMap (_.addE(k1, ek1, " joined to ", k2))).head
  //g.addV(IntKey(), "not an int") //shouldn't work, doesn't work
  println(g.getAllVs(StringKey))
  println(g.getV(k1))
  println(g)
  val strings = g.getAllVs[UUID, String].head
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

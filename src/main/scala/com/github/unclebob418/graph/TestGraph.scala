package com.github.unclebob418.graph

import java.util.UUID

import com.github.unclebob418.graph.TestEdgeSchema.StringEKey
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

sealed trait TestEdgeSchema[_] extends EdgeSchema[TestVertexSchema]
object TestEdgeSchema {
  sealed trait TestEdgeKey[K, V] extends EdgeKey[K, V]
  implicit case object StringToString extends TestEdgeSchema[TestVertexSchema[_, _]] {
    override type In  = TestVertexSchema[UUID, String]
    override type Out = TestVertexSchema[UUID, String]
    override type E   = String
  }
  final case class StringEKey(key: UUID = UUID.randomUUID()) extends TestEdgeKey[UUID, String]
}

object Test extends App {
  val stringKey1 = StringKey()  //UUID.nameUUIDFromBytes(Array[Byte](1)))
  val stringKey2 = StringKey()  //UUID.nameUUIDFromBytes(Array[Byte](2)))
  val intKey1    = IntKey()     //UUID.nameUUIDFromBytes(Array[Byte](3)))
  val ek1        = StringEKey() //UUID.nameUUIDFromBytes(Array[Byte](4)))
  val g = (Some(Graph.empty[TestVertexSchema, TestEdgeSchema[TestVertexSchema[_, _]]])
    flatMap (_.addV(intKey1, 5))
    flatMap (_.addV(stringKey1, "hello"))
    flatMap (_.addV(stringKey2, "world"))
    flatMap (_.addV(intKey1, 42))
    flatMap (_.addE(stringKey1, ek1, " joined to ", stringKey2))
    flatMap (_.addE(intKey1, ek1, " joined to ", stringKey2))
    ).head
  println(g.getVs(StringKey))
  println(g.getV(stringKey1))
  println(g)
  val strings = g.getVs[UUID, String].head
  println(strings)
  println(strings.get(stringKey1))
  /*  Doesn't work, would like it to
    val gIntString = Graph
    .empty[IntSchema[_, _] with StringSchema[_, _], TestVertexSchema]
    .addV(IntSchema.IntKey(10), 5)
    .addV(StringSchema.StringKey("hello"), "world")*/
}

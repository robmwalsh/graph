package com.github.unclebob418.graph

import java.util.UUID

import com.github.unclebob418.graph.TestEdgeSchema.StringEKey
import com.github.unclebob418.graph.TestVertexSchema.{ IntKey, StringKey, TestVertexSchema }
import com.github.unclebob418.graph.WrongVertexSchema.{ BooleanKey, WrongVertexSchema }

object TestVertexSchema {
  val r = scala.util.Random
  sealed trait TestVertexSchema[K, V] extends VertexSchema[K, V]
  sealed trait TestVertexKey[K, V]    extends VertexKey[K, V]
  //todo merge these somehow
  implicit case object IntKey                   extends TestVertexSchema[Int, Int]
  final case class IntKey(key: Int = r.nextInt) extends TestVertexKey[Int, Int]

  implicit case object StringKey                   extends TestVertexSchema[Int, String]
  final case class StringKey(key: Int = r.nextInt) extends TestVertexKey[Int, String]

}
object WrongVertexSchema {
  sealed trait WrongVertexSchema[K, V] extends VertexSchema[K, V]
  sealed trait WrongVertexKey[K, V]    extends VertexKey[K, V]
  //todo merge these somehow
  implicit case object BooleanKey                            extends WrongVertexSchema[UUID, Boolean]
  final case class BooleanKey(key: UUID = UUID.randomUUID()) extends WrongVertexKey[UUID, Boolean]

}

sealed trait WrongEdgeSchema
object WrongVertexSchema {
  sealed trait TestEdgeKey[K, V] extends EdgeKey[K, V]
  implicit case object StringToString extends WrongEdgeSchema {
    override type In  = TestVertexSchema[Int, String]
    override type Out = TestVertexSchema[Int, String]
    override type E   = String
  }
  final case class StringEKey(key: Int) extends TestEdgeKey[Int, String]

}

sealed trait TestEdgeSchema extends EdgeSchema[TestVertexSchema]
object TestEdgeSchema {
  sealed trait TestEdgeKey[K, V] extends EdgeKey[K, V]
  implicit case object StringToString extends TestEdgeSchema {
    override type In  = TestVertexSchema[Int, String]
    override type Out = TestVertexSchema[Int, String]
    override type E   = String
  }
  final case class StringEKey(key: Int) extends TestEdgeKey[Int, String]
}

object Test extends App {
  implicit val graphSchema = new GraphSchema {
    override type VS[K, V] = TestVertexSchema[K, V]
    override type ES       = TestEdgeSchema
  }

  val stringKey1 = StringKey(1)
  val stringKey2 = StringKey(2)
  val intKey1    = IntKey(3)
  val boolKey1   = BooleanKey()
  val ek1        = StringEKey(2)

  val g = (Some(Graph.empty)
    flatMap (_.addV(intKey1, 5))
    flatMap (_.addV(stringKey1, "hello"))
    flatMap (_.addV(stringKey2, "world"))
    flatMap (_.addV(intKey1, 42))
  //flatMap (_.addV(boolKey1, false))
    flatMap (_.addE(stringKey1, ek1, " joined to ", stringKey2))
  //flatMap (_.addE(intKey1, ek1, " joined to ", stringKey2))
  ).head
  println("getVs(StringKey)")
  println(g.getVs(StringKey))
  println("getV(stringKey1)")
  println(g.getV(stringKey1))
  println("es.es.toString()")
  println(g.es.es.toString())

  /*  Doesn't work, would like it to
    val gIntString = Graph
    .empty[IntSchema[_, _] with StringSchema[_, _], TestVertexSchema]
    .addV(IntSchema.IntKey(10), 5)
    .addV(StringSchema.StringKey("hello"), "world")*/
}

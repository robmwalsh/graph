package zio.stm.graph

import Key.VertexKey
import zio.stm.graph.Key.VertexKey


sealed trait Path {
  def :*:[HK, HV](head: VertexKey[HK, HV]): Path
}

object Path {
  type Empty = Empty.type

  case object Empty extends Path {
    override def :*:[HK, HV](head: VertexKey[HK, HV]): VCons[HK, HV, Empty] = VCons(head, Empty)
  }

  case class VCons[HK, HV, B](head: VertexKey[HK, HV], tail: B) extends Path { self =>
    override def :*:[NK, NV](head: VertexKey[NK, NV]): VCons[NK, NV, VCons[HK, HV, B]] = VCons(head, self)
  }
}


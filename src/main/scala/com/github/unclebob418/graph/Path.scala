package com.github.unclebob418.graph


sealed trait Path {
  def :*:[HK, HV](head: VertexKey[HK, HV]): Path
}

/*object Path {
  type Empty = Empty.type

  type :*: [IV <: VertexType[_,_ ], OV] = Cons[HK, HV, B]

  case object Empty extends Path {
    override def :*:[HK, HV](head: VertexKey[HK, HV]): Cons[HK, HV, Empty] = Cons(head, Empty)
  }

  case class VCons[HK, HV, B](head: VertexKey[HK, HV], tail: B) extends Path { self =>
    override def :*:[NK, NV](head: VertexKey[NK, NV]): Cons[NK, NV, Cons[HK, HV, B]] = Cons(head, self)
  }

  case class ECons

}*/


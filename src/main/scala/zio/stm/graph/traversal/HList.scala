package zio.stm.graph.traversal

//https://contramap.dev/2020/02/29/hlist.html
sealed trait HList {
  type Append[B <: HList] <: HList
  def ++[That <: HList](that: That): Append[That]
}
object HList {

  type Aux[A]             = HList { type Append = A }
  type :*:[A, B <: HList] = Cons[A, B]

  type Empty = Empty.type

  case object Empty extends HList {
    override type Append[B <: HList] = B

    override def ++[That <: HList](that: That): That = that
  }

  case class Cons[A, B <: HList](head: A, tail: B) extends HList { self =>
    override type Append[C <: HList] = Cons[A, tail.Append[C]]

    override def ++[That <: HList](that: That): self.Append[That] =
      Cons(head, tail ++ that)
  }

  object :*: {
    def unapply[A, B <: HList](cons: Cons[A, B]): Option[(A, B)] =
      Some((cons.head, cons.tail))
  }

  implicit class ops[A <: HList](a: A) {
    def :*:[B](b: B): B :*: A = Cons(b, a)
  }
}

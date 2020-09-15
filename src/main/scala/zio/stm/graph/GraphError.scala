package zio.stm.graph

sealed trait GraphError {
  val message: String
}

object GraphError {
  sealed case class Generic(message: String) extends GraphError
  sealed case class VertexMissing(message: String) extends GraphError
  sealed case class EdgeMissing(message: String) extends GraphError
  sealed case class VertexExists(message: String) extends GraphError
  sealed case class EdgeExists(message: String) extends GraphError
  sealed case class VertexNotFound(message: String) extends GraphError
  sealed case class EdgeNotFound(message: String) extends GraphError
}
package com.github.unclebob418.graph

//import com.github.unclebob418.graph.TestVertexSchema.TestVertexSchema

abstract class GraphSchema {
  type VS[_, _] <: VertexSchema[_, _]
}

/*object ASchema extends GraphSchema {
  override type VS = TestVertexSchema[_, _]
  override type ES = TestEdgeSchema[_, _, _]
}*/


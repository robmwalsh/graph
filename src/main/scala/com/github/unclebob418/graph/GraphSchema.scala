package com.github.unclebob418.graph


trait GraphSchema {
  type VS[K, V] <: VertexSchema[K,V]
  type ES <: EdgeSchema[VS]
}

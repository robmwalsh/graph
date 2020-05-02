# Graph
This is a work in progress. It doesn't do much yet :)

# Goals
 * Type Safe
 * Schema Based
 * In-memory
 * Purely functional

# Why?
scala-graph limits node/edge types
tinkerpop/gremlin is "stringly" typed, and has no schema. Gremlin-scala is a nice wrapper which adds some type safety, but still no schema

neither allows composition of schemas


# Inspiration
Heavily inspired by apache tinkerpop/gremlin/gremlin-scala, but trying to avoid so many strings...
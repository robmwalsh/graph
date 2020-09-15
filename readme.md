# ZIO Graph
Type Safe graph library traversals for the ZIO ecosystem.

This is a work in progress.

# Goals
 * Useful Errors
 * Purely functional
 * Tight integration with ZIO & ZIO Streams
## Graph
 * Schema based
 * Support for transactions
 * In-memory (for now)
## Traversals
 * Type safe
 * Bounded memory

# Why?
scala-graph limits node/edge types
tinkerpop/gremlin is "stringly" typed, and has no schema. Gremlin-scala is a nice wrapper which adds some type safety, but still no schema

neither allows composition of schemas

# Name


# Inspiration
Heavily inspired by apache tinkerpop/gremlin/gremlin-scala, but trying to avoid so many strings...
# WaterWorks
Type Safe graph traversals for ZIO Streams.

This is a work in progress.

# Goals
 * Type safe traversals
 * Schema based
 * Useful Errors
 * Bounded memory
 * Purely functional
 * Tight integration with ZIO & ZIO Streams

# Why?
scala-graph limits node/edge types
tinkerpop/gremlin is "stringly" typed, and has no schema. Gremlin-scala is a nice wrapper which adds some type safety, but still no schema

neither allows composition of schemas

# Name


# Inspiration
Heavily inspired by apache tinkerpop/gremlin/gremlin-scala, but trying to avoid so many strings...
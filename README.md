# nimsumtree

## Important note
Still early in prototyping phase. Barely anything is implemented yet (e.g. no atomic RC)

This library provides a tree data structure similar to a B-tree,
but each node stores user defined summaries of it's children.

Trees are immutable and it uses atomic RC so that trees can be shared across threads,
but if only one reference to a node exists it will be mutated instead.

This data structure is suitable for e.g. representing file contents in text editors.

The implementation is inspired by sum trees in [Zed](https://github.com/zed-industries/zed)

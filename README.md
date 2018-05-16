Experimenting with different implementations of cached cells in PureScript.

Cached cells is a term I just made up. Essentially it's SAC (like Incremental),
but without the "push" part (no callbacks about updates).

There are two types of cells:

- Root cells. Their value can be updated from the outside.
- Derived cells. They compute their value based on some other cells.

All cells can be read. Derived cell values are cached, and are only recomputed
when the dependencies change (a'la `make`).

# tree_db key value storage

tree_db is a key value storage for items that have keys ordered
in a tree like way.

A key consist of a list of atoms and integers where integer are
normally used as array index.

An external representation is for example a.b[1].c.d for the internal
key [a,b,1,c,d].
This storage makes it easy to group related data into trees that may
be traversed over children and siblings.
There is also support for more advanced matching.

# Fairy Pieces Engine

The Fairy Pieces Engine is a simulation and verification engine for chess-like games.
Licensed under AGPL v3.0.

# Goals

1. Provide data structures capable of representing chess and its many derivative variants.
2. Provide the ability to represent custom pieces with user-defined moves.

Which variants should the engine be capable of representing?

* variants with incomplete information:
    * [kriegspiel (war game)](https://en.wikipedia.org/wiki/Kriegspiel_(chess)) -- cannot see enemy pieces;
    * [dark chess (fog of war)](https://en.wikipedia.org/wiki/Dark_chess) -- can only see tiles reachable by a move;
* multimove variants:
    * [progressive chess](https://en.wikipedia.org/wiki/Progressive_chess);
    * [marseillais chess](https://en.wikipedia.org/wiki/Marseillais_chess);
* variants with different victory conditions:
    * [losing chess](https://en.wikipedia.org/wiki/Losing_Chess);
    * a variable number of *royal pieces* (including 0);
* custom (asymmetric) setup variants;
* variants with custom board size or shape:
    * [infinite chess](https://en.wikipedia.org/wiki/Infinite_chess);
    * [hexagonal chess](https://en.wikipedia.org/wiki/Hexagonal_chess);
    * [triangular chess](https://en.wikipedia.org/wiki/Triangular_chess_(game));
    * [rhombic chess](https://en.wikipedia.org/wiki/Rhombic_chess);
* multiplayer variants:
    * [four-player chess](https://en.wikipedia.org/wiki/Four-player_chess);
    * [team chess](https://en.wikipedia.org/wiki/Team_chess);
    * taking turns;
    * one player decides on piece (or piece type) to be moved, his teammate decides on the move;

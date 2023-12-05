# haskell-board-games

Haskell board games is a tool for analysing game trees of different board games. Currently only tic-tac-toe is supported

## Installation

Install [GHCi](https://downloads.haskell.org/ghc/latest/docs/users_guide/ghci.html) and use the interactive shell to interact with the library.

## Usage

foldRose, height, size, leaves, leavesCount, elemsOnDepth - tree operations.

BoardState is a monad containing current player and list of board fields representing the current game state.

validMoves, playMove, isFinished, createGameTree - operations on BoardState.

Chain applyMove operations using bind to simulate a game.

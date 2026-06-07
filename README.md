# almas-chess

A chess engine in Clojure inspired by [AlphaZero](https://arxiv.org/abs/1712.01815): Monte Carlo Tree Search guided by a neural network, trained through self-play reinforcement learning.

## Requirements

- Java (JRE 8+)
- [Leiningen](https://leiningen.org/)

## Building

```bash
./makeBin.sh
```

## Play (UCI)

```bash
./almas
```

The engine loads `models/network.edn` if present, otherwise starts with a random network. Search uses AlphaZero-style PUCT MCTS with the network for policy priors and leaf values.

## Train (self-play)

```bash
lein run train :iterations 5 :games 2 :simulations 50
```

Or after building the uberjar:

```bash
./almas train :iterations 10 :games 4 :simulations 100
```

Options:

| Flag | Default | Meaning |
|------|---------|---------|
| `:iterations` | 5 | Training iterations (self-play + SGD) |
| `:games` | 2 | Self-play games per iteration |
| `:simulations` | 50 | MCTS simulations per move during self-play |
| `:batch` | 16 | Minibatch size for gradient updates |
| `:threads` | CPU count | Parallel self-play worker threads |
| `:max-plies` | 200 | Cap game length (prevents multi-hour stuck games) |
| `:minutes` | (none) | Stop training after this many minutes |

**Note:** Self-play is CPU-heavy. A rough starting point for overnight runs:

```bash
lein run train :iterations 5 :games 4 :simulations 25 :threads 8
```

Time-limited run (30 minutes):

```bash
lein run train :iterations 100 :games 8 :simulations 50 :threads 8 :minutes 30
```

The network is saved to `models/network.edn` after each iteration.

## Architecture

```
UCI / self-play
      │
      ▼
  chess.az          PUCT MCTS (policy priors + value from NN)
      │
      ▼
  chess.nn          Dual-head network (4096 policy + scalar value)
      ▲
      │
  chess.train       Replay buffer + SGD
      ▲
      │
  chess.selfplay    Generate (position, MCTS policy, outcome) examples
      │
      ▼
  chess.basics      Board, legal moves, game result
  chess.encoding    13×64 board planes → feature vector
  chess.action      Move ↔ action index (4096)
  chess.native      Optional Stockfish subprocess (validmoves)
```

## Native move generation (recommended for training)

MCTS calls legal-move generation heavily. Pure Clojure `valid-moves` is the main bottleneck; patched Stockfish speeds self-play dramatically.

Build the patched binary (requires git, make, g++):

```bash
chmod +x scripts/build-stockfish.sh
./scripts/build-stockfish.sh
export ALMAS_STOCKFISH="$PWD/vendor/stockfish/src/stockfish"
```

Then train as usual — the engine probes Stockfish on startup:

```bash
lein run train :iterations 5 :games 8 :simulations 25 :threads 8 :minutes 30
```

Environment:

| Variable | Default | Meaning |
|----------|---------|---------|
| `ALMAS_STOCKFISH` | `stockfish` | Path to patched Stockfish binary |
| `ALMAS_NATIVE` | (enabled) | Set to `false` to force Clojure move gen |

If Stockfish is unavailable, training falls back to Clojure automatically.

## Note on Stockfish

`src/chess/uci.cpp` is the original reference patch source. The build script applies `scripts/stockfish-validmoves.patch` to Stockfish instead of replacing the whole file.

## License

Copyright © 2020

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.

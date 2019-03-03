```bash
$ yarn install
$ ./node_modules/.bin/elm-live src/EventQueen.elm -- --output=elm.js
```

# To do

## Yes

* wrapper type for node names
* ~~growing dictionary~~ table-like CRDT
* code for syncing multiple clients
* "fit all" functionality in board menu
* "new card" functionality in board menu
* view settings in one place (not scattered all over the code)
* "remove" functionality in note menu
* "edit" functionality in note menu
* either "note" or "card" as a name in all places
* improve tests by using randomized initial states
* error handling for CRDT operations

## Later

* context menu adapts based on where on screen it is being opened up
* multiple boards
* user-information
* "history" functionality in board menu
* "history" functionality in note menu
* search across the board

## Maybe Later

* richer operation-combination API
* card layers
* card labels
* non-card objects

## No

* user permissions control

## Done

* view model
* MVR CRDT
* CRDT testing setup
* draggable board
* draggable cards
* multiple visible cards
* context menu
* improve the CRDT modifier function signatures
* CRDT state
* in tests: state-aware concretization of simulation operations
# Life

A fair bit more to do, but this version of John Conway's Life works (on Android)

It uses F# / Fabulous

Currently the Life board is not configurable though the hooks to do so are in place

Life board size is fixed at a 10 * 10 grid

Life board initialises with random start up seed.  Clicking the reset button reseeds the board.

There are two 'playing' modes Edged and Edgeless.  In the Edged version a corner cell only has 3 neighbours, while  in the edgeless version all cells have 8 neighbours.

When the timer is on, 'evolution' proceeds at 1 'frame' a second.

When no changes occur, the timer stops (albeit not quite immediately - need to send cancellation).  Also the timer button doesn't not update when this happens so you may have to press it twice to start and stop.


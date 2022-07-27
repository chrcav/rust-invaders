# 8080 Space Invaders Emulator

Work in progress 8080 Emulator in rust based on [this tutorial](http://emulator101.com/). Used [computerarcheology.com](http://computerarcheology.com/Arcade/SpaceInvaders/Hardware.html) and [8080 Programmers Manual](https://drakeor.com/uploads/8080-Programmers-Manual.pdf) as further reference material.

## Running

In order to run: `cargo r` it expects the file `invaders.bin` in the current working directory. This can be generated base on the guide at [emulator101.com](http://www.emulator101.com/memory-maps.html).

## Controls

* player 1
  * Use `A` to move left and `D` to move right as player 1.
  * `1` is 1 player start
  * `Space` to shoot
* player 2
  * Use `Left` to move left and `Right` to move right as player 1.
  * `2` is 2 player start
  * `X` to shoot
* `C` adds a credit
* `P` pauses processing op codes
* `G` dumps the assembly
* `H` takes a screen shot
* `Escape` exits

## Missing Features

- [ ] Sound

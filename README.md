# 8080 Space Invaders Emulator

Work in progress 8080 Emulator in rust based on [this tutorial](http://emulator101.com/). Used [computerarcheology.com](http://computerarcheology.com/Arcade/SpaceInvaders/Hardware.html) and [8080 Programmers Manual](https://drakeor.com/uploads/8080-Programmers-Manual.pdf) as further reference material.

## Running

In order to run: `cargo r` it expects the file `invaders.bin` in the current working directory. This can be generated base on the guide at [emulator101.com](http://www.emulator101.com/memory-maps.html).

## Controls

* Use `A` to move left and `D` to move right as player 1.
* `C` adds a credit
* `Enter` is 1 player start
* `Space` to shoot
* `P` pauses processing op codes
* `G` dumps the assembly
* `H` takes a screen shot
* `Escape` exits

## Missing Features

- [ ] Sound
* [ ] Player 2 controls

module LabProg2019.Game_mode1

open System
open Engine
open Gfx
open Maze
    

[< NoEquality; NoComparison >]
type state = {
    player : sprite
}

let main_game (W, H) =     
    // initialize engine and maze
    let engine = new engine (W, H)
    let m = maze.generate (W - 4, H - 4)
    let (p_x, p_y) = m.rnd_room
    let (e_x, e_y) = m.rnd_room
    let mutable is_exit = false

    let my_update (key : ConsoleKeyInfo) (screen : wronly_raster) (st : state) =
        // move player
        let dx, dy =
            match key.KeyChar with 
            | 'w' -> 0., -1.
            | 's' -> 0., 1.
            | 'a' -> -1., 0.
            | 'd' -> 1., 0.
            | _   -> 0., 0.

        let n_x = int (st.player.x + dx) - 1
        let n_y = int (st.player.y + dy) - 3
        is_exit <- (e_x = n_x) && (e_y = n_y)

        // moove + win conditions
        if (is_exit) then
            st, true
        elif (m.is_possible_path (0, 0, n_x, n_y)) then
            st.player.move_by (dx, dy)
            st, key.KeyChar = 'q'
        else
            st, key.KeyChar = 'q'

    // create player and maze sprites
    let player = engine.create_and_register_sprite (image.point (pixel.filled Color.Red), p_x + 1, p_y + 3, 1)
    ignore <| engine.create_and_register_sprite (image.maze (m, W - 4, H - 4,  pixel.filled Color.Gray), 1, 3, 3)
    ignore <| engine.create_and_register_sprite (image.point (pixel.filled Color.Blue), e_x + 1, e_y + 3, 1)

    // initialize state
    let st0 = { 
        player = player
        }
    // start engine
    engine.loop_on_key my_update st0
    // return win condition
    is_exit
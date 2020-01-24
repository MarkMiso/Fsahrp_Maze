module LabProg2019.Game_mode3

open System
open Engine
open Gfx
open Maze
open Ai
    

[< NoEquality; NoComparison >]
type state = {
    player : sprite
}

let main_game (W, H) =
    // intialize engine, maze and solver
    let engine = new engine (W, H)
    let m = maze.generate (W - 4, H - 4)
    let (p_x, p_y) = m.rnd_room
    let (e_x, e_y) = m.rnd_room
    let mutable path = Ai.maze_solver1 (m, p_x, p_y, e_x, e_y)
    let mutable is_exit = false

    let my_update (key0 : ConsoleKeyInfo option) (screen : wronly_raster) (st : state) =
        // auto move player + moove and win conditions
        match path with
        |[] -> 
            st, true
        |(x, y)::xs ->
            st.player.x <- float (x + 1)
            st.player.y <- float (y + 3)

            ignore <| engine.create_and_register_sprite (image.point (pixel.filled Color.Yellow), x + 1, y + 3, 1)

            is_exit <- (e_x = x) && (e_y = y)
            path <- xs

            if (is_exit) then
                st, true
            else
                match key0 with
                |None -> 
                    st, false
                |Some key -> 
                    if (key.KeyChar = 'q') then st, true
                    else st, false

    // create player and maze sprites
    let player = engine.create_and_register_sprite (image.point (pixel.filled Color.Red), p_x + 1, p_y + 3, 1)
    ignore <| engine.create_and_register_sprite (image.maze (m, W - 4, H - 4,  pixel.filled Color.Gray), 1, 3, 3)
    ignore <| engine.create_and_register_sprite (image.point (pixel.filled Color.Blue), e_x + 1, e_y + 3, 1)

    // initialize state
    let st0 = { 
        player = player
        }
    // start engine
    engine.loop my_update st0
    // returns win condition
    is_exit

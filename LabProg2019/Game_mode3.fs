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
    let m = new maze (W, H)
    let (p_x, p_y) = m.maze_entrance
    let mutable path = Ai.maze_solver0 (m, p_x, p_y)
    let mutable is_exit = false

    let my_update (key0 : ConsoleKeyInfo option) (screen : wronly_raster) (st : state) =
        // auto move player + moove and win conditions
        match path with
        |[] -> 
            st, true
        |(x, y)::xs ->
            st.player.x <- float x
            st.player.y <- float (y + 3)
            ignore <| engine.create_and_register_sprite (image.point (pixel.filled Color.Yellow), x, y + 3, 0)
            
            is_exit <- m.is_exit(0, 0, x, y)
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
    let player = engine.create_and_register_sprite (image.point (pixel.filled Color.Red), p_x, p_y + 3, 1)
    ignore <| engine.create_and_register_sprite (image.maze (m, W - 2, H - 3,  pixel.filled Color.Gray), 0, 3, 3)

    // initialize state
    let st0 = { 
        player = player
        }
    // start engine
    engine.loop my_update st0
    // returns win condition
    is_exit
module LabProg2019.Game_mode3

open System
open Engine
open Gfx
open Maze
open Ai
    

[< NoEquality; NoComparison >]
type state = {
    sol1 : sprite
    sol2 : sprite
}

let main_game (W, H) =
    // intialize engine, maze and solver
    let engine = new engine (W, H)
    let m = maze.generate (W - 4, H - 4)
    let (p_x, p_y) = m.rnd_room
    let (e_x, e_y) = m.rnd_room
    let path1 = Ai.maze_solver1 (m, p_x, p_y, e_x, e_y, true)
    let path2 = Ai.maze_solver2 (m, p_x, p_y, e_x, e_y, true)
    let dx = float (H + 1)

    let my_update (key : ConsoleKeyInfo) (screen : wronly_raster) (st : state) =
        //choose what path to visualize
        match key.KeyChar with
        |'w' -> 
            st.sol1.x <- dx
            st.sol2.x <- 1.
        |'s' -> 
            st.sol2.x <- dx
            st.sol1.x <- 1.
        |_ -> ()

        st, key.KeyChar = 'q'

    // create ths sprites
    let sol1 = engine.create_and_register_sprite (image.path (path1, W, H, pixel.filled Color.DarkGray), 1, 3, 1)
    let sol2 = engine.create_and_register_sprite (image.path (path2, W, H, pixel.filled Color.DarkBlue), H + 1, 3, 1)
    ignore <| engine.create_and_register_sprite (image.point (pixel.filled Color.Red), p_x + 1, p_y + 3, 2)
    ignore <| engine.create_and_register_sprite (image.maze (m, W - 4, H - 4, pixel.filled Color.White, pixel.filled Color.Black), 1, 3, 0)
    ignore <| engine.create_and_register_sprite (image.point (pixel.filled Color.Blue), e_x + 1, e_y + 3, 2)

    // initialize state
    let st0 = { 
        sol1 = sol1
        sol2 = sol2
        }
    // start engine
    engine.loop_on_key my_update st0

module LabProg2019.Demo

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
    let m = maze.generate (10, 10)

    let my_update (key : ConsoleKeyInfo) (screen : wronly_raster) (st : state) =
        // move player
        let dx, dy =
            match key.KeyChar with 
            | 'w' -> 0., -1.
            | 's' -> 0., 1.
            | 'a' -> -1., 0.
            | 'd' -> 1., 0.
            | _   -> 0., 0.

        let player_x = int st.player.x
        let player_y = int st.player.y

        

        st.player.move_by(dx, dy)
        
        st, key.KeyChar = 'q'

    // create player and maze sprites
    let player = engine.create_and_register_sprite (image.point (pixel.filled Color.Red), 15, 15, 1)
    ignore <| engine.create_and_register_sprite (image.rectangle (3, 3, pixel.wall Color.White), 10, 10, 0)
    //ignore <| engine.create_and_register_sprite (image.maze (m, 10, 10, pixel.filled Color.White, pixel.filled Color.Cyan), 3, 3, 0)

    // initialize state
    let st0 = { 
        player = player
        }
    // start engine
    engine.loop_on_key my_update st0
    // return win condition
    true
module LabProg2019.End

open System
open Engine
open Gfx

[< NoEquality; NoComparison >]
type state = {
    player : sprite
}

let End (W, H, win_conditon) =       
    let engine = new engine (W, H)
    let my_update (key : ConsoleKeyInfo) (screen : wronly_raster) (st : state) =
        st, key.KeyChar = 'q'

    // create text
    
    let text = 
        if win_conditon then
            engine.create_and_register_sprite (image.text ("You Won", Color.White), (W - 11) / 2, (H - 3) / 2, 0)
        else
            engine.create_and_register_sprite (image.text ("You Lost", Color.White), (W - 11) / 2, (H - 3) / 2, 0)

    // initialize state
    let st0 = { 
        player = text
        }
    // start engine
    engine.loop_on_key my_update st0

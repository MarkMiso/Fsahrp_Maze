(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* Maze.fs: Maze generetor
* (C) 2019 Alvise Spano' @ Universita' Ca' Foscari di Venezia
*)

module LabProg2019.Maze

open Globals

// tipo per indicare la direzione
type direction = 
    Up|Left|Right|Down|Stay


// tipo cella, ogni cella rapresenta un pezzo 2 * 2 del labirinto
type cell () =
    member val corner : bool = true                            // indica l'angolo in alto a sinistra della cella  2 * 2
    member val room : bool = true with get, set                // indica l'angolo in basso a destra della cella  2 * 2, utilizzato come controllo per vedere se la cella è stata generata
    member val wall_up : bool = true with get, set             // indica l'angolo in alto a destra della cella  2 * 2
    member val wall_left : bool = true with get, set           // indica l'angolo in basso a sinistra della cella  2 * 2

    // metodo per rimuovere un muro data una direzione
    member this.remove_wall (d : direction) =
        match d with
        |Up -> this.wall_up <- false
        |Left -> this.wall_left <- false
        |_ -> ()

    // metodo per seganre una cella come visitata
    member this.mark_created =
        this.room <- false


// tipo labirinto foramto da un array di celle 
type maze (w, h) =
    // inizializza l'array del labirinto
    let array1 = [| for i in 1 .. (w * h) -> (new cell ()) |]
    
    // metodo per interagire con le singole celle del labirinto date le loro coordinate x e y
    member this.cell (x : int, y : int) =
        if (((x >= 0) && (x < w)) && ((y >= 0) && (y < h))) then
            let position = x + (w * y)
            array1.[position]
        else
            array1.[0]

    // date delle coordinate x e y ritorna l'elemento del labirinto coorispondente 
    member private this.Gcoord_to_cell (g_x, g_y) =
        match (g_x, g_y) with
        |(x, y) when (x % 2 = 0) && (y % 2 <> 0) -> (this.cell(g_x / 2, g_y / 2).wall_left)
        |(x, y) when (x % 2 = 0) && (y % 2 = 0) -> (this.cell(g_x / 2, g_y / 2).corner)
        |(x, y) when (x % 2 <> 0) && (y % 2 <> 0) -> (this.cell(g_x / 2, g_y / 2).room)
        |(x, y) when (x % 2 <> 0) && (y % 2 = 0) -> (this.cell(g_x / 2, g_y / 2).wall_up)
        |_ -> false

    // metodo per selezionare una cella randomica (utilizzato per scegliere la cella di entrata del labirinto)
    member this.rnd_room =
        let x = rnd_int 0 (w - 1)
        let y = rnd_int 0 (h - 1)

        (x * 2 + 1, y * 2 + 1)

    // data una direzione e le coordinate della cella in cui ti trovi controlla se è possibile muoversi nella determinata posizione
    member this.is_possible_path (dx, dy, x, y) =
        not (this.Gcoord_to_cell(x + dx, y + dy))

    // metodo che data una direzioe e le coordinate di una cella "costruisce" la cella corrispondente alla direzione
    member private this.build (d : direction, x : int, y : int) =
        match d with
        |Up -> 
            ignore <| this.cell(x, y).remove_wall(Up)
            ignore <| this.cell(x, (y - 1)).mark_created
            true
        |Right -> 
            ignore <| this.cell((x + 1), y).remove_wall(Left)
            ignore <| this.cell((x + 1), y).mark_created
            true
        |Down -> 
            ignore <| this.cell(x, (y + 1)).remove_wall(Up)
            ignore <| this.cell(x, (y + 1)).mark_created
            true
        |Left -> 
            ignore <| this.cell(x, y).remove_wall(Left)
            ignore <| this.cell((x - 1), y).mark_created
            true
        |Stay -> 
            false

    // metodo di generazione del labirinto
    static member generate (w, h) =
        let maze_f = new maze (w / 2, h / 2)
        
        // funzione converte un intero positivo nella direzione corrispondente
        let rec int_to_direction (dir : int) =
            match dir with
            |1 -> Up
            |2 -> Right
            |3 -> Down
            |4 -> Left
            |_ -> int_to_direction (dir - 4)

        // funzione di generazione dei percorsi del labirinto bastato sul Recursive backtracker
        let rec backtracker (m : maze) (x : int) (y : int) = 
            let n = rnd_int 1 4
            let dir = int_to_direction (n)

            // funzione che data una direzione controlla se la cella corrispondente alla direzine è stata generata, se si prova con la direzine successiva fino a controllare tutte le celle adiacenti.
            // se tutte le celle adiacenti sono state generate riforna false e le coordinate della cella attuale
            // se invece una cella adiacente non è ancora stata generata, allora genera la cella e ritorna il risulatato della generazione + le coordinate della cella generata
            let rec moove (dir : direction) (i : int) =
                match dir with
                |Up when (m.cell(x, (y - 1)).room) -> (m.build(dir, x, y), x, (y - 1))
                |Right when (m.cell((x + 1), y).room) -> (m.build(dir, x, y), (x + 1), y)
                |Down when (m.cell(x, (y + 1)).room) -> (m.build(dir, x, y), x, (y + 1))
                |Left when (m.cell((x - 1), y).room) -> (m.build(dir, x, y), (x - 1), y)
                |_ -> 
                    if (i = 5) then (false, x, y)
                    else moove (int_to_direction (n + i)) (i + 1)

            let (result, n_x, n_y) = moove (dir) 1

            if (result) then
                let (_x, _y) = backtracker m n_x n_y
                if ((_x = 0) && (_y = 0)) then
                    (0, 0)
                else
                    backtracker m x y
            else
                (x, y)

        ignore <| maze_f.cell(0, 0).mark_created
        ignore <| backtracker maze_f 0 0

        maze_f

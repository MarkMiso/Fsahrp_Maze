module LabProg2019.Ai

open Maze

type Ai () = 

    static member maze_solver1 (m : maze, p_x, p_y, e_x, e_y, ?with_steps) =
        let w_s = defaultArg with_steps false
        let mutable  path_steps = [(p_x, p_y)]

        let rec int_to_direction (dir : int) =
            match dir with
            |1 -> 0, -1
            |2 -> 1, 0
            |3 -> 0, 1
            |4 -> -1, 0
            |_ -> int_to_direction (dir - 4)

        // funzione di generazione dei percorsi del labirinto bastato sul Recursive backtracker
        
        let rec backtracker (path) = 
            let (x,y) = List.head path
            let n = rnd_int 1 4
            let dir = int_to_direction (n)

            let rec moove (d: int * int) (i : int) =
                let (dx, dy) = d

                if ((m.is_possible_path(dx, dy, x, y)) && (not(List.contains (dx + x, dy + y) path_steps))) then
                    path_steps <- path_steps@[(dx + x, dy + y)]
                    (true, x + dx, y + dy)
                else
                    if (i = 5) then (false, x, y)
                    else moove (int_to_direction (n + i)) (i + 1)

            let (result, n_x, n_y) = moove (dir) 1
            
            if (result) then
                backtracker ((n_x, n_y)::path)
            else
                
                if ((x = e_x) && (y = e_y)) then
                    path
                else
                    backtracker (List.tail path)

        let l = List.rev (backtracker [p_x, p_y])

        if w_s then path_steps
        else l
        

    static member maze_solver2 (m: maze, p_x, p_y, e_x, e_y, ?with_steps)=
        let w_s = defaultArg with_steps false
        let mutable path = []
        let mutable path_steps = []

        let rec backtracker (l : (int * int) list) =
            let (x, y) = List.head l
            
            if ((e_x = x) && (e_y = y)) then
                path <- l
                true
            else
                if ((m.is_possible_path (1, 0, x, y)) && (not(List.contains (x + 1, y) l)) && (backtracker ((x + 1, y)::l))) then
                    true
                elif ((m.is_possible_path (0, 1, x, y)) && (not(List.contains (x, y + 1) l)) && (backtracker ((x, y + 1)::l))) then
                    true
                elif ((m.is_possible_path (-1, 0, x, y)) && (not(List.contains (x - 1, y) l)) && (backtracker ((x - 1, y)::l))) then 
                    true
                elif ((m.is_possible_path (0, -1, x, y)) && (not(List.contains (x, y - 1) l)) && (backtracker ((x, y - 1)::l))) then
                    true
                else
                    path_steps <- (List.tail l)@path_steps
                    false

        ignore <| backtracker [(p_x, p_y)]

        if w_s then path_steps
        else List.rev path
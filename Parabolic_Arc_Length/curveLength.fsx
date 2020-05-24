open System

let getXSeq n a b =
    let h = (b - a) / n

    seq {
        yield! a 
            |> Seq.unfold 
                (fun state -> if state < b then Some(state, state + h) else None);
        yield b
    }

let curveLength n a b f =
    let dist ((x1, y1), (x2, y2)) =
        let xdist = x2 - x1
        let ydist = y2 - y1
        (xdist * xdist + ydist * ydist) |> sqrt
         
    getXSeq n a b
    |> Seq.map (fun x -> (x, f x)) 
    |> Seq.pairwise
    |> Seq.sumBy dist

//let set = getXSeq 2.0 0.0 1.0 |> List.ofSeq
// let set = getXSeq 40.0 0.0 1.0 |> List.ofSeq
// set;;

let n = 40.0
curveLength n 0.0 1.0 (fun x -> x * x)



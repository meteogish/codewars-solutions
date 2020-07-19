let queueTime customers n =
    let folder (acc: int list) item =
        ((acc.Head + item) :: acc.Tail) |> List.sort
        
    let initialTillQueue = ((Array.zeroCreate n) |> List.ofArray)

    List.fold folder initialTillQueue customers
    |> List.last

queueTime [10;2;3;3] 2
|> printfn "%i"

queueTime [2;3;10] 2
|> printfn "%i"

queueTime [5;3;4] 1
|> printfn "%i"
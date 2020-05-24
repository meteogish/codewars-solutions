open System

let sumFracts (xs: (int*int) list) =
    let rec gcd x y =
        if x = y then x
        else if x > y then gcd  (x - y) y
        else gcd x (y - x)
    
    let lcm x y =
        (abs(x) / gcd x y) * abs(y)

    let lcmOfSeq numbers =
        numbers |> Seq.reduce lcm
    
    let rec reduceToSimpleGcd x y =
        let divisor = gcd x y

        if divisor = 1 then
            (x, y)
        else 
            reduceToSimpleGcd (x / divisor) (y / divisor)

    if (xs |> List.isEmpty) then 
        None 
    else 
        let denFirst = 
            xs
            |> Seq.map snd
            |> lcmOfSeq 

        let numFirst = 
            xs
            |> Seq.sumBy (fun xf -> (fst xf) * denFirst / (snd xf))
        
        if ((gcd numFirst denFirst) = denFirst) then
            Some (sprintf "%i" (numFirst / denFirst))
        else
            let (numerator, denumerator) = reduceToSimpleGcd numFirst denFirst
            Some (sprintf "%i %i" numerator denumerator)
    
    
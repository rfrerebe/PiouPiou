namespace PiouPiouImplementation

module internal Utils =

    open System.Reflection
    open Microsoft.FSharp.Reflection

    let rnd =
        (new System.Random(int System.DateTime.Now.Ticks))

    /// Shuffle function
    /// Takes an array in input and return same array shuffle in place
    let shuffle (array : array<'a>) =
        let Swap i j =
            let item = array.[i]
            array.[i] <- array.[j]
            array.[j] <- item
        let ln = array.Length
        [0..(ln - 2)]
        |> Seq.iter (fun i -> Swap i ((rnd.Next(i, ln) )))
        array

 
    let unionCases<'a>() =
        let union = FSharpType.GetUnionCases(typeof<'a>, BindingFlags.NonPublic ||| BindingFlags.Public)
        union
        |> Array.map (fun x -> FSharpValue.MakeUnion(x, [||], BindingFlags.NonPublic ||| BindingFlags.Public ) :?> 'a) 
        

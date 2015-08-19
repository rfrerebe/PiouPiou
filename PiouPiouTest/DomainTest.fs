module DomainTest 

open NUnit.Framework
open FsUnit
open PiouPiouDomain
open System.Reflection
open Microsoft.FSharp.Reflection


let unionCases<'a>() =
    let union = FSharpType.GetUnionCases(typeof<'a>, BindingFlags.NonPublic ||| BindingFlags.Public)
    union
    |> Array.map (fun x -> FSharpValue.MakeUnion(x, [||], BindingFlags.NonPublic ||| BindingFlags.Public ) :?> 'a) 
        
//    type StatutDeLOeuf =
//        | PasDOeuf
//        | Pondu
//        | Eclos
[<Test>]
let ``Le statut de l'oeuf doit avoir 3 etats`` ()=
    
    let statut = unionCases<Oeuf>()

    statut
    |> should haveLength 2

    statut
    |> Array.contains Pondu
    |> should be True

    statut
    |> Array.contains Eclos
    |> should be True

//    type TypeDeCarte =
//       | Renard
//       | Poule
//       | Coq
//       | Nid
[<Test>]
let ``Il y a 4 types de cartes : Renard, Poule, Coq et Nid`` ()=
    let typeDeCarte = unionCases<Carte>()

    typeDeCarte
    |> should haveLength 4

    typeDeCarte
    |> Array.contains Renard
    |> should be True

    typeDeCarte
    |> Array.contains Poule
    |> should be True

    typeDeCarte
    |> Array.contains Coq
    |> should be True

    typeDeCarte
    |> Array.contains Nid
    |> should be True

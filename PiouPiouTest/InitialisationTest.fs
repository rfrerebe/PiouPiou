module InitialisationTest

open NUnit.Framework
open FsUnit
open PiouPiouDomain
open PiouPiouImplementation.Implementation
open PiouPiouImplementation.Initialisation


[<Test>]
let ``Verifie qu'apres l'initialisation les joueurs sont correctement définis`` () =
    let init = initialisation ()
    let joueurs = init.joueurs
    joueurs
    |> List.fold (fun acc joueur-> acc && joueur.Verification) true
    |> should be True

[<Test>]
let ``Verifie qu'au moins un joueur a "moi" = true`` () =
    let init = initialisation ()
    let joueurs = init.joueurs
    joueurs
    |> List.filter (fun joueur -> joueur.moi)
    |> should haveLength 1

[<Test>]
let ``Verifie la taille de la pioche`` ()=
    let init = initialisation ()
    let joueurs = init.joueurs
    let pioche = init.cartes.pioche
    let tailleDuDebut = 
        nombreDeRenard +
        nombreDeCoq +
        nombreDePoule +
        nombreDeNid
    let nombreDeCartesDistribuées =
        4 * joueurs.Length
    pioche
    |> should haveLength (tailleDuDebut - nombreDeCartesDistribuées)

[<Test>]
let ``Verifie qu'il y a le bon nombre de Poule dans le jeu`` () =
    let init = initialisation ()
    let joueurs = init.joueurs
    let pioche = init.cartes.pioche
    let nombreChezLesJoueurs =
        joueurs
        |> List.fold (fun acc joueur -> acc + joueur.poule) 0
    let nombreDansLaPioche =
        pioche
        |>  List.fold (fun acc carte -> if (Poule.Equals(carte)) then acc + 1 else acc) 0
        
    nombreChezLesJoueurs + nombreDansLaPioche
    |> should equal nombreDePoule

[<Test>]
let ``Verifie qu'il y a le bon nombre de Coq dans le jeu`` () =
    let init = initialisation ()
    let joueurs = init.joueurs
    let pioche = init.cartes.pioche
    let nombreChezLesJoueurs =
        joueurs
        |> List.fold (fun acc joueur -> acc + joueur.coq) 0
    let nombreDansLaPioche =
        pioche
        |>  List.fold (fun acc carte -> if (Coq.Equals(carte)) then acc + 1 else acc) 0
    
    nombreChezLesJoueurs + nombreDansLaPioche
    |> should equal nombreDeCoq

[<Test>]
let ``Verifie qu'il y a le bon nombre de Renard dans le jeu`` () =
    let init = initialisation ()
    let joueurs = init.joueurs
    let pioche = init.cartes.pioche
    let nombreChezLesJoueurs =
        joueurs
        |> List.fold (fun acc joueur -> acc + joueur.renard) 0
    let nombreDansLaPioche =
        pioche
        |>  List.fold (fun acc carte -> if (Renard.Equals(carte)) then acc + 1 else acc) 0
    
    nombreChezLesJoueurs + nombreDansLaPioche
    |> should equal nombreDeRenard

[<Test>]
let ``Verifie qu'il y a le bon nombre de Nid dans le jeu`` () =
    let init = initialisation ()
    let joueurs = init.joueurs
    let pioche = init.cartes.pioche
    let nombreChezLesJoueurs =
        joueurs
        |> List.fold (fun acc joueur -> acc + joueur.nid) 0
    let nombreDansLaPioche =
        pioche
        |>  List.fold (fun acc carte -> if (Nid.Equals(carte)) then acc + 1 else acc) 0
    
    nombreChezLesJoueurs + nombreDansLaPioche
    |> should equal nombreDeNid

[<Test>]
let ``Verifie les actions possibles apres l'initialisation`` ()=
    let init = initialisation ()
    let actions = actionPossible init
    actions
    |> List.length
    |> should be (greaterThan 2)
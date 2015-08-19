namespace PiouPiouImplementation

module internal Initialisation =
    open PiouPiouDomain

    open Utils
    open InternalDomain
    open InternalLibrairie

    let nombreDeRenard = 4
    let nombreDePoule = 10
    let nombreDeCoq = 10
    let nombreDeNid = 7

    let cartes () =
        let pioche =
            Array.concat 
                [|(Array.create nombreDeRenard Renard);
                (Array.create nombreDePoule Poule);
                (Array.create nombreDeCoq Coq);
                (Array.create nombreDeNid Nid)|]
            |> shuffle
            |> Array.toList
        { pioche = pioche;
            defausse = []
            }

    let noms = 
        let noms' ()= 
            [|"Alfred";
                "Suzanne";
                "Joseph";
                "Jeanne"
                |]              
            // shuffle to randomize who will start
            |> shuffle
            |> Array.toList
        noms' ()

    let creeJoueur ne np nom=
        let moi  =
            function
            | NomDeJoueur "Alfred" -> true
            | _ -> false
        { renard = 0;
            poule = 0;
            coq = 0;
            nid = 0;
            pondu = np;
            eclos = ne;
            nom = nom;
            moi = moi nom;
            }

    let joueurs () =       
        let joueurs =
            noms
            |> List.map (fun nom -> NomDeJoueur nom)
            |> List.map (creeJoueur 0 0)
        joueurs

    let etatDuJeu () =
        { joueurs = joueurs ();
            cartes = cartes () ;
            simulation = false;
            }

    let pioche edj =
        let joueur, cartes =
            (edj.joueurs.Head, edj.cartes)
            |> pioche
        let edj' = 
            {edj with 
                joueurs = edj.joueurs.Tail @ [joueur]; 
                cartes = cartes}
        edj'

    let rec initialisation' i edj =
        match i with
        | 0 -> edj
        | _ ->
            let edj' =
                edj
                |> pioche
                |> initialisation' (i - 1) 
            edj'
        
    let initialisation () =
        let edj = etatDuJeu ()
        initialisation' (edj.joueurs.Length * 4) edj
                
            






namespace PiouPiouImplementation

module internal InternalLibrairie =
    open PiouPiouDomain
    open InternalDomain
    open Utils

    let appliqueVol resultat joueur =
        let execute =
            function 
            | PerdUnOeuf -> 
                {joueur with
                    pondu = joueur.pondu - 1;
                    }
            | DefendUnVolDOeuf ->
                {joueur with
                    coq = joueur.coq - 2;
                    }
        execute resultat

    let applique resultat joueur=
        let execute =
            function 
            | PondUnOeuf -> 
                {joueur with 
                    coq = joueur.coq - 1;
                    poule = joueur.poule - 1;
                    nid = joueur.nid - 1;
                    pondu = joueur.pondu + 1;
                    }
            | FaitEcloreUnOeuf ->
                {joueur with
                    poule = joueur.poule - 2;
                    pondu = joueur.pondu - 1;
                    eclos = joueur.eclos + 1;
                    }
            | VoleUnOeuf -> 
                {joueur with
                    renard = joueur.renard - 1;
                    pondu = joueur.pondu + 1;
                    }
            | RateUnVolDOeuf ->
                {joueur with
                    renard = joueur.renard - 1;
                    }
            | Defausse carte ->
                match carte with
                | Coq -> 
                    {joueur with
                        coq = joueur.coq - 1;
                        }
                | Poule ->
                    {joueur with
                        poule = joueur.poule - 1;
                        }
                | Nid ->
                    {joueur with
                        nid = joueur.nid - 1;
                        }
                | renard ->
                    {joueur with
                        renard = joueur.renard - 1;
                        }
        execute resultat

    // utilisé dans initialisation
    let rec pioche (j,c) =
        match c.pioche with
        | [] ->
            pioche (j, {pioche = c.defausse |> List.rev ; defausse =[] })
        | x::xs ->
            match x with
            | Coq -> {j with coq = j.coq + 1} , { c with pioche = xs}
            | Poule -> {j with poule = j.poule + 1}, { c with pioche = xs}
            | Nid -> {j with nid = j.nid + 1}, { c with pioche = xs}
            | Renard -> {j with renard = j.renard + 1}, { c with pioche = xs}

    let piochesVol resultat jc =
        match resultat with
        // deux cartes
        | DefendUnVolDOeuf ->
            jc
            |> pioche
            |> pioche
        // pas de cartes
        | PerdUnOeuf ->
            jc

    let pioches resultat jc =
        match resultat with
        // trois cartes
        | PondUnOeuf ->
            jc
            |> pioche
            |> pioche
            |> pioche
        // deux cartes
        | FaitEcloreUnOeuf ->
            jc
            |> pioche
            |> pioche
        // une carte
        | VoleUnOeuf 
        | RateUnVolDOeuf 
        | Defausse _ ->
            jc
            |> pioche

    let defausseListe cartes list =
        {cartes with defausse = list @ cartes.defausse}

    let defausseCarte cartes carte =
        {cartes with defausse = carte::cartes.defausse}

    let defausseVol resultat cartes =
        match resultat with
        | PerdUnOeuf -> 
            cartes
        | DefendUnVolDOeuf -> 
            [Coq; Coq]
            |> defausseListe cartes

    /// Impacte sur les cartes d'un action + roll du tas
    /// retourne les cartes à ajouter chez le joueur
    let defausse resultat cartes =
        match resultat with
        | PondUnOeuf ->
            [|Coq; Poule; Nid|]
            |> shuffle
            |> Array.toList
            |> defausseListe cartes
        | FaitEcloreUnOeuf -> 
            [Poule; Poule]
            |> defausseListe cartes
        | VoleUnOeuf -> 
            Renard
            |> defausseCarte cartes
        | RateUnVolDOeuf -> 
            Renard
            |> defausseCarte cartes
        | Defausse c -> 
            c
            |> defausseCarte cartes

    let appliqueResultat resultat (joueur, cartes) =
        // on applique le resultat de l'action au joueur et aux cartes
        let joueur' = 
            joueur 
            |> applique resultat
        let cartes' =
            cartes 
            |> defausse resultat
        let jc =
            (joueur', cartes')
            |> pioches resultat
        jc

    let appliqueResultatVol resultat (joueur, cartes) =
        // applique le vol chez le joeur
        let joueur' =
            joueur 
            |> appliqueVol resultat
        // defausse des cartes si besoin
        let cartes' =
            cartes 
            |> defausseVol resultat
        // pioche si besoin
        let jc =
            (joueur', cartes')
            |> piochesVol resultat
        // retourne la structure
        jc
            

    let leJeuEstIlGagnéPar joueur = 
        joueur.eclos = 3

//    type Action =
//        | VaPondreUnOeuf
//        | VaFaireEcloreUnOeuf
//        | TenteUnVolDOeuf of NomDeJoueur
//        | VaDefausser of Carte
    let verifie joueur = 
            function
            | VaPondreUnOeuf ->
                joueur.coq > 0 &&  
                joueur.poule > 0 && 
                joueur.nid > 0 &&
                joueur.Verification
            | VaFaireEcloreUnOeuf ->
                joueur.Verification &&
                joueur.poule > 1 &&
                joueur.pondu > 0
            | TenteUnVolDOeuf _ ->
                joueur.Verification &&
                joueur.renard > 0
            | VaDefausser carte ->
                match carte with
                | Coq -> 
                    joueur.Verification &&
                    joueur.coq > 0
                | Poule ->
                    joueur.Verification &&
                    joueur.poule > 0
                | Nid -> 
                    joueur.Verification &&
                    joueur.nid > 0
                | Renard ->
                    joueur.Verification &&
                    joueur.renard > 0

    let cartesJoueur joueur =
        let renard = [for i in [1..joueur.renard] do yield Renard]
        let coq = [for i in [1..joueur.coq] do yield Coq]
        let poule = [for i in [1..joueur.poule] do yield Poule]
        let nid = [for i in [1..joueur.nid] do yield Nid]
        List.concat [renard; coq; poule; nid]
namespace PiouPiouImplementation

module internal Resultat =
    open InitialisationAi
    open Implementation
    open PiouPiouDomain
    open Initialisation
    open InternalDomain


    let creeResultat f edj pas  = 
        let idp = edj |> informationDuPlateau 
        let c = edj |> cartesAMontrer
        MonTour (idp, pas, c)

    let creeResultatAi f edj pas  = 
        let idp = edj |> informationDuPlateau
        let c = edj |> cartesAMontrer
        let joueur = edj.joueurs.Head.nom
        TourDesAutres (joueur, idp, pas, c)

    let faitLesActionsSuivantes f edj action =
        let edj' = fun () -> copieAleatoire edj
        let commande () = f edj action
        match edj.joueurs.Head.moi || edj.simulation with
        | true -> 
            {action = action; 
                commande = commande;
                commandeSimulation = None}
        | false -> 
            let commandeSimulation () = f (edj' ()) action
            {action = action; 
                commande = commande;
                commandeSimulation = Some(commandeSimulation)}

    let donneResultatEtActionsSuivantes f edj actions =
        match edj.joueurs.Head.moi with
        | true ->
            let resultat = 
                actions
                |> List.map (faitLesActionsSuivantes f edj)
                |> creeResultat f edj
            resultat 
        | false ->
            let resultatAi = 
                actions
                |> List.map (faitLesActionsSuivantes f edj)
                |> creeResultatAi f edj
            resultatAi


    let execute edj f : Resultat =
        let mdj = 
            edj
            |> actionPossible
            |> donneResultatEtActionsSuivantes f edj
        mdj

    let rec mouvementDuJoueur etatDeJeu action =
        let edj = appliqueLAction etatDeJeu action
        if edj |> leJeuEstIlGagnéPar then 
            let idp = informationDuPlateau edj
            let joueur = edj.joueurs.Head
            JeuGagné (idp, joueur.nom)
        // calcul pour le prochain joueur
        else
            let edj' = edj |> tourSuivant
            execute edj' mouvementDuJoueur 

    let nouveauJeu () =
        let edj = initialisation ()
        execute edj mouvementDuJoueur




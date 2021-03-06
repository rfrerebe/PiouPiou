﻿namespace GameWithAi

module PiouPiouConsoleUI =
    
    open System
    open IntegrationAi
    open PiouPiouDomain
    open MCTS
        
    /// Suit l'etat de l'UI
    type ActionDuJoueur<'a> =
        | ContinueDeJouer of 'a
        | QuitteLeJeu

    /// Afficge chaque action dansèla console
    let afficheLesProchainsActions pas = 
        pas 
        |> List.iteri (fun i pa -> printfn "%i) %A" i pa.action)

    /// Get the move corresponding to the 
    /// index selected by the user
    let getCommande index pas = 
        if index < List.length pas then
            let pa = List.item index pas
            Some pa.commande 
        else
            None

    /// Given that the user has not quit, attempt to parse
    /// the input text into a index and then find the move
    /// corresponding to that index
    let processMoveIndex inputStr availableMoves processInputAgain = 
        match Int32.TryParse inputStr with
        // TryParse will output a tuple (parsed?,int)
        | true,inputIndex ->
            // parsed ok, now try to find the corresponding move
            match getCommande inputIndex availableMoves with
            | Some capability -> 
                // corresponding move found, so make a move
                let moveResult = capability()  
                ContinueDeJouer moveResult // return it
            | None ->
                // no corresponding move found
                printfn "...Pas de mouvement pour la valeur %i. Essaye encore" inputIndex 
                // try again
                processInputAgain()
        | false, _ -> 
            // int was not parsed
            printfn "...Merci de rentrer un chiffre pour choisir votre action."             
            // try again
            processInputAgain()

    /// Ask the user for input. Process the string entered as 
    /// a move index or a "quit" command
    let rec processInput availableCapabilities = 

        // helper that calls this function again with exactly
        // the same parameters
        let processInputAgain() = 
            processInput availableCapabilities 

        printfn "Entrer un le chiffre correspondant à l'action choisie ou q pour quitter:" 
        let inputStr = Console.ReadLine()
        if inputStr = "q" then
            QuitteLeJeu
        else
            processMoveIndex inputStr availableCapabilities processInputAgain


    let afficheCartes cartes =
        let carteToStr carte =
            match carte with
            | Renard -> "Renard"
            | Coq -> "Coq"
            | Poule -> "Poule"
            | Nid -> "Nid"
        cartes
        |> List.map carteToStr
        |> List.reduce (fun s1 s2 -> s1 + "|" + s2) 
        |> printfn "Cartes : %s"
            
    let afficheStatuts idp = 
        let oeufToStr oeuf = 
            match oeuf with
            | Pondu -> "Pondu"            
            | Eclos -> "Eclos"

        let afficheOeufs oeufs  = 
            if oeufs |>  List.isEmpty then
                printfn "Pas d'oeufs"
            else
                oeufs
                |> List.map oeufToStr
                |> List.reduce (fun s1 s2 -> s1 + "|" + s2) 
                |> printfn "Oeufs : %s"
    
        let afficheNom (nom : NomDeJoueur) =
            let (NomDeJoueur nomStr) = nom
            nomStr
            |> printfn "Joueur %s"

        let afficheStatutPublic sp =
            printfn ""
            sp.joueur
            |> afficheNom
            sp.oeufs
            |> afficheOeufs

        idp
        |> List.rev // pour avoir le joueur courant en dernier
        |> List.iter afficheStatutPublic


    let compute nom idp (pas :ProchaineAction list) c =
        let getMove name =
            let result =
                pas
                |> List.tryFind (fun pa -> name.Equals(ToString pa.action))
            match result with
            | None -> 
                let actionList = 
                    pas
                    |> List.map (fun pa -> ToString pa.action)
                invalidOp "Cherche a executer l'action {0}, absente dan la liste d'action {1}" name actionList
            | Some (pa) -> pa

        let r = TourDesAutres(nom, idp, pas, c)
        let print = 
            new Action<string>(fun arg -> printfn "%s" arg)
        match nom with
        | NomDeJoueur "Suzanne" ->
            let gs  = GameState(r)
            let move  = UCT.ComputeUCT(gs,9503, false, print, 1E-16F)
            let pa  = getMove move.Name
            pa
        | NomDeJoueur "Joseph" ->
            let gs  = GameState(r)
            let move  = UCT.ComputeUCT(gs,9503, false, print,1E-12F)
            let pa  = getMove move.Name
            pa
        | NomDeJoueur "Jeanne" -> 
            let gs  = GameState(r)
            let move  = UCT.ComputeUCT(gs,9503, false, print, 1E-6F)
            let pa  = getMove move.Name
            pa
        | _ -> invalidOp "nom de joueur invalid"
// Dummy AI, pick always first choice (it's often the best ...)         
//        let action = pas.Head.action
//        let capability = pas.Head.commande ()
//        action, ContinueDeJouer (capability)

    let joueurSuivant ()=
        printfn "Appuyer sur une touche pour passer au joueur suivant"
        Console.ReadLine() |> ignore

    /// After each game is finished,
    /// ask whether to play again.
    let rec demandeDeRejouer api  = 
        printfn "Est ce que vous voulez jouer de nouveau (o/n)?"             
        match Console.ReadLine() with
        | "o" -> 
            ContinueDeJouer (api.nouvellePartie())
        | "n" -> 
            QuitteLeJeu
        | _ -> demandeDeRejouer api 

    /// The main game loop, repeated
    /// for each user input
    let rec gameLoop api userAction = 
        printfn "\n------------------------------\n"  // a separator between moves
        
        match userAction with
        | QuitteLeJeu -> 
            printfn "Quitte le jeu."             
        
        | ContinueDeJouer moveResult -> 
            // handle each case of the result
            match moveResult with
            | JeuGagné (idp,nom) -> 
                idp |> afficheStatuts
                let (NomDeJoueur nomStr) = nom
                printfn "Jeu GAGNE par %A" nomStr            
                printfn ""             
                let nextUserAction = demandeDeRejouer api 
                gameLoop api nextUserAction
            | MonTour (idp, pas, cartes) -> 
                idp |> afficheStatuts
                cartes |> afficheCartes
                pas |> afficheLesProchainsActions
                let result = processInput pas
                gameLoop api result
            | TourDesAutres (nom, idp, pas, c) ->                
                idp |> afficheStatuts
                c |> afficheCartes
                pas |> afficheLesProchainsActions
                let prochaineAction = compute nom idp pas c 
                let (NomDeJoueur nom) = idp.Head.joueur
                printfn "%A joue %A" nom prochaineAction.action
                joueurSuivant ()
                let result = ContinueDeJouer (prochaineAction.commande())
                gameLoop api result
                

    /// start the game with the given API
    let startGame api =
        let userAction = ContinueDeJouer (api.nouvellePartie())
        gameLoop api userAction 
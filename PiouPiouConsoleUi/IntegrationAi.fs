namespace GameWithAi

module IntegrationAi =

    open MCTS.Interfaces
    open PiouPiouDomain
    open System

    let mkEnum<'T> (ie : System.Collections.IEnumerable) = 
        (Seq.cast<'T> ie)

    let random =
        new Random( DateTime.Today.Millisecond )

    let ToString =
        function
        | VaPondreUnOeuf -> "Pond un oeuf"
        | VaFaireEcloreUnOeuf -> "Fait eclore un oeuf"
        | TenteUnVolDOeuf ((NomDeJoueur nom)) -> "Tente de voler un oeuf à " + nom
        | VaDefausser (carte) -> 
            match carte with
            | Nid -> "Defausse un Nid"
            | Renard -> "Defausse un Renard"
            | Poule -> "Defausse une Poule"
            | Coq -> "Defausse un Coq"

    type Player (nom : NomDeJoueur) =
        interface IPlayer with 
            member this.Name =
                let (NomDeJoueur nom') = nom
                nom'


    type Move (pa :ProchaineAction) =
        interface IMove with
            member this.DoMove() =
                match pa.commandeSimulation with
                | None -> GameState(pa.commande ()) :> IGameState
                | Some (commande) -> GameState(commande ()) :>IGameState
            member this.Name =
                ToString pa.action



    and GameState (r : Resultat ) =
        let nom, idp, pas, c =
            match r with
            | MonTour  (idp, pas, c) -> (NomDeJoueur "Alfred"), idp, pas, c //invalidOp "l'ordinateur de ne doit pas jouer pour moi"
            | JeuGagné _ -> invalidOp "l'ordinateur ne peut pas gercher une solution quand le jeu est gagné"
            | TourDesAutres (nom, idp, pas, c) -> nom, idp, pas, c 

        
        interface IGameState with
            member this.GetMoves() =
                let moves =
                    pas
                    |> List.map (fun pa -> Move(pa))
                mkEnum<IMove> moves

            member this.CurrentPlayer() =
                Player(nom) :> IPlayer

            member this.PlayRandomlyUntilTheEnd( player ) =
                let rec play =
                    let play' pas =
                        let r = 
                            pas
                            |> List.length
                            |> random.Next 
                        let pa = 
                            pas
                            |> List.item r
                        pa.commande ()
                        |> play
                    function
                    | MonTour (_ ,pas, _) -> play' pas 
                    | TourDesAutres (_,_,pas,_) -> play' pas
                    | JeuGagné (_ , (NomDeJoueur nom)) -> 
                        if  nom = player.Name then
                            MCTS.Enum.EGameFinalStatus.GameWon
                        else
                            MCTS.Enum.EGameFinalStatus.GameLost
                play r



                        


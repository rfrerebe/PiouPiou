namespace PiouPiouImplementation

module internal Implementation =
    open PiouPiouDomain
    open InternalDomain
    open Utils
    open InternalLibrairie

    /// Retourne le StatutPublic d'un joueur
    let statutPublic joueur =
        let pondu = [for i in [1..joueur.pondu] do yield Pondu]
        let eclos = [for i in [1..joueur.eclos] do yield Eclos]
        { joueur = joueur.nom ;
            oeufs = List.concat [pondu; eclos] ;
            }

    let resultatDuVol joueur =
        match joueur.coq > 1 with
        | true -> DefendUnVolDOeuf
        | false -> PerdUnOeuf

    /// Retourne l'InformationDuPlateu d'un EtatDeJeu
    let informationDuPlateau (etatDuJeu : EtatDuJeu) : InformationDuPlateau = 
        etatDuJeu.joueurs
        |> List.map statutPublic

    let cartesAMontrer edj =
        cartesJoueur edj.joueurs.Head

    /// Retourne la liste d'Action possible pour un EtatDeJeu
    let actionPossible etatDuJeu =

        // le joueur est le premier de liste
        let joueur = etatDuJeu.joueurs.Head

        // Cree un fonction avec le joueur implicite
        let verifieJoueur =
            verifie joueur

        // On restreint le champ des possibles
        let joueursPouvantEtreVolé =
            etatDuJeu.joueurs.Tail
            |> List.filter (fun j -> j.pondu > 0)
            |> List.map (fun j -> j.nom)

        // on construit le champ des possibles
        let cartes = unionCases<Carte>()
        let actionsPossibles = [
            yield VaPondreUnOeuf;
            yield VaFaireEcloreUnOeuf;
            for jpev in joueursPouvantEtreVolé do yield TenteUnVolDOeuf (jpev);
            for carte in cartes do yield VaDefausser carte
            ]

        // on recupere les actions possibles
        let result =
            actionsPossibles
            |> List.filter verifieJoueur
        result

    let actionSiVol (joueur, cartes) =
        let resultat = resultatDuVol joueur
        let jc = 
            (joueur, cartes) 
            |> appliqueResultatVol resultat
        jc, resultat

    let trouveJoueur edj (nom : NomDeJoueur) =
        let joueurs = edj.joueurs
        let joueur =
            joueurs
            |> List.find (fun j -> nom.Equals(j.nom))
        joueur

        
    let joueurCartes edj =
        (edj.joueurs.Head, edj.cartes)

    let edj edj' (joueur, cartes) =
        let joueurs = 
            edj'.joueurs
            |> List.map (fun j -> if (joueur.nom.Equals(j.nom)) then joueur else j)
        let edj'' = { edj' with joueurs = joueurs; cartes = cartes}
        edj''

    /// Applique l'Action a un EtatDeJeu.
    /// Retourne le nouvel EtatDeJeu
    let appliqueLAction etatDuJeu =
        function
        | VaPondreUnOeuf ->
            // applique l'action au premier joueur
            joueurCartes etatDuJeu
            // Pond un Oueuf
            |> appliqueResultat PondUnOeuf
            // retourne le nouvel EtatDeJeu
            |> edj etatDuJeu

        | VaFaireEcloreUnOeuf ->
            // applique l'action au premier joueur
            joueurCartes etatDuJeu
            // Fait eclore un oeuf
            |> appliqueResultat FaitEcloreUnOeuf
            // retourne le nouvel EtatDeJeu
            |> edj etatDuJeu
        | TenteUnVolDOeuf nomDeJoueur ->
            // Trouve le Joueur attaque par le vol
            let joueurAttaqué = 
                nomDeJoueur
                |> trouveJoueur etatDuJeu
            let jc, resultatVol= 
                // Applique le vol au joueur attaque et 
                // retourne le resultat de l'attaque
                (joueurAttaqué, etatDuJeu.cartes)
                |> actionSiVol
            // cree un EtatDeJeu apres le vol
            // integrant les modifictions sur le joueur volé
            let edjApresVol = edj etatDuJeu jc

            match resultatVol with
            | PerdUnOeuf -> 
                // applique l'action au premier joueur
                joueurCartes edjApresVol
                // La tentative de vol reussi
                |> appliqueResultat VoleUnOeuf
            // retourne le nouvel EtatDeJeu
                |> edj edjApresVol
            | DefendUnVolDOeuf -> 
                // applique l'action au premier joueur
                joueurCartes edjApresVol
                // La tentative de vol echoue
                |> appliqueResultat RateUnVolDOeuf
                // retourne le nouvel EtatDeJeu
                |> edj edjApresVol               
        | VaDefausser carte -> 
            // applique l'action au premier joueur
            joueurCartes etatDuJeu
            // Defausse uen carte
            |> appliqueResultat (Defausse carte)
            // retourne le nouvel EtatDeJeu
            |> edj etatDuJeu

    let leJeuEstIlGagnéPar edj =
        leJeuEstIlGagnéPar edj.joueurs.Head

    let tourSuivant edj =
        let joueurs =
            edj.joueurs.Tail @ [edj.joueurs.Head]
        {edj with joueurs = joueurs}

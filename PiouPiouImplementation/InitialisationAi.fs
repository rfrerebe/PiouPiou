namespace PiouPiouImplementation

module internal InitialisationAi =
    open InternalDomain
    open InternalLibrairie
    open Initialisation
    open Utils

    let enleveLesCartes joueur =
        creeJoueur joueur.eclos joueur.pondu joueur.nom

    let etatDuJeuVide edj : Joueur * EtatDuJeu =
        let joueur = edj.joueurs.Head
        let autresJoueurs = 
            edj.joueurs.Tail
        let cartesJoueur = 
            autresJoueurs
            |> List.collect cartesJoueur
        let pioche =
            edj.cartes.pioche @ cartesJoueur
            |> List.toArray
            |> shuffle
            |> Array.toList
        let cartes =
            {pioche = pioche;
                defausse = edj.cartes.defausse}
        let joueurs = 
            autresJoueurs
            |> List.map enleveLesCartes
        let edj' =
            { joueurs = joueurs;
                cartes = cartes;
                simulation = true;}
        joueur, edj'

    let copieAleatoire edj =
        let joueur, edjVide =
            etatDuJeuVide edj
        let edjComplet = initialisation' (edjVide.joueurs.Length * 4) edjVide
        let edj' =
            {edjComplet with joueurs = joueur::edjComplet.joueurs}
        edj'



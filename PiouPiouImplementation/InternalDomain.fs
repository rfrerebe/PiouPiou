namespace PiouPiouImplementation

module internal InternalDomain =

    open PiouPiouDomain
    
    type Joueur = {
        renard : int;
        poule : int ;
        coq : int;
        nid : int ;
        pondu : int;
        eclos : int ;
        nom : NomDeJoueur;
        moi : bool;
        }
    with
        member this.Verification =
            this.renard + this.poule + this.coq + this.nid = 4
            && this.renard >= 0
            && this.poule >= 0
            && this.coq >= 0
            && this.nid >= 0
            && this.pondu >= 0
            && this.eclos >= 0

    type Cartes = {
        pioche : Carte list;
        defausse : Carte list; 
        }

    type EtatDuJeu = {
        joueurs : Joueur list;
        cartes : Cartes;
        simulation : bool;
        }

    type ResultatDeVolDOeuf =
        | PerdUnOeuf 
        | DefendUnVolDOeuf

    
    type ResultatDAction =
        | PondUnOeuf
        | FaitEcloreUnOeuf
        | VoleUnOeuf
        | RateUnVolDOeuf
        | Defausse of Carte



           



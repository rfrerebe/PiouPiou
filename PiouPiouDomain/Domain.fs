module public PiouPiouDomain

// Jeu du Piou Piou
// 
// On peut jouer de 2 à 5 joueurs
// Chaque joueurs à 4 cartes en main.
// Tout le long du jeu, oè garde 4 cartes en main.
// Le but du jeu est d'avoir 3 oeufs eclos
// Il y a 4 types de cartes :
// - Coq
// - Poule
// - Nid
// - Renard
// Il y a 5 type d'actions possibles dans le jeu.
// On en choisit une seule par tour.
// A chaque tour, on defausse une ou plusieurs cartes et on pioche le meme nombre de cartes.
// Chaque joueur doit toujours avoir 4 cartes.
// Voici les actions possibles :
// - Pondre un oeuf, il faud un Coq, une Poule et un Nid dans sa main. 
//   On defausse ces 3 cartes et on en pioche des nouvelles.
//   On gagne un oeuf pondu.
// - Faire Eclore Un Oeuf, il faut deux Poules et un oeuf pondu.
//   On defausse ces deux cartes.
//   L'oeuf pondu devient un oeuf eclos.
// - Tenter de Voler un oeuf pondu, il faut un Renard et 
//   un adversaire doit avoir un oeuf Pondu. On choisit à quel
//   adversaire on tente de voler l'oeuf s'il y a plusieurs 
//   joueurs avec des oeufs pondus.
//   Le voleur defausse son Renard et pioche une carte.
//   Le joueur a qui on vole peut defendre son oeuf.
//   S'il a deux Coqs, alors le vol echoue.
//   Il defausse ses deux Coqs, pioche deux cartes et garde son oeuf.
//   Sinon le vol reussi. Il perd son oeuf pondu et le voleur
//   gagne l'oeuf pondu.
// - Defausser une carte. Le joueur choisit une carte à defausser 
//   dans son jeu, puis pioche une carte.
// Le but du jeu est d'etre le premier à avoir 3 oeufs eclos.

    type NomDeJoueur = NomDeJoueur of string

    type Carte =
        | Renard
        | Poule
        | Coq
        | Nid

    type Oeuf =
        | Pondu
        | Eclos

    type Action =
        | VaPondreUnOeuf
        | VaFaireEcloreUnOeuf
        | TenteUnVolDOeuf of NomDeJoueur
        | VaDefausser of Carte

    type StatutPublic = {
        joueur : NomDeJoueur 
        oeufs : Oeuf list 
        }

    type InformationDuPlateau =
        StatutPublic list

    type Commande = 
        unit -> Resultat

    and ProchaineAction = {
        action : Action
        commande : Commande
        commandeSimulation : Option<Commande>
        }

    and Resultat = 
        // C'est mon tour. Je vois l'information du plateau, mes cartes et ma liste d'action
        | MonTour of InformationDuPlateau * ProchaineAction list * Carte list
        // C'est le tour des autres. Je vois l'information du plateau, mes cartes et l'actioè du joueur
        | TourDesAutres of NomDeJoueur * InformationDuPlateau * ProchaineAction list * Carte list
        //| Tour of InformationDuPlateau * Carte list * ProchaineAction list
        // Le jeu est gagne par un Joueur
        | JeuGagné of InformationDuPlateau * NomDeJoueur 

    type PiouPiouApi  = {
        nouvellePartie : Commande;
        }


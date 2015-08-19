module EtatDuJeuTest

open NUnit.Framework
open FsUnit
open PiouPiouDomain
open PiouPiouImplementation.InternalDomain
open PiouPiouImplementation.Implementation

let nom = NomDeJoueur "Alfred"

[<Test>]
let ``Verifie le Statut Public pour un jouer sans oeufs`` ()=
    let joueur =         
        {renard = 1;
            coq = 1;
            poule = 1;
            nid = 1;
            pondu = 0;
            eclos = 0;
            nom = nom;
            moi = true;
            }
    let statutPublic = statutPublic joueur
    statutPublic.oeufs
    |> should haveLength 0

[<Test>]
let ``Verifie le Statut Public pour un jouer avec un oeuf pondu`` ()=
    let joueur =         
        {renard = 1;
            coq = 1;
            poule = 1;
            nid = 1;
            pondu = 1;
            eclos = 0;
            nom = nom;
            moi = true;
            }
    let statutPublic = statutPublic joueur
    statutPublic.oeufs
    |> should haveLength 1
    
    statutPublic.oeufs
    |> should contain Pondu

[<Test>]
let ``Verifie le Statut Public pour un jouer avec un oeuf eclos`` ()=
    let joueur =         
        {renard = 1;
            coq = 1;
            poule = 1;
            nid = 1;
            pondu = 0;
            eclos = 1;
            nom = nom;
            moi = true;
            }
    let statutPublic = statutPublic joueur
    statutPublic.oeufs
    |> should haveLength 1
    
    statutPublic.oeufs
    |> should contain Eclos

[<Test>]
let ``Verifie le Statut Public pour un joueur avec un oeuf eclos et un pondu`` ()=
    let joueur =         
        {renard = 1;
            coq = 1;
            poule = 1;
            nid = 1;
            pondu = 1;
            eclos = 1;
            nom = nom;
            moi = true;
            }
    let statutPublic = statutPublic joueur
    statutPublic.oeufs
    |> should haveLength 2
    
    statutPublic.oeufs
    |> should contain Pondu

    statutPublic.oeufs
    |> should contain Eclos
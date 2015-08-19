module InternalDomainTest

open NUnit.Framework
open FsUnit
open PiouPiouDomain
open PiouPiouImplementation.InternalDomain

let nom = NomDeJoueur "Alfred"

[<Test>]
let ``La somme des cartes ne peut pas etre superieur a 4`` ()=

    {renard = 1;
        coq =1;
        poule=1;
        nid=2;
        pondu = 0;
        eclos =0;
        nom = nom;
        moi = true;
        }.Verification
    |> should be False

let ``On ne peut pas avoir de nombre de cartes negatives : renard`` ()=

    {renard = -1;
        coq = 1;
        poule = 1;
        nid = 3;
        pondu = 0;
        eclos = 0;
        nom = nom;
        moi = true;
        }.Verification
    |> should be False

let ``On ne peut pas avoir de nombre de cartes negatives : coq`` ()=

    {renard = 1;
        coq = -1;
        poule = 1;
        nid = 3;
        pondu = 0;
        eclos = 0;
        nom = nom;
        moi = true;
        }.Verification
    |> should be False

let ``On ne peut pas avoir de nombre de cartes negatives : poule`` ()=

    {renard = 1;
        coq = 1;
        poule = -1;
        nid = 3;
        pondu = 0;
        eclos = 0;
        nom = nom;
        moi = true;
        }.Verification
    |> should be False


let ``On ne peut pas avoir de nombre de cartes negatives : nid`` ()=

    {renard = 1;
        coq = 1;
        poule = 3;
        nid = -1;
        pondu = 0;
        eclos = 0;
        nom = nom;
        moi = true;
        }.Verification
    |> should be False

let ``On ne peut pas avoir de nombre de cartes negatives : pondu`` ()=

    {renard = 1;
        coq = 1;
        poule = 2;
        nid = 0;
        pondu = -1;
        eclos = 0;
        nom = nom;
        moi = true;
        }.Verification
    |> should be False

let ``On ne peut pas avoir de nombre de cartes negatives : eclos`` ()=

    {renard = 1;
        coq = 1;
        poule = 2;
        nid = 0;
        pondu = 0;
        eclos = -1;
        nom = nom;
        moi = true;
        }.Verification
    |> should be False
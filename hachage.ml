type ('a, 'b) table_hachage = {hache : 'a -> int; donnees : ('a * 'b) list array; largeur : int };; (* Définition d'une table de hachage *)

let creer h w = (* Crée une table de hachage de fonction de hachage h, et de nombre d'éléments w *)
    let hach = {hache = h; largeur = w; donnees = Array.make w []} in
    hach;;

let recherche t k = (* Recherche s'il existe un élément dont la clef d'entrée correspond avec une clef du tableau de hache *)
    let rec aux = function
        | [] -> false
        | (k', _)::q -> k=k' || aux q
    in
    aux (t.donnees.(t.hache k));;
    
    
let element t k = (* Renvoie l'élément associé à la clef d'entrée s'il y en a un *)
    let rec aux = function
        | [] -> failwith "Pas d'élément"
        | (k', a)::q -> if k = k' then a else aux q
    in
    aux (t.donnees.(t.hache k));;
    

let ajout t k e = (* Ajoute un élément au tableau *)
    if not(recherche t k) then
        let au = t.hache k in
        t.donnees.(au) <- (k, e)::(t.donnees.(au));;

let suppr t k = (* Supprime un élément du tableau *)
    if recherche t k then
    let a = t.hache k in
        let rec aux = function
            | [] -> failwith "Impossible"
            | (k', e)::q -> if k = k' then q else (k', e) :: aux q
        in
    t.donnees.(a) <- aux t.donnees.(a);;


type ('a, 'b) table_dyn = {
                        hache :int -> 'a -> int;
                        mutable taille: int;
                        mutable donnees: ('a * 'b) list array;
                        mutable largeur: int} (* Création d'une table de hachage modifiable *)


let creer_dyn h = (* Crée une table de hachage dynamique *)
    let hach = {hache = h; taille = 0; donnees = Array.make 1 []; largeur = 1} in
    hach;;
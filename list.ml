(*algorithmes sur les listes Ocaml *)

let rec mem l e = match l with (* List.mem *)
    | [] -> false
    | f::q -> if e = f then true
              else mem q e;;


let rec iter f l = match l with (* List.iter *)
    | [] -> ()
    | e::q -> f e;iter f q;;

let rec filter f l = match l with (* Renvoie les éléments d'une liste qui vérifient les critères de la fonction f *)
    | [] -> []
    | e::q -> if f(e) = true then e:: filter (f) (q)
                             else filter f q;;


let rec map f l = match l with (* List.map *)
    | [] -> []
    | e::q -> f(e)::map f q;;

let f x = 2*x 



let rec range = function (* renvoie la liste des entiers de 1 à n *)
    | 0 -> [0]
    | a -> range (a-1) @ [a];;
    


let rec positif l = match l with (* Renvoie si tout les éléments d'une liste sont positifs *)
    | [] -> true
    | e::q -> if e > 0 then positif q
              else false


let rec pair l = match l with (* Renvoie si tout les éléments d'une liste sont pairs *)
    | [] -> false
    | e::q -> if e mod 2 = 0 then true
              else pair q


let rec add e ll = match ll with (* Rajoute un élément e au début de chaque liste de liste *)
    | [] -> []
    | f::q -> (e::f):: add e q 


let rec parties = function (* Renvoie tout les sous ensembles qui composent la liste *)
    | [] -> [ [] ]
    | e::q -> parties q @ add e (parties q)


let rec somme l = match l with (* Renvoie la somme des termes de la liste l *)
    | [] -> 0
    | e::q -> e + somme (q) 


let decomposition n l = (* Renvoie le nombre de manières d'écrire n comme somme d'éléments de l *)
    let rec parcours ll = match ll with
        | [] -> 0
        | e::q -> if somme (e) = n then (parcours q)+1
              else parcours q in
        parcours (parties (l))    


let decomposition1 n l = (* Même chose que précédemment sauf que cette fonction renvoie les combinaisons 						possibles pour écrire n comme somme d'éléments de l *)
    let rec parcours ll = match ll with
        | [] -> []
        | e::q -> if somme (e) = n then e::(parcours q)
              else parcours q in
        parcours (parties (l))
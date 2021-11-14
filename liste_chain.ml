type 'a case = { elem : 'a; mutable next : 'a liste1}
and 'a liste1 = Vide | C of 'a case;; (* Création d'une liste chainée *)


let rec to_list = function (* Transforme une liste chainée en liste *)
        | Vide -> []
        | C{elem = e; next =q} -> e::(to_list q);;
    

let rec to_list2 = function (* Autre implémentation *)
    | Vide -> []
    | C(c) -> (c.elem)::(to_list(c.next))


let rec has_cycle l vus = match l with (* Recherche si la liste reboucle sur elle-même *)
    | Vide -> false
    | C{elem =e; next =q} -> List.memq e vus || has_cycle q vus;
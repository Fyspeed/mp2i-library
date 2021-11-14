let somme t = (* Calcule la somme des entiers d'un tableau *)
    let s = ref 0 in 
    for i=0 to (Array.length t)-1 do
        s := (!s) + t.(i) 
    done;
   (!s) ;;
   

let maximum t = (* Renvoie le plus grand élément du tableau *)
    let m = ref 0 in
    for i=0 to (Array.length t)-1 do
        m := max !m t.(i)
    done;
    !m;;


let list_of_array t = (* Transforme un tableau en liste (sans conserver les indices des éléments *)
    let l = ref [] in
    for i=0 to (Array.length t)-1 do
      l :=   t.(i)::(!l) 
    done;
    !l;;


let max_local t = (* Calcule le maximum local d'un tableau *)
    let maximum = ref 0 in
    if t.(0)>=t.(1) then maximum := t.(0)
    else if t.(Array.length t -1)>=t.((Array.length t) -2) 
        then maximum := t.(Array.length t -1)
    else for i=1 to (Array.length t-2 ) do
            if t.(i)>= t.(i+1) && t.(i) >= t.(i-1)
            then maximum := t.(i)
            done;
            !maximum;;
            
let sommenulle t = (*Renvoie s'il existe une somme de suite d'éléments nulle *)
    let max_temp = ref false in
    for x = 0 to (Array.length t -1) do
        let s = ref 0 in
        for y = x to (Array.length t -1) do
            s := !s + t.(y);
            max_temp := !max_temp || !s = 0
        done;
    done;
    !max_temp
(* Expressions logiques linéaires temporelles *)
type ltl =
  | True | False
  | Atom     of string
  | Not      of ltl
  | And      of ltl * ltl
  | Or       of ltl * ltl
  | Next     of ltl
  | Finally  of ltl
  | Globally of ltl
  | Until    of ltl * ltl
  | Release  of ltl * ltl

(* Imprimer une représentation de chaîne de caractères d'une expression ltl *)
let rec to_string expression =
  let affichage_parenthese expression =
    match expression with
      | True 
      | False 
      | Atom(_) 
      | Not(_) 
      | Next(_) 
      | Finally(_) 
      | Globally(_)
	  -> to_string expression (* Pas de parenthèses nécessaires *)
      | _ -> "(" ^ (to_string expression) ^ ")"
  in
  match expression with
    | True        
    	-> "⊤"
    | False     
    	-> "⊥"
    | Atom(p)   
    	-> p (* "ρ" *)
    | Not(expression)
    	-> "¬" ^ (affichage_parenthese expression)
    | And(gauche, droite)  
    	-> (affichage_parenthese gauche) ^ " ⋀ " ^ (affichage_parenthese droite)
    | Or(gauche, droite)       
    	-> (affichage_parenthese gauche) ^ " ⋁ " ^ (affichage_parenthese droite)
    | Next(expression)      
    	-> "◯ " ^ (affichage_parenthese expression) (* X *)
    | Finally(expression)   
    	-> "◇ " ^ (affichage_parenthese expression) (* F *)
    | Globally(expression)  
    	-> "□ " ^ (affichage_parenthese expression) (* G *)
    | Until(gauche, droite)    
    	-> (affichage_parenthese gauche) ^ " 𝓤 " ^ (affichage_parenthese droite)
    | Release(gauche, droite)  
    	-> (affichage_parenthese gauche) ^ " 𝓡 " ^ (affichage_parenthese droite)

(* Déterminer la taille d'une expression conforme à l'ordre des sous-formules *)
let rec taille_de expression =
  match expression with
    | True | False | Atom(_) 
    	-> 1
    | Not(expression) | Next(expression) 
    | Finally(expression) | Globally(expression) 
    	-> 1 + taille_de expression
    | And(gauche, droite) | Or(gauche, droite) | Until(gauche, droite) | Release(gauche, droite) 
    	-> 1 + max (taille_de gauche) (taille_de droite)

module EnsembleLTL = 
  struct
    module S_ = SetO.Make(
      struct
	type t = ltl
	let compare = compare
      end)
    include S_

    let compare a b = Stdlib.compare (elements a) (elements b)
    let (=) a b     = compare a b = 0

    let to_string set =
      let exps = List.map to_string (elements set) in
      "{" ^ (String.concat ", " exps) ^ "}"
	
    (* Retourner un tuple contenant la plus grande formule de l'ensemble et les éléments restants *)
    let retourne_plus_grande_formule ensemble =
      let (_, largest) = List.fold_left (fun (size, value) formula ->
	let cur_size = taille_de formula in
	if size < cur_size then
	  (cur_size, formula)
	else
	  (size, value)
      ) (0, False) (elements ensemble) in
      (largest, remove largest ensemble)
end


(* Convertit une expression en forme normale négative. *)
let rec forme_normale_negative expression = match expression with
  | True | False | Atom(_) 
  	-> expression
  | And(g, d)     
  	-> And(forme_normale_negative g, forme_normale_negative d)
  | Or(g, d)      
  	-> Or(forme_normale_negative g, forme_normale_negative d)
  | Next(p)       
  	-> Next(forme_normale_negative p)
  | Finally(p)    
  	-> Finally(forme_normale_negative p)
  | Globally(p)   
  	-> Globally(forme_normale_negative p)
  | Until(g, d)   
  	-> Until(forme_normale_negative g, forme_normale_negative d)
  | Release(g, d) 
  	-> Release(forme_normale_negative g, forme_normale_negative d)
  | Not(expression) 
  	-> match expression with
	      | True           
	      	-> False
	      | False        
	      	-> True
	      | Atom(_)       
	      	-> Not(expression)
	      | Not(p)        
	      	-> forme_normale_negative p
	      | And(d,g)      
	      	-> Or(forme_normale_negative (Not d), forme_normale_negative (Not g))
	      | Or(d,g)       
	      	-> And(forme_normale_negative (Not d), forme_normale_negative (Not g))
	      | Next(p)       
	      	-> Next(forme_normale_negative (Not p))
	      | Finally(p)    
	      	-> Globally(forme_normale_negative (Not p))
	      | Globally(p)   
	      	-> Finally(forme_normale_negative (Not p))
	      | Until(d, g)   
	      	-> Release(forme_normale_negative (Not d), forme_normale_negative (Not g))
	      | Release(d, g) 
	      	-> Until(forme_normale_negative (Not d), forme_normale_negative (Not g))

(* Re-écriture partielle de l'expression en forme normale négative pour réduire le nombre de symboles. *)
			(* (◯ c) ⋀ (◯ b)   ->   ◯ (c ⋀ b) *)
let rec simplifier expression = 
      match expression with
	  | And(Next g, Next d) 
	  	-> Next(And(simplifier d, simplifier g))
	  | Until(Next g, Next d) 
	  	-> Next(Until(simplifier g, simplifier d))
	  | And(Release(a, x), Release(b,y)) when a = b 
	  	-> Release(a, And(x, y))
	  | Or(Release(a, x), Release(b, y)) when x = y 
	  	-> Release(Or(a, b), x)
	  | And(Globally g, Globally d) 
	  	-> Globally(And(simplifier d, simplifier g))
	  | Or(Globally(Finally d), Globally(Finally g)) 
	  	-> 
	    Globally(Finally(Or(simplifier d, simplifier g)))
	  | _ 
	  	-> expression

(* Conjonction concaténation d'une liste ltl *)
let et_concat = 
	function
	    | [] -> True
	    | (e::exps) -> List.fold_left (fun a b -> And(a, b)) e exps

(* Une expression est réduite si elle est un littéral ou si elle a suivant comme connecteur le plus externe *)
let est_reduit = 
	function
	    | True | False | Atom(_) | Not(Atom(_)) | Next(_) -> true
	    | _ -> false

(* Appliquer les règles de réduction epsilon sur un ensemble de formules pour produire des transitions valides vers des ensembles plus réduits *)
let epsilon_transforme ensemble =
  (* Application d'une seule règle, en gardant la trace des !transitions *)
  let appliquer_regle expression = 
  	match expression with
	    | And(g, d) 
	    	-> [(EnsembleLTL.of_list [g; d], None)]
	    | Or(g, d) 
	    	-> [(EnsembleLTL.singleton g, None); (EnsembleLTL.singleton d, None)]
	    | Release(g, d) 
	    	-> [(EnsembleLTL.of_list [g ; d], None); (EnsembleLTL.of_list [d ; Next(expression)], None)]
	    | Globally(p) 
	    	-> [(EnsembleLTL.of_list [p; Next(expression)], None)]
	    | Until(g, d) 
	    	-> [(EnsembleLTL.singleton d, None); (EnsembleLTL.of_list [g;Next(expression)], Some(expression))]
	    | Finally(p) 
	    	-> [(EnsembleLTL.singleton p, None); (EnsembleLTL.singleton (Next expression), Some(expression))]
	    | _ 
	    	-> failwith "forme déja réduite donnée"
  	in
     let (reduit, complexe) = EnsembleLTL.partition est_reduit ensemble in
  if EnsembleLTL.is_empty complexe then
    None
  else
    let (expression, complexe) = EnsembleLTL.retourne_plus_grande_formule complexe in
    let reste = EnsembleLTL.union reduit complexe in
    let transforme = appliquer_regle expression in (* Réduire la plus grande formule non réduite *)
    Some(List.map (fun (ensemble, condition) 
    		-> (EnsembleLTL.union ensemble reste, condition)) transforme)

(* Calculer la condition de la transformation sigma et l'ensemble des résultats *)
let sigma_transforme ensemble =
  List.fold_left (fun (conditions, suivant) ->
        function
	    | True 
	    	-> (conditions, suivant)
	    | Atom(p) 
	    	-> (Atom(p) :: conditions, suivant)
	    | Not(Atom(p)) 
	    	-> (Not(Atom(p)) :: conditions, suivant)
	    | Next(x) 
	    	-> (conditions, EnsembleLTL.add x suivant)
	    | False 
	    	-> failwith "Faux incohérent"
	    | other 
	    	-> failwith ("non réduite " ^ to_string other))
	    ([], EnsembleLTL.empty)(EnsembleLTL.elements ensemble)
 let calculer_ou prop1 prop2 =
  let rec merge prop1 prop2 =
    if prop1 = prop2 then
      Some(prop1)
    else if taille_de prop1 > taille_de prop2 then
      merge prop2 prop1
    else
      match prop1 with
	| True 
		-> Some(True)
	| False 
		-> Some(prop2)
	| _ 
		->
	  match prop2 with
	    | True 
	    	-> Some(True)
	    | False 
	    	-> Some(prop1)
	    | Atom(_) 
	    	-> None
	    | Not(_) 
	    	-> None
	    | And(l, r) 
	    	-> begin
	      match merge prop1 l with
		| Some(result) 
			-> Some(Or(result, r))
		| None 
			->
		  match merge prop1 r with
		    | Some(result) 
		    	-> Some(Or(l, result))
		    | None 
		    	-> None
	    end
	    | _ 
	    	-> failwith "Pas dans le langage propositionnel"
	in
	  match merge prop1 prop2 with
	    | Some(result) 
	    	-> result
	    | None 
	    	-> Or(prop1, prop2)

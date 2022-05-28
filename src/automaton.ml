type lien = 
	Epsilon of Ltl.ltl list | 
	Sigma of Ltl.ltl list * Ltl.ltl list

type etat = Ltl.EnsembleLTL.t

type transition = {
  lien : lien;
  s : etat;
  t : etat;
}

(* Ordonner les transitions lexicographiquement *)
module TransitionsOrdonnees = struct
  type t = transition
  let compare a b =
    let c = compare a.lien b.lien in
    if c <> 0 then c else
      let c = Ltl.EnsembleLTL.compare a.s b.s in
      if c <> 0 then c else
	Ltl.EnsembleLTL.compare a.t b.t
  let (=) a b = 
    compare a b = 0
end

module EnsembleTransition = SetO.Make(TransitionsOrdonnees)

(* Automate de Buchi généralisé *)
type buchiAutomaton = {
  commence: etat list;
  finisse: etat list;
  transitions: EnsembleTransition.t;
}

let lien_to_string lien =
  let conditions_format conds =
    String.concat " ∧ " (List.map Ltl.to_string conds)
  in
  match lien with
    | Sigma(conds, repousse) 
    	-> "Σ" ^ (conditions_format conds) 
    | Epsilon(repousse) 
    	-> "ε" ^ (String.concat "" (List.map (fun f -> ", !" ^ Ltl.to_string f) repousse))

let transition_vers_chaine {lien = lien; s = s; t = t} =
  Printf.sprintf "%s -> %s (%s)" (Ltl.EnsembleLTL.to_string s) (Ltl.EnsembleLTL.to_string t) (lien_to_string lien)

(* Produire un ensemble de transitions en ajoutant des transitions epsilon et en réduisant le graphe résultant *)
let rec reduction_graphique transitions etat =
  let est_connu trans transitions =
    EnsembleTransition.exists (TransitionsOrdonnees.(=) trans) transitions
  in
  let ajouter_transition trans transitions =
    if 
    	est_connu trans transitions then transitions
    else 
    	reduction_graphique (EnsembleTransition.add trans transitions) trans.t
  in
  let epsilon_de_loption = function
    | None -> Epsilon([])
    | Some(p) -> Epsilon([p])
  in
  match Ltl.epsilon_transforme etat with
    | None ->
      let (conds, next) = Ltl.sigma_transforme etat in
      let trans = {lien = Sigma(conds, []); s = etat; t = next } in
      ajouter_transition trans transitions
    | Some(conv_list) ->
      List.fold_left (fun transitions (next, cond) ->
	let trans = { lien = epsilon_de_loption cond;
		      s = etat; t = next } in
	ajouter_transition trans transitions
      )	transitions conv_list
      
(* Construire un Automaton Buchi à partir d'un état de départ *)
let construire_de etat_de_depart =
  { commence = [etat_de_depart]; finisse = []; transitions = reduction_graphique EnsembleTransition.empty etat_de_depart }

(* Produit un graphe à partir du buchi Automaton *)
let vers_graph automaton =
  let mettre_a_s = Ltl.EnsembleLTL.to_string in
  let g = (Graph.nouveau_graph "Automaton") in
  let est_debut s = List.exists ((=) s) automaton.commence in
  (* N'accepte que les branches infinies de la forme not(aUb) <=> a'Rb' dans nnf *)
  let est_acceptant s = List.for_all (fun e -> 
    match e with
      | Ltl.Release(_, _) 
      	-> true
      | Ltl.Next(_) 
      	-> true
      | Ltl.Globally(_) 
      	-> true
      | _ 
      	-> false
  ) (Ltl.EnsembleLTL.elements s) in
  let ajouter_un_noeud s = 
    (if est_debut s then
	if 
		est_acceptant s then Graph.ajouter_un_premier_noeud_final
	else 
		Graph.ajouter_un_premier_noeud
     else 
	if 
		est_acceptant s then Graph.ajouter_un_noeud_final
	else 
		Graph.ajouter_un_noeud) in
  List.fold_left (fun g { lien = lien; s = s; t = t } ->
    let s_string = mettre_a_s s in
    let t_string = mettre_a_s t in
    let g = (ajouter_un_noeud s) g s_string in
    let g = (ajouter_un_noeud t) g t_string in
    Graph.lien g s_string t_string (lien_to_string lien)
  ) g (EnsembleTransition.elements automaton.transitions)

(* Suppression inefficace des doublons *)
let rec unique = 
	function
	  | [] -> []
	  | hd::tl -> hd :: (unique (List.filter (fun x -> x <> hd) tl))
  
let unique_repousses transitions =
  unique (List.fold_left (fun postponed { lien = lien } ->
    match lien with
      | Epsilon(p) 
      	-> p @ postponed
      | _ 
      	-> postponed
  ) [] transitions
)

let setup_repousses_sigma repousses =
  List.map (fun trans ->
    match trans.lien with
      | Sigma(list1, list2) 
      	-> { trans with lien = Sigma(list1, list2 @ repousses) }
      | _ 
      	-> trans)

let sauter_epsilons automaton =
  let transitions = EnsembleTransition.elements automaton.transitions in
  let repousses = unique_repousses transitions in
  let transitions = setup_repousses_sigma repousses transitions in
  let rec saute transitions =
    let (epsilons, sigmas) = List.partition (fun t ->
      match t.lien with
	| Epsilon(_) 
		-> true 
	| Sigma(_, _) 
		-> false
    ) transitions in
    if (epsilons = []) then
      sigmas
    else
      let reste = List.tl epsilons @ sigmas in
      let target = List.hd epsilons in
      if Ltl.EnsembleLTL.(target.s = target.t) then
        saute reste
      else
        let replacer regle_re_ecrite =
          let (suivants, reste) = List.partition (fun t -> Ltl.EnsembleLTL.(t.s = target.t)) reste in
          let transitions = List.fold_left (fun transitions n ->
            if List.exists (fun t -> Ltl.EnsembleLTL.(t.t = n.s)) transitions then (* toujours référencée *)
              (regle_re_ecrite n) :: n :: transitions
            else
              (regle_re_ecrite n) :: transitions
          ) reste suivants in
          saute transitions
        in
        match target.lien with
          | Epsilon([]) ->
            replacer (fun suivant -> { suivant with s = target.s })
          | Epsilon(ps) ->
            let nouveau_lien = function
              | Epsilon(ps')  
              	-> Epsilon(ps @ ps')
              | Sigma(c, constraints) 
              	-> Sigma(c, List.find_all (fun p -> not (List.exists ((=) p) ps)) constraints)
            in
            replacer (fun suivant 
            		-> { suivant with s = target.s; lien = nouveau_lien suivant.lien })
          | _ 
          	-> failwith "valeur non-epsilon inattendue"
  in
  { automaton with transitions = EnsembleTransition.of_list (saute transitions) }

let est_fusionnable g d =
  if g.s = d.s && g.t = d.t then
    match (g.lien, d.lien) with
      | (Epsilon(g_ps), Epsilon(d_ps))
      | (Sigma(_, g_ps), Sigma(_, d_ps)) 
      	-> g_ps = d_ps
      | _ 
      	-> false
  else
    false

let fusionner_transitions g d =
  let lien_fusionnee = match (g.lien, d.lien) with
      | (Epsilon(_), Epsilon(_)) -> g.lien
      | (Sigma(g_condition, ps), Sigma(d_condition, _)) 
      	-> begin
        match Ltl.calculer_ou (Ltl.et_concat g_condition) (Ltl.et_concat d_condition) with
          | Ltl.True 
          	-> Sigma([], ps)
          | prop 
          	-> Sigma([prop], ps)
      end
      | _ 
      	-> failwith (Printf.sprintf "Impossible de fusionner  %s avec %s" (lien_to_string g.lien) (lien_to_string d.lien))
  in
  { g with lien = lien_fusionnee }

let fusionner_vers_paralleles transitions trans =
  match List.find_all (est_fusionnable trans) transitions with
    | [] 
    	-> trans :: transitions
    | fusionner_vers :: _ 
    	-> fusionner_transitions trans fusionner_vers :: List.filter (fun x -> x <> fusionner_vers) transitions

let joindre_sigmas buchiAutomaton =
  let transitions = List.fold_left (fun transitions trans 
  ->
    fusionner_vers_paralleles transitions trans
  ) [] (EnsembleTransition.elements buchiAutomaton.transitions) in
  { buchiAutomaton with transitions = EnsembleTransition.of_list transitions }

let construire_GBA_from ensemble_ltl =
  joindre_sigmas
    (sauter_epsilons
       (construire_de ensemble_ltl))

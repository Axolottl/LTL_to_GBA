(* Fichiers de sortie compatibles avec dot *)
open Printf

type forme = DoubleCircle | Circle

(* nœuds et arêtes *)

type noeud = {
  nom : string;
  forme : forme;
  premier : bool;
}

type arete = {
  etiquette : string;
  pnoeud : noeud;
  dnoeud : noeud;
}

(* Le graphe *)
type graph = {
  titre : string;
  parametres : string list;
  noeuds : noeud list;
  aretes : arete list;
}

let nouveau_graph titre =
  { titre = titre; parametres = []; noeuds = []; aretes = [] }

let trouver_un_noeud {noeuds = noeuds} nom =
  List.find (fun n -> n.nom = nom) noeuds

let ajouter_un_noeud_internal graph noeud =
  try begin
    ignore (trouver_un_noeud graph noeud.nom);
    graph
  end with
    | Not_found -> 
    	{ graph with noeuds = noeud :: graph.noeuds }

let ajouter_un_noeud_final graph nom =
  ajouter_un_noeud_internal graph 
  	{ nom = nom; forme = DoubleCircle; premier = false }

let ajouter_un_premier_noeud graph nom =
  ajouter_un_noeud_internal graph 
  	{ nom = nom; forme = Circle; premier = true }

let ajouter_un_premier_noeud_final graph nom =
  ajouter_un_noeud_internal graph 
  	{ nom = nom; forme = DoubleCircle; premier = true }

let ajouter_un_noeud graph nom =
  ajouter_un_noeud_internal graph 
  	{ nom = nom; forme = Circle; premier = false }

let lien graph s_nom t_nom etiquette =
  {
   graph with aretes = 
      { etiquette = etiquette;
	pnoeud = trouver_un_noeud graph s_nom; 
	dnoeud = trouver_un_noeud graph t_nom;
      } :: 
    graph.aretes 
   }

let affichage_noeuds out noeuds =
  let affichage_noeud_list noeuds =
    if not (noeuds = []) then
      (List.iter (fun n -> fprintf out "\"%s\" " n.nom) noeuds;
       fprintf out ";\n")
  in
  let (circle, double) = List.partition (fun n -> n.forme = Circle) noeuds in
	  fprintf out "\tnode [shape = doublecircle]; ";
  affichage_noeud_list double;
	  fprintf out "\tnode [shape = circle]; ";
  affichage_noeud_list circle;
  let premiers = List.find_all (fun n -> n.premier) noeuds in
  List.iter (fun n -> 
    fprintf out "\t\"_nil_%s\" [style=\"invis\"];\n\t\"_nil_%s\" -> \"%s\";\n"
      n.nom n.nom n.nom) premiers

let affichage_aretes out =
  List.iter (fun element ->
    fprintf out "\t\"%s\" -> \"%s\" [ label = \"%s\" ];\n" element.pnoeud.nom element.dnoeud.nom element.etiquette)

let affichage_graph out { titre = titre; parametres = parametres; noeuds = noeuds; aretes = aretes } =
  fprintf out "digraph %s {\n" titre;
  List.iter (fprintf out "\t%s\n") parametres;
  affichage_noeuds out noeuds;
  affichage_aretes out aretes;
  fprintf out "}\n"

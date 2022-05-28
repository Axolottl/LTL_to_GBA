(* Message d'utilisation *)
let fichier chemin =
  open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_text] 0o666 chemin

let main formula chemin =
  let formula = (Parser.main Lexer.token (Lexing.from_string formula)) in
  print_endline ("Interprété : φ = " ^ (Ltl.to_string formula) ^ ".");
  print_endline "	1. Conversion de la formule LTL en forme normale négative.";
  let ensemble_formule = (Ltl.EnsembleLTL.singleton (Ltl.forme_normale_negative formula)) in
  print_endline "	2. Construction d'un automate Büchi à partir de la formule.";
  let automaton = Automaton.construire_GBA_from ensemble_formule in
  print_endline "	3. Impression de l'automate sur un graphique.";
  let graph = Automaton.vers_graph automaton in
  let temp = (chemin ^ ".tmp") in
  let out = fichier temp in
  Graph.affichage_graph out graph;
  close_out out;
  let _ = Sys.command (Printf.sprintf "dot -Tpng -o %s %s" chemin temp) in
  let _ = Sys.command (Printf.sprintf "rm %s" temp) in
  print_endline ("	4. La sortie du graphique est sauvegardée dans " ^ chemin)
		    
let usage = "Usage: LTL_to_BA [φ Expression] [fichier de sortie]\n" ^
  "Voir `LTL_to_BA --help' pour plus d'informations.\n"

let help_arg = "Exemple : ./LTL_to_BA \"X(aUb)\" out.png\n"

(* Fonction principale, produit un graphique à partir d'une entrée ltl de type chaîne de caractères *)
let () =
  if Array.length Sys.argv >= 2 && Sys.argv.(1) = "--help"
  then print_string help_arg else
    if Array.length Sys.argv <> 3 then print_string usage
    else
      (* Exécuter le programme principal sur les arguments d'entrée *)
      main Sys.argv.(1) Sys.argv.(2)

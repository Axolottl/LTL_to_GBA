<div align="center"><h1> 𝝍  ↬  𝕻</h1>

Linear Temporal Logic compiler to Generalized Büchi Automata.

</div>

# Traduction de la logique linéaire temporelle en automates de Büchi

Fournit un outil de base pour la traduction d'expressions LTL en accepteurs d'automates de Büchi.

# Utilisation
```sh
$ sudo apt-get install graphviz
$ ./build
$ LTL_to_BA <expression φ>[ex : aUb] <fichier de sortie>[ex : out.png]
$ eog <fichier de sortie>
```

# Syntaxe
Afin de réduire la taille et la complexité des automates résultants, la syntaxe standard de la logique temporelle a été étendue avec plusieurs notations redondantes à inclure :

Les opérateurs standards de la logique propositionnelle : and, or, not
Les notions de base de la logique temporelle linéaire : X (suivant), U (jusqu'à), F (finallement), G(globallement) et R(release).

# Sortie
Une fois exécuté, le programme produit une représentation visuelle de l'automate résultant au format png. Chaque état est représenté par un nœud étiqueté avec un ensemble de formules qui n'ont pas encore été satisfaites. Les transitions sont représentées par des arêtes étiquetées où l'étiquette est une expression booléenne positive sur les atomes et leurs compléments.

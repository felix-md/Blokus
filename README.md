# BlokusPPF-2024

Projet d'études, Jeux de plateau Blokus réalisé en Ocaml

# Pour lancer le programme :

installer les dépendances :

```bash
opam install . --deps-only
``` 


build le projet :

```bash
dune build
```
puis, lancer l'executable :

```bash
dune exec --display=quiet ./bin/main.exe
```

# Blokus

## Description
Blokus est un jeu de stratégie abstrait où les joueurs doivent placer un maximum de leurs pièces sur un plateau de jeu tout en respectant certaines règles de placement. Ce projet vise à reproduire les règles et l'expérience de jeu de Blokus dans un format numérique.

## Composition du jeu
- **Plateau** : Un plateau de 400 cases (20x20).
- **Pièces** : Chaque joueur dispose de 21 pièces de forme unique dans une couleur spécifique (bleu, jaune, rouge ou vert).
  - 1 pièce de 1 carré (« monomino »)
  - 1 pièce de 2 carrés (« domino »)
  - 2 pièces de 3 carrés (« triominos »)
  - 5 pièces de 4 carrés (« tétraminos »)
  - 12 pièces de 5 carrés (« pentaminos »)

![Plateau de jeu](documentation/images/sac%20de%20piece.jpg)

## Règles du jeu

### But du jeu
Chaque joueur doit placer le maximum de ses 21 pièces sur le plateau. Le joueur avec le moins de carrés non placés gagne.

### Règles de placement
1. **Départ** : Chaque joueur commence dans un des coins du plateau.
2. **Ordre de jeu** : Bleu, jaune, rouge, puis vert.
3. **Placement des pièces** :
   - Chaque nouvelle pièce doit toucher au moins une autre pièce de la même couleur **par un coin uniquement**, sans contact par les côtés.

#### Contraintes
![Plateau de jeu](documentation/images/contraintes.jpg)

### Fin de la partie
La partie se termine lorsqu'aucun joueur ne peut plus placer de pièce. Les joueurs doivent alors passer leur tour, et le jeu continue jusqu'à ce que plus personne ne puisse jouer.

### Calcul des points
1. Chaque carré non placé donne un point négatif.
2. Bonus :
   - **15 points** si un joueur a placé ses 21 pièces.
   - **20 points** si les 21 pièces sont placées et que le monomino (pièce de 1 carré) est posé en dernier.

### Victoire
Le joueur avec le **score le plus élevé** (le moins de points négatifs) remporte la partie.




Yanis Mansouri : discord zair_ksm
Félix Martins-Ducasse : discord felix_md

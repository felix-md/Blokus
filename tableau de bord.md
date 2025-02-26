# Tableau de bord du projet Blokus

## Structure générale des fichiers


On va utilisé les principes du MVC et de la POO

Structure générale des fichiers :

```
blokus/
├── bin/
│   └── main.ml
├── src/
│   ├── model/
│   │   ├── board/
│   │   │   ├── board.ml
│   │   │   └── board.mli
│   │   ├── piece/
│   │   │   ├── piece.ml
│   │   │   └── piece.mli
│   │   ├── player/
│   │   │   ├── player.ml
│   │   │   └── player.mli
│   │   └── game_state/
│   │       ├── game_state.ml
│   │       └── game_state.mli
│   ├── view/
│   │   ├── terminal_view.ml
│   │   └── terminal_view.mli
│   ├── controller/
│   │   ├── game_controller.ml
│   │   ├── game_controller.mli
│   └── utils/
│       ├── utils.ml
│       └── utils.mli
├── tests/
│   ├── test_board.ml
│   ├── test_piece.ml
│   └── ...
├── dune-project
├── dune
└── README.md
```



### Remarques sur l'implémentation :

- **ANSITerminal** : Utilisation d'ANSITerminal pour améliorer l'affichage dans le terminal. Pour l'installer, exécutez:
`opam install ANSITerminal`.
  
- Concepts utilisés :
  - **Foncteurs**
  - **Polymorphisme et/ou paramétrique**
  - **Abstraction des types**


Signatures utilisé :
Game :

```module type GAME = sig
    type game_state
    type game_view
    type player
    type play
    type error
  
    type outcome = 
      | Next of game_state 
      | Error of error 
      | Endgame of player list
  
    val view : game_state -> player -> game_view
    val display : game_view -> int
    val act : player -> play -> game_state -> outcome
    val bot : game_view -> play
  end
```

[![Haskell CI](https://github.com/solidsnakedev/brick-game/actions/workflows/haskell.yml/badge.svg)](https://github.com/solidsnakedev/brick-game/actions/workflows/haskell.yml)

# brick-game

## Keyboard Control Board
Board can be move left `a` or right `d` , discard any other input except `q` to exit the game
* a -> Move Left
* d -> Move Right
* q -> Exit Game

## Set Board Size
### Board size can be set in the below file
brick-game/lib/Animation/Env.hs
`Line 13`
```
initGameEnv = Env 30 20 5
```

## Set number of bricks
### Number of bricks can be assigned in the below file
brick-game/lib/Animation/State.hs
`Line 67`
```
brickList <- mapM (\_-> getRandPos) [1..70]
```


```
|------------------------------|
|       ==   ==                |0
|   ==  =   = ==    =      == =|1
|    =    =           =    = = |2
|   =        =  = = =    =   ==|3
| =   = =      =          ===  |4
|     =  =    =  o   ===  ==  =|5
| = =    =       =     =       |6
|=          =   =             =|7
| =        =       =       =   |8
|                      =     = |9
|=   =   =           ==    =   |10
|                              |11
|                              |12
|                              |13
|                              |14
|                              |15
|                              |16
|                              |17
|                              |18
|                              |19
|:::::                         |
SCORE: 0
Debug: 17 5 GoLeft GoDown ["brick(17,5)"]0
```

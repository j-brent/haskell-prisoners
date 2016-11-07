{- THE GAME OF PRISONERS ON GRAPHS

The givens are:

(a) A simple graph, that is, no weights on the nodes, no loops, no directionality, no multiple edges [http://mathworld.wolfram.com/SimpleGraph.html], which in addition is finite. Every node stands for a "player" and every edge for an "opponent" relation (which is indeed just symmetric).

(b) A finite set of available "moves", common to all players.

(c) A finite set of possible "payoffs", common to all players.

The game is played in rounds.

 (R 1) Every player (secretly) decides on a move based on some arbitrary strategy (all moves are announced simultaneously). The payoff for each player is determined according to some arbitrary rules.

 (R n+1) Each player decides on a move based on their strategy, taking into account the payoffs of all players at previous rounds. Again, the payoff for each player is calculated, based on the moves of the player and their opponents in the same round.

It's a deterministic game. The way a player decides on their move, ie their strategy, is not arbitrary in the sense of "random" or "psychologically motivated" or something, but some preselected algorithm, conceivably reflecting an assumed rationality on behalf of the player, for example (to be implemented below): is there a opponent who had a bigger payoff at round n? if yes, then the move at n+1 will be the move at n of the opponent with the biggest payoff at n; if no, then the move at n+1 will be the move of the player at n. Similarly, the payoff rules are also not random, but algorithmic rules which are assumed to be known to, and hopefully agreed upon by, all players.

So, loosely speaking, we have two sorts of algorithms: strategies and payoff rules. Which raises the question of uniformity: are these algorithms shared by everyone? Regarding payoffs obviously yes (it's one game, the rules apply to all), whereas regarding strategies it's for reasons of simplicity that we may choose to have uniformity.

-}


{- TYPE SYSTEM

We need three base datatypes, one for the players, one for the payoffs, and one for the moves. I use the "data" construct instead of either the "newtype" (we need several constructors for each of the basic types) or the "type" (it seems cleaner to avoid aliasing and to directly use new datatypes).

I use the "deriving" clause to automatically equip the datatypes with equality, order, and the ability to show and read the values respectively. Not sure what the difference between the classes "Enum" and "Ord" is exactly, and if we really need both of them in all three types. An important thing about the Enum class is that we can always turn values into integers using its method "fromEnum", which is a poor's man way to do something like this; a more sophisticated way would be to say

 data PayoffP a = NoneP a | SomeP a | MuchP a | AllP a

that is, to parametrize the type of payoffs by some (numeric) type "a" like integers or floating point numbers -- ma non adesso.

As for the "Bounded" class, I include it in order to make use of the trick shown here: http://stackoverflow.com/a/4300640/3749908 .

-}

data Player = Alina | Brian | Cyrill | Dana | Ephraim | Forest--we can always extend this of course
 deriving (Eq, Enum, Ord, Show, Read, Bounded)
 
data Payoff = None | Some | Much | All --these correspond to the pet case payoffs of 0, 1, 3, and 5 points
 deriving (Eq, Enum, Ord, Show, Read, Bounded)
 
data Move = Cooperate | Defect
 deriving (Eq, Enum, Ord, Show, Read, Bounded)
 
{-

Let's introduce shorthands ("aliases") for more types that are meaningful for the ontology that we have in mind.

-}

--height 0

type PayoffValue = Int --this would serve as the value space of the payoffs (in our case integers suffice), allowing also to take sums and do arithmetic that the "Payoff" type does not immediately provide; we can turn a payoff into the corresponding value by using the method fromEnum

--height 1

type MoveState = Player -> Move
type PayoffFunction = Move -> Move -> Payoff --not sure if this shouldn't be given a more general type (meaning, if it should really just depend on the moves regardless of, say, the players, the graph structure etc); in any case, the first input typically concerns the current player and the second one their opponent, whereas the output is the payoff for the current player
type Neighborhoods_Lookup = Player -> [Player] --this would be the "lookup table representation" of the graph
type Neighborhoods_Matrix = Player -> Player -> Bool --this would be the "matrix representation" of the graph
type Game = [MoveState]

--height 2

type PayoffState = Player -> MoveState -> Payoff --after some reflection it becomes clear that a payoff state should depend on a move state: there are no payoffs before moves have been made, and the payoffs actually depend on the kinds of moves
type MoveStateUpdate = PayoffFunction -> MoveState -> MoveState --this turns out to be the only update function that we need, namely a move state update

{- TERMS

Now let's define our pet game from Prague to have something to play with. We got six players, four possible payoffs, and two different moves, so the above typology is adequate. We just need to have certain "constants", ie, definitions of specific functions, the ones that make up the game as we discussed it in Prague; for these specific constants I use the suffix "_p". But there'll also be a couple more.

First we can give the opponent relation. We can do this here explicitly, for instance in a lookup-table representation.

-}

opps_lookup_p :: Player -> [Player]

opps_lookup_p Alina = [Dana]
opps_lookup_p Brian = [Dana]
opps_lookup_p Cyrill = []
opps_lookup_p Dana = [Alina, Brian, Ephraim, Forest]
opps_lookup_p Ephraim = [Dana, Forest]
opps_lookup_p Forest = [Dana, Ephraim]

{-

Or we can decide for a more sophisticated definition (which would demand some thought on what it all means and how we want to use it), like as a matrix.

-}

opps_matrix_p :: Player -> Player -> Bool

opps_matrix_p Alina Dana = True
opps_matrix_p Brian Dana = True
opps_matrix_p Dana Ephraim = True
opps_matrix_p Dana Forest = True
opps_matrix_p Ephraim Forest = True
opps_matrix_p p1 p2
 | (fromEnum p1) > (fromEnum p2) = opps_matrix_p p2 p1
 | otherwise = False


{-

Here I'll use the first representation, so I introduce a shorthand for brevity. (Note that we don't have to mention arguments in this definition.)

-}

opps_p :: Player -> [Player]
opps_p = opps_lookup_p

{-

We also need the specific payoff function. This can again be given explicitly or not; again, in the latter case the definition necessarilly reflects the coder's understanding of the theory, in other words, we could come up with several different ways to implement this function, each of them suggesting different and possibly unique generalizations.

-}

pf_p :: Move -> Move -> Payoff

pf_p Cooperate Cooperate = Much
pf_p Cooperate Defect = None
pf_p Defect Cooperate = All
pf_p Defect Defect = Some

pf_p_2 :: Move -> Move -> Payoff

pf_p_2 s1 s2
 | sdiff > 0 = All
 | sdiff == 0 = if s1 == Cooperate then Much else Some
 | sdiff < 0 = None 
  where sdiff = (fromEnum s1) - (fromEnum s2)
  
{-

Sanity check: Do the two payoff functions have the same extension?

In general, the question "are two functions extensionally equal?" is unanswerable (see http://stackoverflow.com/q/9906628/3749908), and even for finite domains the answer can be very inefficient. Here of course it's easy to do it. For example,

> [pf_p s1 s2 == pf_p_2 s1 s2 | s1 <- [Cooperate, Defect], s2 <- [Cooperate, Defect]]

should output

[True,True,True,True] .

We could actually make a test function out of this, for future use, like

haveEqualExtension :: PayoffFunction -> PayoffFunction -> Bool

haveEqualExtension pf1 pf2 = foldl (&&) True [pf1 s1 s2 == pf2 s1 s2 | s1 <- [Cooperate, Defect], s2 <- [Cooperate, Defect]]

-}

{-

In order to define the move state update function of Prague, it helps to break the procedure in lesser functions. I begin with the (here trivial) function for turning a Payoff term to a PayoffValue term. It is trivial because in our case the constant to be defined is just a synonym for "fromEnum".

-}

pval :: Payoff -> PayoffValue
pval pf = fromEnum pf

totalPayoffValue :: MoveState -> PayoffFunction -> Player -> PayoffValue
totalPayoffValue ms pf player = sum (map (\opp -> pval (pf (ms player) (ms opp))) (opps_p player))

biggestEarner :: MoveState -> PayoffFunction -> Player -> [Player] -> Player
biggestEarner ms pf pl [] = pl
biggestEarner ms pf pl (op:ops)
 | (totalPayoffValue ms pf pl) >= (totalPayoffValue ms pf op) = biggestEarner ms pf pl ops
 | otherwise = biggestEarner ms pf op ops

msu_p :: PayoffFunction -> MoveState -> MoveState
msu_p pf ms pl = ms (biggestEarner ms pf pl (opps_p pl))

{-

Assume as initial move state the one where every player decides it's better to start the game by defecting.

-}

ms_p :: MoveState
ms_p _ = Defect

{-

Two last definitions, nonspecific to the Prague specification: the n-th iterate of a game and the trick of the stackoverflow user ozgur.

-}

rounds :: MoveStateUpdate -> PayoffFunction -> MoveState -> Int -> MoveState
rounds msu pf ms 0 = ms
rounds msu pf ms n = msu pf (rounds msu pf ms (n-1))

allValues :: (Bounded a, Enum a) => [a]
allValues = [minBound..]

{-

A term to play with; it just packs up the function "rounds" with our choices of "_p" parameters.

-}

game_p :: Int -> [Move]
game_p n = map (rounds msu_p pf_p ms_p n) (allValues :: [Player])

{-

Alternative constants.

-}

ms_p1 :: MoveState
ms_p1 Alina = Defect
ms_p1 _ = Cooperate

game_p1 :: Int -> [Move]
game_p1 n = map (rounds msu_p pf_p ms_p1 n) (allValues :: [Player])

--

ms_p2 :: MoveState
ms_p2 Dana = Defect
ms_p2 _ = Cooperate

game_p2 :: Int -> [Move]
game_p2 n = map (rounds msu_p pf_p ms_p2 n) (allValues :: [Player])

--

ms_p3 :: MoveState
ms_p3 Alina = Defect
ms_p3 Brian = Defect
ms_p3 _ = Cooperate

game_p3 :: Int -> [Move]
game_p3 n = map (rounds msu_p pf_p ms_p3 n) (allValues :: [Player])

--

ms_p4 :: MoveState
ms_p4 Alina = Defect
ms_p4 Brian = Defect
ms_p4 Dana = Defect
ms_p4 _ = Cooperate

game_p4 :: Int -> [Move]
game_p4 n = map (rounds msu_p pf_p ms_p4 n) (allValues :: [Player])
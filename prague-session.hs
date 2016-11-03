{-

We need three base datatypes, one for the players, one for the payoffs, and one for the moves -- what we called "strategies", which on retrospect sounds to me better suited for a different concept (namely, for a "plan" of a given player that would result in determining several moves in a game). I use the "data" construct instead of either the "newtype" (we need several constructors for each of the basic types) or the "type" (it seems cleaner to avoid aliasing and to directly use new datatypes).

I use the "deriving" clause to automatically equip the datatypes with equality, order, and the ability to show and read the values respectively. Not sure what the difference between the classes "Enum" and "Ord" is exactly, and if we really need both of them in all three types. An important thing about the Enum class is that we can always turn values into integers using its method "fromEnum".

-}

data Player = Alina | Brian | Cyrill | Dana | Ephraim | Forest--we can always extend this of course
 deriving (Eq, Enum, Ord, Show, Read)
 
data Payoff = None | Some | Much | All --these correspond to the pet case payoffs of 0, 1, 3, and 5 points
 deriving (Eq, Enum, Ord, Show, Read)
 
data Move = Cooperate | Defect
 deriving (Eq, Enum, Ord, Show, Read)
 
{-

Let's introduce shorthands ("aliases") for some higher types that are meaningful for the ontology that we have in mind.

-}

--height 1

type MoveState = Player -> Move
type PayoffState = Player -> Payoff
type PayoffFunction = Move -> Move -> Payoff --not sure if this shouldn't be given a more general type; in any case, the first input typically concerns the current player and the second one their opponent, whereas the output is the payoff for the current player
type Neighborhoods_Lookup = Player -> [Player] --this would be the "lookup table representation" of the graph
type Neighborhoods_Matrix = Player -> Player -> Bool --this would be the "matrix representation" of the graph
type Game = [MoveState]

--height 2

type PayoffStateUpdate = MoveState -> PayoffState --this is where u1 lives
type MoveStateUpdate = MoveState -> PayoffState -> MoveState --this is where u2 lives

--height 3

type Update = PayoffStateUpdate -> MoveStateUpdate -> MoveState -> MoveState --here lives the general update function; I changed the typing so that its dependence on (different choices of) u1 and u2 becomes apparent and usable

{-

For the definition of the general update function, I use the variables "psu" for a payoff state update (the old u1), "ssu" for a move state update (the old "u2"), and "ss" for a move state.

-}

update :: PayoffStateUpdate -> MoveStateUpdate -> MoveState -> MoveState
update psu ssu ss = ssu ss (psu ss)

{-

Now let's define our pet game from Prague to have something to play with. We got two players, three possible payoffs, and two different moves, so the above typology is adequate.

We need the payoff function. This can be given explicitly, or not; in the latter case the definition necessarilly reflects the coder's understanding of the theory, in other words, we could come up with several different ways to implement this function, each of them suggesting different and possibly unique generalizations.

-}

pf_prague :: Move -> Move -> Payoff

pf_prague Cooperate Cooperate = Much
pf_prague Cooperate Defect = None
pf_prague Defect Cooperate = All
pf_prague Defect Defect = Some

pf_prague_2 :: Move -> Move -> Payoff

pf_prague_2 s1 s2
 | sdiff > 0 = All
 | sdiff == 0 = if s1 == Cooperate then Much else Some
 | sdiff < 0 = None 
  where sdiff = (fromEnum s1) - (fromEnum s2)
  
{-

Sanity check: Do the two payoff functions have the same extension?

In general, the question "are two functions extensionally equal?" is unanswerable (see http://stackoverflow.com/q/9906628/3749908), and even for finite domains the answer can be very inefficient. Here of course it's easy to do it. For example,

> [pf_prague s1 s2 == pf_prague_2 s1 s2 | s1 <- [Cooperate, Defect], s2 <- [Cooperate, Defect]]

should output

[True,True,True,True] .

We could actually make a test function out of this, for future use, like

haveEqualExtension :: PayoffFunction -> PayoffFunction -> Bool

haveEqualExtension pf1 pf2 = foldl (&&) True [pf1 s1 s2 == pf2 s1 s2 | s1 <- [Cooperate, Defect], s2 <- [Cooperate, Defect]]

-}

{-

Now let's define the two component update functions, one that determines the payoffs for the players (from the "last round") and one that determines the moves of the players (for the "next round"). To this end we already need the neighbors of a player, that is, that's where we have to provide the specific graph; we can do this here explicitly, for instance in a lookup-table representation (again, cleverer and more implicit definitions would demand some thought on what it all means).

-}

neighs_lookup_prague :: Player -> [Player]

neighs_lookup_prague Alina = [Dana]
neighs_lookup_prague Brian = [Dana]
neighs_lookup_prague Cyrill = []
neighs_lookup_prague Dana = [Alina, Brian, Ephraim, Forest]
neighs_lookup_prague Ephraim = [Dana, Forest]
neighs_lookup_prague Forest = [Dana, Ephraim]

neighs_matrix_prague :: Player -> Player -> Bool

neighs_matrix_prague Alina Dana = True
neighs_matrix_prague Brian Dana = True
neighs_matrix_prague Dana Ephraim = True
neighs_matrix_prague Dana Forest = True
neighs_matrix_prague Ephraim Forest = True
neighs_matrix_prague p1 p2
 | (fromEnum p1) > (fromEnum p2) = neighs_matrix_prague p2 p1
 | otherwise = False


psu_prague :: MoveState -> PayoffState

psu_prague ms = undefined

msu_prague :: MoveState -> PayoffState -> MoveState

msu_prague ms ps = undefined

{-

Assume that the two players start off with the respective moves ...

-}
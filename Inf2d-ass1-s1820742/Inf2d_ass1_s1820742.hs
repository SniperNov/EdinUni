-- Inf2d Assignment 1 2019-2020

-- Matriculation number:s1820742

-- {-# OPTIONS -Wall #-}





module Inf2d1 where



import Data.List (sortBy, elemIndices, elemIndex)

import ConnectFourWithTwist









{- NOTES:



-- DO NOT CHANGE THE NAMES OR TYPE DEFINITIONS OF THE FUNCTIONS!

You can write new auxillary functions, but don't change the names or type definitions

of the functions which you are asked to implement.



-- Comment your code.



-- You should submit this file when you have finished the assignment.



-- The deadline is the  10th March 2020 at 3pm.



-- See the assignment sheet and document files for more information on the predefined game functions.



-- See the README for description of a user interface to test your code.



-- See www.haskell.org for haskell revision.



-- Useful haskell topics, which you should revise:

-- Recursion

-- The Maybe monad

-- Higher-order functions

-- List processing functions: map, fold, filter, sortBy ...



-- See Russell and Norvig Chapters 3 for search algorithms,

-- and Chapter 5 for game search algorithms.



-}



-- Section 1: Uniform Search







-- The Node type defines the position of the agent on the graph.

-- The Branch type synonym defines the branch of search through the graph.

type Node = Int

type Branch = [Node]

type Graph= [Node]



numNodes::Int

numNodes = 4
--the graph is 4*4 



-- 



-- The next function should return all the possible continuations of input search branch through the graph.

-- Your function should return an empty list if the input search branch is empty.

-- This implementation of next function does not backtrace branches.

-- graph=[(0,0), (1,1), (2,2) ,(3,0),
--        (4,0), (5,0), (6,0) ,(7,1),
--        (8,0), (9,0), (10,0),(11,0),
--        (12,0),(13,0),(14,0),(15,0)]

next::Branch -> Graph ->  [Branch]

next branch [] = []

next [] g = []

next branch g= [concat[[n `mod` 4],branch]|(n,t) <- zip xs g ,n >= (head branch) * numNodes,n < (head branch + 1) * numNodes,t /= 0]  
                  where xs = [0..length(g)-1]  
   

-- |The checkArrival function should return true if the current location of the robot is the destination, and false otherwise.

checkArrival::Node -> Node -> Bool

checkArrival destination curNode = if curNode == destination then True
                                   else False 


explored::Node-> [Node] ->Bool

explored point exploredList = if point `elem` exploredList then True
                              else False


-- Section 3 Uniformed Search

-- | Breadth-First Search

-- The breadthFirstSearch function should use the next function to expand a node,

-- and the checkArrival function to check whether a node is a destination position.

-- The function should search nodes using a breadth first search order.


breadthFirstSearch::Graph -> Node->(Branch ->Graph -> [Branch])->[Branch]->[Node]->Maybe Branch

breadthFirstSearch g destination next [] exploredList = Nothing

breadthFirstSearch [] destination next branches exploredList = Nothing

breadthFirstSearch g destination next branches exploredList 

--If the head branch reaches the destination then return this branch
  |checkArrival destination (head(head branches)) = Just $ head branches

--If the first node of head branch is explored, then search from the second one   
  |head(head branches) `elem` exploredList = breadthFirstSearch g destination next (tail branches) exploredList

-- otherwise 1)Delete the first branch in agenda and add its child's branches to the end of the agenda
--           2)Update exploredlist: add the original first branch's leading node in to the list
--           3)Search again
  |otherwise =breadthFirstSearch g destination next (filter(not.null) (tail branches ++(next (head branches) g))) (head(head branches):exploredList)

 


-- | Depth-Limited Search

-- The depthLimitedSearch function is similiar to the depthFirstSearch function,

-- except its search is limited to a pre-determined depth, d, in the search tree.



depthLimitedSearch::Graph ->Node->(Branch ->Graph-> [Branch])->[Branch]-> Int->[Node]-> Maybe Branch


depthLimitedSearch g destination next [] d exploredList = Nothing


depthLimitedSearch [] destination next branches d exploredList = Nothing


depthLimitedSearch g destination next branches d exploredList

-- Depth is equal to number of nodes in the first branch minus 1, if exceed 100 then 'delete' this branch
    | d > 100 = depthLimitedSearch g destination next (tail branches) (length(head (tail branches))-1) exploredList

-- If the head branch reaches the destination then return this branch
    | checkArrival destination (head (head branches)) = Just $ (head branches)
    
-- If the first node of head branch is explored, then search from the second one 
    | (head (head branches)) `elem` exploredList = depthLimitedSearch g destination next (tail branches) (length(head branches)-1) exploredList                   

-- otherwise 1)Replace the first branch in agenda to its child's branches
--           2)Update exploredlist: add the original first branch's leading node in to the list
--           3)Search again
    | otherwise = depthLimitedSearch g destination next (filter(not.null) ((next (head branches) g)++((tail branches))))(length(head branches)-1) (head (head branches):exploredList)




-- | Section 4: Informed search





-- | AStar Helper Functions



-- | The cost function calculates the current cost of a trace. The cost for a single transition is given in the adjacency matrix.

-- The cost of a whole trace is the sum of all relevant transition costs.

-- graph=[(0,0), (1,1), (2,2) ,(3,0),
--        (4,0), (5,0), (6,0) ,(7,1),
--        (8,0), (9,0), (10,0),(11,0),
--        (12,0),(13,0),(14,0),(15,0)]

--graph2=[0,1,2,0,0,
--        0,0,4,0,0,
--        0,0,1,1,3,
--        2,0,0,0,4,
--        5,0,0,0,2]

cost :: Graph ->Branch  -> Int

cost [] branch = 0

cost gr [] = 0

cost gr [b] = 0

cost gr branch = (gr !! ( head(tail branch) * numNodes + head branch )) + (cost gr (tail branch))         
    

-- | The getHr function reads the heuristic for a node from a given heuristic table.

-- The heuristic table gives the heuristic (in this case straight line distance) and has one entry per node. It is ordered by node (e.g. the heuristic for node 0 can be found at index 0 ..)  

getHr:: [Int]->Node->Int

getHr hrTable node = head[h| (n,h)<- zip xs hrTable, n == node] where xs = [0..length(hrTable)-1]


-- | A* Search

-- The aStarSearch function uses the checkArrival function to check whether a node is a destination position,

---- and a combination of the cost and heuristic functions to determine the order in which nodes are searched.

---- Nodes with a lower heuristic value should be searched before nodes with a higher heuristic value.

                                         

minSort:: Graph -> [Branch] -> [Int] -> [Branch]

minSort g [] hrTable = []

minSort [] branches hrTable = []

--sort the branches from min c+h to max c+h value 
minSort g branches hrTable = map snd (sortBy (\(a,_) (b,_) -> compare a b) tuples)

-- zip the branch(brch) and its cost+heuristic(f) value
                                      where tuples = [(f,brch)|(f,brch)<-zip fs branches]
                           
-- list the cost+heuristic for each branch of branches
                                             where fs = [(cost g brch) + (getHr hrTable (head(brch))) | brch<-branches]



------------- g     dst   (        next           )  (       getHr    ) hrTable (      cost        )  branches exploredList      
aStarSearch::Graph->Node->(Branch->Graph->[Branch])->([Int]->Node->Int)->[Int]->(Graph->Branch->Int)->[Branch]-> [Node]-> Maybe Branch

aStarSearch g destination next getHr hrTable cost [] exploredList = Just $ [2,333]

aStarSearch [] destination next getHr hrTable cost branches exploredList = Nothing

aStarSearch g destination next getHr hrTable cost branches exploredList

-- If the head branch reaches the destination then return this branch
        |checkArrival destination (head(head branches)) = Just $ (head branches)
        
-- If the head branch is already explored then 'delete' the head branch
        |head(head branches) `elem` exploredList = aStarSearch g destination next getHr hrTable cost (minSort g (tail branches) hrTable) exploredList
        
-- otherwise 1)Replace the first branch of branches to its child's branches (using 'next' function)
--           2)Move those branch(es) which has(have) minimum c+h value to the head of the branches(sortby)
--           3)Add the head node of the original first branch to exploredList
--           4)Search again
--        | otherwise = aStarSearch g destination next getHr hrTable cost (filter (not.null) (minSort g ((tail branches)++next (head branches) g) hrTable)) (exploredList++[head (head branches)])
        | otherwise = aStarSearch g destination next getHr hrTable cost (minSort g (filter (not.null) (concat[(tail branches), next (head branches) g])) hrTable) (concat[exploredList, [head (head (branches))]])


-- | Section 5: Games

-- See ConnectFourWithTwist.hs for more detail on  functions that might be helpful for your implementation. 







-- | Section 5.1 Connect Four with a Twist



 



-- The function determines the score of a terminal state, assigning it a value of +1, -1 or 0:

eval :: Game -> Int

eval game |terminal game && checkWin game maxPlayer = 1

          |terminal game && checkWin game minPlayer = (-1)
          
          |terminal game = 0



-- | The alphabeta function should return the minimax value using alphabeta pruning.

-- The eval function should be used to get the value of a terminal state. 

-- Because I really need two parameters (alpha) and (beta) but the signature of alphabeta function cannot be changed

--  So I attached a new function alphabeta2 which allows alpha beta as two parameters

alphabeta:: Role -> Game -> Int

alphabeta  player game = alphabeta2 player game (-2) 2    

    
alphabeta2:: Role -> Game ->Int ->Int ->Int

alphabeta2  player game a b

    |terminal game = eval game

-- The maxEval = -2 at the beginning instead of negativeinfinity because the score has lowest value -1   
    |player == maxPlayer = forEachMax (movesAndTurns game player) player a b (-2) 
    
-- The minEval = 2 at the beginning instead of infinity because the score has highest value 1    
    |otherwise = forEachMin (movesAndTurns game player) player a b 2

-- MAX-VALUE function    
forEachMax:: [Game] -> Role -> Int-> Int->Int ->Int

forEachMax [] player alpha beta maxEval = maxEval

forEachMax (g:games) player alpha beta maxEval 

    |beta <= v = v
                                                
    |otherwise = forEachMax games player (maximum[alpha, v]) beta v

                         where v = maximum[maxEval,eval] 
                            
                                    where eval = alphabeta2 (switch player) g alpha beta 
                                                    
                                                              

--MIN-VALUE function                                                     
forEachMin::[Game] -> Role -> Int-> Int ->Int ->Int

forEachMin [] player alpha beta minEval = minEval

forEachMin (g:games) player alpha beta minEval 
                                                    
      | minEval <= alpha = minEval
                                                
      | otherwise = forEachMin games player alpha (minimum[beta,v]) v
                         
                           where v = minimum[eval, minEval]
                           
                                     where eval = alphabeta2 (switch player) g alpha beta

                                           
            

-- | OPTIONAL!

-- You can try implementing this as a test for yourself or if you find alphabeta pruning too hard.

-- If you implement minimax instead of alphabeta, the maximum points you can get is 10% instead of 15%.

-- Note, we will only grade this function IF YOUR ALPHABETA FUNCTION IS EMPTY.

-- The minimax function should return the minimax value of the state (without alphabeta pruning).

-- The eval function should be used to get the value of a terminal state.

minimax:: Role -> Game -> Int

minimax player game

-- If game is over then return the mark
    |terminal game = eval game
    
-- If it is human player's turn, find out the highest score the player can get from the scores passing by its child's games    
    |player == maxPlayer = maximum[(minimax (switch player) child) | child <- (movesAndTurns game player)]
    
-- If it is computer player's turn, find out the lowest score the human player can get    
    |otherwise = minimum[(minimax (switch player) child) | child <- (movesAndTurns game player)]

{- Auxiliary Functions

-- Include any auxiliary functions you need for your algorithms below.

-- For each function, state its purpose and comment adequately.

-- Functions which increase the complexity of the algorithm will not get additional scores

-}


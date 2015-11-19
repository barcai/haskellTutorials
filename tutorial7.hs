-- Informatics 1 - Functional Programming 
-- Tutorial 7
--
-- Week 9 - Due: 19/20 Nov.


import LSystem
import Test.QuickCheck

-- Exercise 1

-- 1a. split
split :: Command -> [Command]
split Sit = []
split (Go d) = (Go d):[]
split (Turn a) = (Turn a):[]
split (com1 :#: com2) = (split com1) ++ (split com2)

-- 1b. join
join :: [Command] -> Command
join (command:[]) = command
join (command:list) = command :#: join list

-- 1c  equivalent
equivalent :: Command -> Command -> Bool
equivalent com1 com2 = (split com1) == (split com2)

-- 1d. testing join and split
prop_split_join :: Command -> Bool
prop_split_join command = equivalent (join (split command)) command

prop_split :: Command -> Bool
prop_split command = not (Sit `elem` (split command))


-- Exercise 2
-- 2a. copy
copy :: Int -> Command -> Command
copy 1 command = command
copy n command = command :#: (copy (n-1) command)

-- 2b. pentagon
pentagon :: Distance -> Command
pentagon d = copy 5 (Go d :#: Turn 72)

-- 2c. polygon
polygon :: Distance -> Int -> Command
polygon d n = copy n (Go d :#: Turn (360 / (fromIntegral n)))



-- Exercise 3
-- spiral
spiral :: Distance -> Int -> Distance -> Angle -> Command
spiral distance 1 _ angle    = (Go distance) :#: (Turn angle)
spiral distance n step angle = (Go distance) :#: (Turn angle) :#: spiral (distance + step) (n-1) step angle


-- Exercise 4
-- optimise
optimise :: Command -> Command
optimise command = join . opt $ split command
    where
    	opt (Go 0 : restOfList) = opt restOfList
        opt (Turn 0 : restOfList) = opt restOfList
        opt (Turn a : Turn b : restOfList) = opt (Turn (a+b) : restOfList)
        opt (Go a : Go b : restOfList) = opt (Go (a+b) : restOfList)
        opt (somethingElse : restOfList) = somethingElse : opt restOfList
        opt [] = []

optimiseRight :: Command -> Command
optimiseRight command 
    | equivalent (optimise command) (optimise (optimise command)) = optimise command
    | otherwise = optimise $ optimise command



-- L-Systems

-- 5. arrowhead
arrowhead :: Int -> Command
arrowhead x = f x
    where
    	f 0 = GrabPen red :#: Go 10
    	f i = g (i-1) :#: p :#: f (i-1) :#: p :#: g (i-1)
    	g 0 = GrabPen blue :#: Go 10
    	g i = f (i-1) :#: n :#: g (i-1) :#: n :#: f (i-1)
    	p = Turn 60
    	n = Turn (-60)

-- 6. snowflake
snowflake :: Int -> Command
snowflake = undefined

-- 7. hilbert
hilbert :: Int -> Command
hilbert = undefined


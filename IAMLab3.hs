--Ivan Malison
--Lab 3 - Math 121 - Jim Fix

module IAMLab3 where

--Not Assigned:

--helper function from Lab 2
removeAll :: Int->[Int]->[Int]
removeAll _ [] = []
removeAll x (a:as)
 |x == a = removeAll x as
 |otherwise = a: removeAll x as

--helper function from Lab 2
removeDuplicates :: [Int]->[Int]
removeDuplicates [] = []
removeDuplicates (a:as) = a: removeDuplicates (removeAll a as)

--Exercise 1:

--prefixsums returns a list of partial sums of a list of integers.  It takes the head of the list
--it recieves and appends it to prefixsums of (the head of the list plus the head of the tail of
--the list) prepended to the tail of the tail of the list. In simplified terms it makes the head of 
--the new list the appropriate by adding the first two terms of the list, so that as prefixsums
--removes the head of the list recursively, the appropriate sum terms are produced.
prefixsums :: [Int]->[Int]
prefixsums [] = []
prefixsums (x:y:zs) = x:prefixsums((x+y):(zs))
prefixsums (a:_) = [a]

--suffixsums returns a list of partial sums in reverse direction.  It sends the list it recieves to
--the function sumList, which sums the terms of the list, and then runs suffixsums on the tail of
--the list until it recieves the empty set.
suffixsums :: [Int]->[Int]
suffixsums [] = []
suffixsums (x:xs) = sumList (x:xs):suffixsums (xs)

--sumList returns a sum of all the terms in a list.  It adds the head of the list to sumList of the
--tail until it recieves the empty set (it sums the list recursively).
sumList :: [Int]->Int
sumList [] = 0
sumList (x:xs) = x + sumList (xs)

--Exercise 2:

--isMemberOf determines whether a given integer is in a list of integers.  It checks to see if the
--integer is equal to the head of the list, and if it is it returns true.  If not it runs
--isMemberOf on the tail of the list until it recieves the empty set.
isMemberOf :: Int->[Int]->Bool
isMemberOf _ [] = False
isMemberOf a (x:xs)
 |a == x = True
 |otherwise = isMemberOf a xs

--intersect runs isMemberOf on each of the items in a list recursively against another list,
--prepending the ones which come back true to each other until it recieves the empty set.
intersect :: [Int]->[Int]->[Int]
intersect [] _ = []
intersect (a:as) b
 |isMemberOf a b == True = a:intersect (removeAll a as) (removeAll a b)
 |otherwise = intersect as b

--minus removes all the terms from one set from another using the removeAll function.
minus :: [Int]->[Int]->[Int]
minus [] _ = []
minus (a:as) b
 |isMemberOf a b == True = minus (removeAll a as) b
 |otherwise = a: minus (removeAll a as) b

--union adds prepends all the terms in one set to another and removes duplicates.
union :: [Int]->[Int]->[Int]
union [] b = removeDuplicates b
union (a:as) b = a:union (removeAll a as) (removeAll a b)

--Exercise 3:

--cross gives all possible list containing a first term from one list and a second term from
--another.
cross :: [x]->[x]->[[x]]
cross as bs = crossh as as bs

--crossh is the helper function for cross.  It appends every item in the first list to the head of
--the second by chopping off the head of the first list and running itself recursively.  This is
--why the first set is stored in memory twice when the information is passed to crossh from cross.
--when crossh reaches the null set in the first list it replaces it with the other full version of
--the list stored as the letter z and then runs crossh on the tail of the second list, which allows
--crossh to eventually reach every term in b.
crossh :: [x]->[x]->[x]->[[x]]
crossh [] z b = crossh z z (tail b)
crossh _ _ [] = []
crossh (a:as) z b = [a,head b]:(crossh as z b)

--Not Assigned

--listsoflists is a useful helper function that takes a list and makes it into a list
--of lists with each term occupying its own list.
listoflists :: [Int]->[[Int]]
listoflists [] = []
listoflists (l:ls) = [l]:(listoflists ls)

--Exercise 4:

--binSeq2 is a solution for exercise 4 through the anySeq (description in Exercise 4 Bonus function.
--For a standalone function see below.
binSeq2 :: Int->[[Int]]
binSeq2 a = anySeq a [0,1]

--binSeq n returns the exact same thing as anySeq n [0,1], and it functions in pretty much the
--exact same way.  See anySeq for a detailed explanation
binSeq :: Int->[[Int]]
binSeq 0 = []
binSeq n = binSeqCounter n [[0],[1]]

binSeqCounter :: Int->[[Int]]->[[Int]]
binSeqCounter n a
 |n > 1 = binSeqCounter (n-1) (binSeqh a)
 |otherwise = a

binSeqh :: [[Int]]->[[Int]]
binSeqh [] = []
binSeqh (a:as) = (binSeqh2 a)++(binSeqh as)

binSeqh2 :: [Int]->[[Int]]
binSeqh2 a = [(0:a),(1:a)]

--Exercise 4 Bonus:

--anySeq is a generalized version of binSeq.  Given an integer n and a list, it returns every
--possible sequence of length n made up of elements from the list.
anySeq :: Int->[Int]->[[Int]]
anySeq 0 _ = []
anySeq n l = anySeqCounter n (listoflists l) l

--(read anySeqh first) anySeqCounter's job is to run anySeqh n times so that the lists it produces
--are of length n.
anySeqCounter :: Int->[[Int]]->[Int]->[[Int]]
anySeqCounter n a l
 |n > 1 = anySeqCounter (n-1) (anySeqh a l) l
 |otherwise = a

--anySeqh and and anySeqh2 take a list of lists and a list and append every item in the list to
--each list in the list of lists.  Specifically, anySeqh takes the head of the list of
--lists and sends it to anySeqh2, and appends that to anySeqh of the tail of the list, ensuring that
--anySeqh2 is run on every list in the list of lists.
anySeqh :: [[Int]]->[Int]->[[Int]]
anySeqh [] _ = []
anySeqh (a:as) l = (anySeqh2 a l)++(anySeqh as l)

--anySeqh2 takes the head of the lists of lists in anySeqh and prepends every item in the list from
--anySeqh recursively.
anySeqh2 :: [Int]->[Int]->[[Int]]
anySeqh2 _ [] = []
anySeqh2 a (l:ls) = (l:a):anySeqh2 a ls

--Exercise 5:

--subsoflength2 takes a set (list) and returns all possible subsets of cardinality 2 of that list.
--subsoflength2 applies makesallbegw to each item in s (listoflists s). see makesallbegw.
subsoflength2 :: [Int]->[[Int]]
subsoflength2 [] = []
subsoflength2 s = makesallbegw (head (listoflists s)) s ++ subsoflength2 (tail s)

--makesallbegw is the function that does the lionshare of the work for allSubs and subsoflength2.
--Given 2 lists, a (which is treated as a set and has length n) and s, makesallbegw makes all the
--sets beggening with a with one additional element from s that is not already in a.  makesallbegw
--accoplishes this by testing whether or not head s is in a.  If it is then it runs makesallbegw
--on the tail of s.  If it is it appends a and the list [head s] and does the same thing.
--The function stops when the empty set is reached.
makesallbegw :: [Int]->[Int]->[[Int]]
makesallbegw _ [] = []
makesallbegw a s
 |isMemberOf (head s) a == True = makesallbegw a (tail s)
 |otherwise = (a ++ [head s]):makesallbegw a (tail s)

--Exercise 5 Bonus:

--allSubs takes a set (list) and returns all possible subsets of that list.  It uses a number of
--helper functions whose mechanics are described below.  The mechanics of this function are very
--difficult to describe since there is a huge amount of recursion involved.  This makes sense
--since allSubs must make 2^n lists to finish its task (where n is the number of items in the list
--it receives).  allSubs prepends the empty list because the helper fuctions do not make the empty
--list, but the empty list is a subset of every set.  It may be helpful to read about makesallbegw
--before reading about the other helper functions to better understand allSubs.
allSubs :: [Int]->[[Int]]
allSubs a = []: headtaker a [[]]

--headtaker gets its name because it takes the head of the list of lists and passes it on to
--makesallbegw.  This probably sounds confusing because it is hard to tell what the lists of lists
--is.  If you look above to the definition of allSubs you will notice that the list of lists starts
--out as the list that contains only the empty list.  From there, the list of list becomes what
--makesallbegw returns.  it should be noted that headtaker passes the tail of the list of lists on
--to tailtaker, as well as the head of the list of lists, but in separate peices.
headtaker :: [Int]->[[Int]]->[[Int]]
headtaker [] _ = []
headtaker _ [] = []
headtaker s l = makesallbegw (head l) s ++ headtaker s (makesallbegw (head l) s) ++ tailtaker s (head l) (tail l)

--tailtaker was the last helper function of allSubs to be built, and the solution to a tricky
--problem.  Before tailtaker was implemented, headtaker was simply reiterated recursively (headtaker
--s (tails) appeared in place of tailtaker in the headtaker definition).  This version of allSubs
--successfully produced all the subsets of s, but with some duplicates (different orders, same set).
--Despite its name, tailtaker actualy takes both the head and the tail of the list of lists from
--headtaker.  tailtaker essentially just passes the list of lists it recieves from headtaker right
--back to headtaker.  The only subtlety of tailtaker is that it removes all the items that appear
--in the head from the set s that is originally inputed with allSubs.  This works because all the
--sets that include the items in head l have already been made with the makesallbegw and headtaker
--functions.  To understand why this works it helps to think about which subsets including the nth
--term have been made.
tailtaker :: [Int]->[Int]->[[Int]]->[[Int]]
tailtaker s h l = headtaker (minus s h) l

--Exercise 5 extensions:

--subsoflength returns all the subsets of integer length n of a set of integers.  It is a
--generalized form of subsoflength2, but it uses the allSubs function.
subsoflength :: Int->[Int]->[[Int]]
subsoflength n a = solnh n (allSubs a)

--solnh checks to see if the head of (allSubs a) is a list of length n prepending it if it is and
--ignoring it if it is not.  It runs recursively on all the items in allSubs a.
solnh :: Int->[[Int]]->[[Int]]
solnh _ [] = []
solnh n (a:as)
 |length a == n = a:(solnh n as)
 |otherwise = solnh n as

--lsoln is a function that was written to discern a pattern in the number of subsets of length n
--a set of length x produces.  Given a set of integers, it returns a list whose kth term is the
--number of subsets of length k of that set (where k starts at 0).  For example, since the set 
--[1,2,3] has the subsets [[],[1],[2],[3],[1,2],[1,3],[1,2,3],[2,3]] lsoln returns [1,3,3,1] because
--[1,2,3] has 1 subset of length 0, 3 subsets of length 1, 3 subsets of length 2, and one subset of
--length 3.  For an explanation of how this is accomplished see clsoln.
lsoln :: [Int]->[Int]
lsoln a = clsoln 0 a

--given a set lsoln->clsoln runs subsoflength n for every n between 0 and (length of the set), 
--prepending the items to each other.
clsoln :: Int->[Int]->[Int]
clsoln n a
 |n <= length a = length(subsoflength n a):clsoln (n+1) a
 |otherwise = []

module Party where

import Data.List
import Data.Monoid
import Data.Tree
import Employee
import System.IO

--1
glCons :: Employee -> GuestList -> GuestList
glCons employee (GL employees fun) = GL (employee:employees) (fun + empFun employee)

testGuestList :: GuestList
testGuestList = GL [(Emp "Stan" 9), (Emp "Bob" 3), (Emp "Joe" 5)] 17

testGuestList2 :: GuestList
testGuestList2 = GL [(Emp "Harriet" 2), (Emp "Jean" 3), (Emp "Laura" 4)] 9

glAppend ::  GuestList -> GuestList -> GuestList
glAppend (GL employees1 fun1) (GL employees2 fun2) = GL (employees1 ++ employees2) (fun1 + fun2)

instance Monoid GuestList where
	mempty = GL [] 0
	mappend = glAppend

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL employees1 fun1) gl2@(GL employees2 fun2)
	| (fun1 < fun2) = gl2
	| otherwise = gl1

--2
treeFold :: (a -> b -> b) -> b -> Tree a -> b
treeFold f e (Node a []) = f a e
treeFold f e (Node a treeList) = f a (foldl (treeFold f) e treeList)

--3
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel employee [] = ((GL [employee] (empFun employee)), GL [] 0)
nextLevel employee guestlist = ((GL [employee] (empFun employee)) <> (withBoss guestlist), noBoss guestlist)

noBoss :: [(GuestList, GuestList)] -> GuestList
noBoss [] = mempty
noBoss ((list1, list2):guestlist) = (moreFun list1 list2) <> (noBoss guestlist)

withBoss :: [(GuestList, GuestList)] -> GuestList
withBoss [] = mempty
withBoss ((list1, list2):guestlist) = list2 <> (withBoss guestlist)

--4
maxFun :: Tree Employee -> GuestList
maxFun employeetree = uncurry moreFun (descendNext employeetree)

descendNext :: Tree Employee -> (GuestList, GuestList)
descendNext (Node employee guestlist) = nextLevel employee (map descendNext guestlist)

formatList :: GuestList -> [String]
formatList (GL employeelist fun) = ("Total fun: " ++ (show fun)) : sort(map empName employeelist)

--5
main :: IO ()
main = do
	file <- readFile "company.txt"
	mapM_ putStrLn ( formatList ( maxFun (read file)))

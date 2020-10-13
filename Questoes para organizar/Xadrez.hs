import Data.List

type Pos = (Int,Int)

ataca::Pos->Pos->Bool
ataca (x,y) (z,w) |x==z = True
	|y==w = True
	|x-y == z-w = True
	|x+y == z+w = True

posOk::Pos->[Pos]->Bool
posOk x [] = True
posOk x (y:ys) = not (ataca x y) && posOk x ys

safe::[Pos]->Bool
safe [] = True
safe (x:xs) = posOk x xs && safe xs

rainha n = [zip xs [1..n] | xs <-permutations[1..n], safe (zip xs [1..n])]

intercala x [] = [[x]]
intercala x (y:ys) = (x:y:ys) : [y z|z<-intercala x ys]
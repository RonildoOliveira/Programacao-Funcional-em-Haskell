--
partes [] = [[]]
partes (x:xs) = partes xs ++ map (x) partes xs
--
{-7-}
data Mobile = Pendente Int | Barra Mobile Mobile

peso::Mobile->Int
peso (Pendente p) = p
peso (Barra m1 m2) = peso m1 + peso m2

balanceado::Mobile->Bool
balanceado (Pendente p) = True
balanceado (Barra m1 m2) = if peso m1 == peso m2 then True else False

{-8-}
makeBMobile::[Int]->Maybe Mobile


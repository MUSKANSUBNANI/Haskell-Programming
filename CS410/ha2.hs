classify :: Integer -> String
classify n
        | n<1="Illegal"
        | aliquote <n ="Deficient"
        | aliquote>n ="Abundant"
        | otherwise="Perfect"
    where 
        aliquote =sum factors
        factors =[x|x <- [1..(n-1)], n `mod` x==0]

perms :: (Eq m)=>[m]->[m]->Bool
perms lst1 lst2
        | checklst1==[] && checklst2==[] =True
        | otherwise =False
        where
            checklst1=[x|x <-lst1, cnt x lst1/=cnt x lst2]
            checklst2=[x|x <-lst2, cnt x lst1/=cnt x lst2]
            
cnt ::  (Eq m)=>m->[m]->Integer
cnt x lst = sum[1|n <-lst, n==x]
        
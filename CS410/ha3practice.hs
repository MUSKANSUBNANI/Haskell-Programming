-- binInt :: String -> Integer
-- binInt ""=0
-- binInt "0"=0
-- binInt "1"=1
-- binInt s=2 * binInt (init s) +binInt ([last s])

-- addTwos ::[Double]-> [Double]
-- addTwos []=[]
-- addTwos [d]=[]
-- addTwos (d:e:tail)=(d+e):(addTwos tail)

-- quicksort :: (Ord a) => [a] -> [a]  
-- quicksort [] = []  
-- quicksort (x:xs) =   
--     let smallerSorted =  [a | a <- xs, a <= x]  
--         biggerSorted =  [a | a <- xs, a > x]  
--     in  quicksort smallerSorted ++ [x] ++ quicksort biggerSorted

-- mybutlast::[a]->a
-- mybutlast[]=error "No elements"
-- mybutlast[x]=error "few elements"
-- mybutlast (x:y:[])=x
-- mybutlast (x:xs)=mybutlast xs


-- isPalindrome :: Eq a=>[a]->Bool
-- isPalindrome[]=error "empty hai"
-- isPalindrome (x:xs)= let newlist= reverse (x:xs) in newlist==(x:xs)


-- compress :: Eq a=>[a]->[a]
-- compress []=[]
-- compress [x]=[x]
-- compress (x:ys@(y:_))
--     |x==y=compress ys
--     |otherwise = x:compress (ys)

-- -- split ::[a]->Int->([a],[a])
-- -- split []_=([],[])
-- -- split l@(x:xs) n
-- --             | n>0 =x: split xs n-1
-- --             | otherwise =([],l)
-- -- split :: [a] -> Int -> ([a], [a])
-- -- split []         _             = ([], [])
-- -- split l@(x : xs) n | n > 0     = x: split xs n-1
-- --                    | otherwise = ([], l)
-- --    -- where (ys,zs) = split xs (n - 1)

-- -- slice ::[a]->Int->Int-> [a]
-- -- slice [] _ _ =[]
-- -- slice (x:xs) m n
-- --             |m>1 =slice xs (m-1) (n-1)
-- --             |(m==1)&& (n>=1) =x:slice xs m (n-1)
-- --             |otherwise= []


-- -- InsertAt :: a -> [a] -> Int -> [a]
-- -- InsertAt y (x:xs)     1= y:x:xs
-- -- InsertAt y (x:xs) n= x:InsertAt y xs (n-1)


-- -- insertAt :: a -> [a] -> Int -> [a]
-- -- insertAt x ys     1 = x:ys
-- -- insertAt x (y:ys) n = y:insertAt x ys (n-1)

range:: Int -> Int -> [Int]
range m n
    | m<n = m:(range (m+1) n)  
    | m>n = m:(range (m-1) n) 
    | otherwise =[m]

-- binInt ::String->Integer
-- binInt ""=0
-- binInt xs = bI(read xs +0) (length xs)
--         where bI y m
--                 |m>=0= ((y `mod`(10^m))*2^m) +
--                 |



main :: IO()
main=do
      putStr "Text: "
      line<-getline
      if line==[]
        then return ()
        else do
          print $ fn (line)
          main

fn x = totalCountVowel(filterPalindromes(upperCaseLst(s2w(x) ) ) )
s2w :: String -> [String]
s2w [] = []
s2w phrase
  | null next = s2w rest
  | otherwise = next:(s2w rest)
  where (next, rest) = phrase `untilElement` separator
        separator = ' '

untilElement :: (Eq a) => [a] -> a -> ([a], [a])
[] `untilElement` _ = ([], [])
(x:xs) `untilElement` separator
  | x == separator = ([], xs)
  | otherwise      = let (xs', xs'') = xs `untilElement` separator in (x:xs', xs'')

upperCaseLst :: [String]->[String]
upperCaseLst []=[]
upperCaseLst (x:xs)= h: upperCaseLst xs
                where h = upperCase' x

upperCase':: String ->String
upperCase' []=[]
upperCase' (x:xs) = c : upperCase' xs
            where c = getUppercase x
    
getUppercase :: Char-> Char
getUppercase x 
            |j== True = x
            |otherwise = fst' k
        where j= (null [n|n<-zip' ['A'.. 'Z']['a'..'z'], x == fst' n || x==snd' n])
              k=(head' [n|n<-zip' ['A'.. 'Z']['a'..'z'], x == fst' n || x==snd' n])


isPalindrome :: String->Bool
isPalindrome[]=error "empty"
isPalindrome (x:xs)= let newlist= reverse (x:xs) in newlist==(x:xs)

filterPalindromes:: [String]->[String]
filterPalindromes []=[]
filterPalindromes (x:xs)
                    |  m== True = x:filterPalindromes xs
                    | otherwise = filterPalindromes xs
                        where m = isPalindrome x


totalCountVowel :: [String]-> Int
totalCountVowel []= 0
totalCountVowel (x:xs)= sum + totalCountVowel xs
            where sum = countVowel x 


countVowel :: String -> Int
countVowel []=0
countVowel (x:xs)
            |m==True = 0+ countVowel xs
            | otherwise= 1+ countVowel xs
                where m= let  zippedList = zip' ['A','E','I','O','U']['a', 'e', 'i', 'o','u'];vowelsLst = [j|j <- zippedList, not (fst' j == x), not (snd' j==x)]
                              in zippedList ==vowelsLst
                    -- where  zippedList = zip ['A','E','I','O','U']['a', 'e', 'i', 'o','u'] 
                    --         vowelsLst = [j|j <- zippedList, fst j /= x, snd j/=x]


zip' :: [a]->[b]->[(a,b)]
zip' _[]=[]
zip' []_=[]
zip'(x:xs)(y:ys)= (x,y): zip' xs ys

fst' :: (a,b)-> a
fst' (x,_)=x

snd' :: (a,b)->b
snd' (_,y)=y

head' :: [a]->a
head' []=error "No elements in the list"
head' (x:_)=x

tail' :: [a]-> [a]
tail' []=error "No elements in the list"
tail (_:xs)=xs
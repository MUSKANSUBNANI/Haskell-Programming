main :: IO()
main=do
      putStr "Text: "
      line<-getLine
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
isPalindrome (x:xs)= let newlist= reverse'(x:xs) in newlist==(x:xs)

reverse' :: [a]->[a]
reverse' []=[]
reverse' (x:xs)= reverse' xs ++ [x]


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
module Lib
    (
        myIsNeg,
        myAbs,
        myMin,
        myMax,
        myTuple,
        myTruple,
        myFst,
        mySnd,
        mySwap,
        myHead,
        myTail,
        myLength,
        myNth,
        myTake,
        myAppend,
        myReverse,
        myInit,
        myLast,
        myZip
    ) where

myIsNeg :: Int -> Bool
myIsNeg b = b < 0

myAbs :: Int -> Int
myAbs c = if c < 0 then c * (-1) else c

myMin :: Int -> Int -> Int
myMin a b = if a <= b then a else b

myMax :: Int -> Int -> Int
myMax a b = if a <= b then b else a

myTuple :: a -> b -> (a , b )
myTuple a b = (a, b)

myTruple :: a -> b -> c -> (a , b , c )
myTruple a b c = (a, b, c)

myFst :: (a , b ) -> a
myFst (a, b) = a

mySnd :: (a , b ) -> b
mySnd (a, b) = b

mySwap :: (a , b ) -> (b , a )
mySwap (a, b) = (b, a)

myHead :: [ a ] -> a
myHead (a:_) = a
myHead [] = error "empty list"

myTail :: [ a ] -> [ a ]
myTail (_:a) = a
myTail [] = error "empty list"

myLength :: [ a ] -> Int
myLength list = countElem list 0

countElem :: [ list ] -> Int -> Int
countElem (_:list) n = countElem list (n + 1)
countElem [] n = n

myNth :: [ a ] -> Int -> a
myNth [] n = error "empty list"
myNth (la:_) 0 = la 
myNth (_:lb) n | myLength lb < n = error "empty list"
               | n < 0 = error "empty list"
               | otherwise = myNth lb (n-1)

myTake :: Int -> [ a ] -> [ a ]
myTake n [] = []
myTake 0 list = []
myTake n (a:b) = a : myTake (n - 1) b

-- myDrop :: Int -> [ a ] -> [ a ]
-- myDrop n list | n == 0 = list
            --   | n < 0 = error "empty list"
            --   | myLength list - 1 < n = []
-- myDrop n (_:list) = myDrop (n - 1) list

myAppend :: [ a ] -> [ a ] -> [ a ]
myAppend la [] = la
myAppend [] lb = lb
myAppend (a:b) l = a : myAppend b l

myReverse :: [ a ] -> [ a ]
myReverse l = rev l []
    where
        rev [] a = a
        rev (x:xs) a = rev xs (x:a)

myInit :: [ a ] -> [ a ]
myInit [] = error "empty list"
myInit l = myTake (myLength l - 1) l

myLast :: [ a ] -> a
myLast [] = error "empty list"
myLast l = myNth l (myLength l - 1)

myZip :: [ a ] -> [ b ] -> [( a , b ) ]
myZip a [] = []
myZip [] b = []
myZip (a:as) (b:bs) = (a, b) : myZip as bs

myUnzip [] = ([], [])
myUnzip ((a, b):xs) = let (as, bs) = myUnzip xs in (a : as, b : bs)

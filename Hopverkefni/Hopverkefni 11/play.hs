{-# LANGUAGE TemplateHaskell #-}

revHS x = foldl (\x y -> y : x) [] x

---Fyrra einstaklingsverkefni
listmaker n m = [n..m]
square x = x * x
listAll f n m = (map f ([n..m]))

---H1
-- Notkun:	reverseList x
-- Fyrir:	x er listi [x1,x2,..,xN].
-- Gildi:  	skilar listanum snúnum: [xN,..,x2,x1].
reverseList x = foldl (\x y -> y:x) [] x


y = [[1,2,3],[10,9,8],[5,5,5]]


---H2
-- Notkun:	multList x
-- Fyrir:	x er listi lista af fleytitölum:
-- 			[[x1,x2,..,xN],[y1,..,yN],..,[z1,..,zN]].
-- Gildi:  	skilar summunni af margfeldi innri listanna.
multList x = sum $ map product x


multiplicity x = (foldl (*) 1 x)
multiplicityList x = (map multiplicity x)
summer x = foldl (+) 0 x
snorriSTFU x = (summer (multiplicityList x))


main = do 
print "Test fyrir fyrra einstaklingsverkefni"
print (listmaker 5 10) 
print (square 5) 
print (listAll square 1 10)
print (listAll (\x -> x*x)  1 10)

print "Test fyrir fyrra hopverkefni"
print (reverseList (listmaker 1 10))


print "Test fyrir seinna hopverkefni lausn 1"
print (summer (listmaker 1 10))
print (multiplicity [1,2,3])
print (multiplicityList y)
print (snorriSTFU y)

print "Test fyrir H1"
print (revHS [1..10])

print "Test fyrir H2"
print (multList [[1..5], [1..4], [2..3]])
print (product [1..5])
print (product [1..4])
print (product [2..3])

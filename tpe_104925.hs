--exercice 1.1 
pgcd::(Int,Int)->Int 
pgcd (a,b) 
         |a == b = a 
         |a>b =pgcd(a-b,b) 
         |a<b =pgcd(b,b-a)



--exercice 1.2

ppcm::(Int,Int)->Int 
ppcm (a,b)=(a*b) `div` pgcd(a,b) 



--exercice 1.3

somImpair::Int->Int 
somImpair 1=1 
somImpair n=somImpair(n-1)+2*(n-1)+1 



--exercice 1.4

reste_Quotient::Int->Int->(Int,Int) 
reste_Quotient a b= (mod a b ,div a b) 



--exercice 1.5

myMax :: Int -> Int -> Int
myMax a b 
        | a <= b = b
        |otherwise = a

myMin :: Int -> Int -> Int
myMin a b 
         | a <= b = a
         |otherwise = b



--exercice 1.6

min_max :: Int -> Int -> Int -> Int -> Int
min_max a b c d = myMin a (myMin b (myMin c d))



--exercice 1.7

borneDans :: Int -> Int -> Int -> Int
borneDans a b c 
               |myMin a b < c && c < myMax a b = c
               |c >= myMax a b = myMax a b
               |c < myMin a b = myMin a b 



--exercice 1.8

somChif :: Int -> Int
somChif x
        |x < 0 = error"le nombre entrer  doit etre positif" 
        |x < 10 = x
        |otherwise = (x `mod`10) + (x `div` 10)


--exercice 1.10

type Point=(Double,Double) 
distance::Point->Point->Double 
distance (x1,y1)(x2,y2) =(sqrt((x2-x1)^2 + (y2-y1))^2) 



--exercice 1.11

suite::Int->Int 
suite n 
       |n==0 = (-2) 
       |n>0 = 3+ 4*suite(n-1) 



--exercice 1.12

sn::Integer->Double 
sn n 
    |n==1=1 
    |n>1=sn(n-1)+(1/fromInteger(n)) 



--exercice 3

data Jours=Lundi|Mardi|Mercredi|Jeudi|Vendredi|Samedi|Dimanche 
weekend:: Jours->Bool 
weekend Samedi=True  
weekend Dimanche =True 
weekend  _ =False

numJours::Jours->Int 
numJours Lundi=1 
numJours Mardi=2 
numJours Mercredi=3 
numJours Jeudi=4 
numJours Vendredi=5 
numJours Samedi=6 
numJours Dimanche=7 



--exercice 4

type Couple=(Integer,Integer) 
produitScalaire::Couple->Couple->Integer 
produitScalaire x y=(fst(x)*fst(y))+(snd(x)*snd(y)) 
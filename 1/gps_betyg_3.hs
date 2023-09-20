import Data.List

-- Generic part
type StatePair = (State,State)

run :: [State]
run = gps [(startState,startParent)] []

gps :: [StatePair]->[StatePair]->[State]
gps [] _ = error "No solution"
gps (s:restOfOpen) closed 
    |   isGoal (fst s) = reverse(retrieveSolution (fst s) (s:closed))  
    |   otherwise = gps newOpen (s:closed) 
            where newOpen = restOfOpen ++ zip newStates (repeat (fst s))
                  newStates = filter (`notElem` map fst (restOfOpen ++ closed)) (makeMoves (fst s))
             

retrieveSolution :: State -> [StatePair] -> [State]
retrieveSolution s closed = 
    let parent = findParent s closed 
    in if parent == startParent then [s] else s: retrieveSolution parent closed
            where findParent y xs = snd . head $ filter ((== y) . fst) xs
           


-- Problem specific
type State = (Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int)

startState :: State
startState = (0,1,1,1,1,1,1,1,1,1,1,1,1,1,1)

startParent :: State
startParent = (-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1)

isGoal :: State -> Bool
isGoal (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) = a == 1 && b == 0 && c == 0 && d == 0 && e == 0 && f == 0 && g == 0 && h == 0 && i == j && k == 0 && l == 0 &&  m == 0 && n == 0 && o == 0

makeMoves :: State -> [State]
makeMoves state = filter (/= state) (map ($ state) operators)

operators :: [State->State]
operators = [a,ai,b,bi,c,ci,d,di,e,ei,f,fi,g,gi,h,hi,i,ii,j,ji,k,ki,l,li,m,mi,n,ni,o,oi,p,pinv,q,qi,r,ri]

a :: State->State
a (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15) = if p4 == 1 && p2 == 1 && p1 == 0 then (1,0,p3,0,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15) else (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15);

b :: State->State
b (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15) = if p7 == 1 && p4 == 1 && p2 == 0 then (p1,1,p3,0,p5,p6,0,p8,p9,p10,p11,p12,p13,p14,p15) else (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15);

c :: State->State
c (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15) = if p11 == 1 && p7 == 1 && p4 == 0 then (p1,p2,p3,1,p5,p6,0,p8,p9,p10,0,p12,p13,p14,p15) else (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15);

ai :: State->State
ai (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15) = if p4 == 0 && p2 == 1 && p1 == 1 then (0,0,p3,1,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15) else (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15);

bi :: State->State
bi (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15) = if p7 == 0 && p4 == 1 &&  p2 == 1 then (p1,0,p3,0,p5,p6,1,p8,p9,p10,p11,p12,p13,p14,p15) else (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15);

ci :: State->State
ci (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15) = if p11 == 0 && p7 == 1 && p4 == 1 then (p1,p2,p3,0,p5,p6,0,p8,p9,p10,1,p12,p13,p14,p15) else (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15);



d :: State->State
d (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15) = if p3 == 1 && p5 == 1 && p8 == 0 then (p1,p2,0,p4,0,p6,p7,1,p9,p10,p11,p12,p13,p14,p15) else (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15);

e :: State->State
e (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15) = if p5 == 1 && p8 == 1 && p12 == 0 then (p1,p2,p3,p4,0,p6,p7,0,p9,p10,p11,1,p13,p14,p15) else (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15);

f :: State->State
f (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15) = if p6 == 1 && p9 == 1 && p13 == 0 then (p1,p2,p3,p4,p5,0,p7,p8,0,p10,p11,p12,1,p14,p15) else (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15);

di :: State->State
di (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15) = if p3 == 0 && p5 == 1 && p8 == 1 then (p1,p2,1,p4,0,p6,p7,0,p9,p10,p11,p12,p13,p14,p15) else (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15);

ei :: State->State
ei (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15) = if p5 == 0 && p8 == 1 && p12 == 1 then (p1,p2,p3,p4,1,p6,p7,0,p9,p10,p11,0,p13,p14,p15) else (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15);

fi :: State->State
fi (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15) = if p6 == 0 && p9 == 1 && p13 == 1 then (p1,p2,p3,p4,p5,1,p7,p8,0,p10,p11,p12,0,p14,p15) else (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15);




g :: State->State
g (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15) = if p6 == 1 && p3 == 1 && p1 == 0 then (1,p2,0,p4,p5,0,p7,p8,p9,p10,p11,p12,p13,p14,p15) else (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15);

h :: State->State
h (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15) = if p10 == 1 && p6 == 1 && p3 == 0 then (p1,p2,1,p4,p5,0,p7,p8,p9,0,p11,p12,p13,p14,p15) else (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15);

i :: State->State
i (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15) = if p15 == 1 && p10 == 1 && p6 == 0 then (p1,p2,p3,p4,p5,1,p7,p8,p9,0,p11,p12,p13,p14,0) else (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15);

gi :: State->State
gi (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15) = if p6 == 0 && p13 == 1 && p1 == 1 then (0,p2,0,p4,p5,1,p7,p8,p9,p10,p11,p12,p13,p14,p15) else (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15);

hi :: State->State
hi (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15) = if p10 == 0 && p6 == 1 && p3 == 1 then (p1,p2,0,p4,p5,0,p7,p8,p9,1,p11,p12,p13,p14,p15) else (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15);

ii :: State->State
ii (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15) = if p15 == 0 && p10 == 1 && p6 == 1 then (p1,p2,p3,p4,p5,0,p7,p8,p9,0,p11,p12,p13,p14,1) else (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15);



j :: State->State
j (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15) = if p9 == 0 && p5 == 1 && p2 == 1 then (p1,0,p3,p4,0,p6,p7,p8,1,p10,p11,p12,p13,p14,p15) else (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15);

k :: State->State
k (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15) = if p14 == 0 && p9 == 1 && p5 == 1 then (p1,p2,p3,p4,0,p6,p7,p8,0,p10,p11,p12,p13,1,p15) else (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15);

l :: State->State
l (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15) = if p13 == 0 && p8 == 1 && p4 == 1 then (p1,p2,p3,0,p5,p6,p7,0,p9,p10,p11,p12,1,p14,p15) else (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15);

ji :: State->State
ji (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15) = if p9 == 1 && p5 == 1 && p2 == 0 then (p1,1,p3,p4,0,p6,p7,p8,0,p10,p11,p12,p13,p14,p15) else (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15);

ki :: State->State
ki (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15) = if p14 == 1 && p9 == 1 && p5 == 0 then (p1,p2,p3,p4,1,p6,p7,p8,0,p10,p11,p12,p13,0,p15) else (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15);

li :: State->State
li (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15) = if p13 == 1 && p8 == 1 && p4 == 0 then (p1,p2,p3,1,p5,p6,p7,0,p9,p10,p11,p12,0,p14,p15) else (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15);



m :: State->State
m (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15) = if p6 == 0 && p5 == 1 && p4 == 1 then (p1,p2,p3,0,0,1,p7,p8,p9,p10,p11,p12,p13,p14,p15) else (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15);

n :: State->State
n (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15) = if p9 == 0 && p8 == 1 && p7 == 1 then (p1,p2,p3,p4,p5,p6,0,0,1,p10,p11,p12,p13,p14,p15) else (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15);

o :: State->State
o (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15) = if p10 == 0 && p9 == 1 && p8 == 1 then (p1,p2,p3,p4,p5,p6,p7,0,0,1,p11,p12,p13,p14,p15) else (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15);

mi :: State->State
mi (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15) = if p6 == 1 && p5 == 1 && p4 == 0 then (p1,p2,p3,1,0,0,p7,p8,p9,p10,p11,p12,p13,p14,p15) else (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15);

ni :: State->State
ni (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15) = if p9 == 1 && p8 == 1 && p7 == 0 then (p1,p2,p3,p4,p5,p6,1,0,0,p10,p11,p12,p13,p14,p15) else (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15);

oi :: State->State
oi (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15) = if p10 == 1 && p9 == 1 && p8 == 0 then (p1,p2,p3,p4,p5,p6,p7,1,0,0,p11,p12,p13,p14,p15) else (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15);




p :: State->State
p (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15) = if p13 == 0 && p12 == 1 && p11 == 1 then (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,0,0,1,p14,p15) else (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15);

q :: State->State
q (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15) = if p14 == 0 && p13 == 1 && p12 == 1 then (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,0,0,1,p15) else (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15);

r :: State->State
r (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15) = if p15 == 0 && p14 == 1 && p13 == 1 then (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,0,0,1) else (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15);

pinv :: State->State
pinv (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15) = if p13 == 1 && p12 == 1 && p11 == 0 then (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,1,0,0,p14,p15) else (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15);

qi :: State->State
qi (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15) = if p14 == 1 && p13 == 1 && p12 == 0 then (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,1,0,0,p15) else (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15);

ri :: State->State
ri (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15) = if p15 == 1 && p14 == 1 && p13 == 0 then (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,1,0,0) else (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15);




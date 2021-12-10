module Main where
import qualified Control.Applicative as Applicative
import Control.Monad
import Data.Char

data Op =  Constant Int
           | Variable String
           | Minus Op Op
           | Plus Op Op
           | Div Op Op
           | Greater Op Op
           | Less Op Op
           | Equal Op Op
           | Times Op Op
           deriving Show

data ByteCode =  Assign String Op
           | Seq ByteCode ByteCode
           | Cond Op ByteCode ByteCode
           | While Op ByteCode
           | Let String Op ByteCode
           | Return Op
          deriving Show

type Location = Int
type Index = [String]
type Stack = [Int]

position :: String -> Index ->  Location
position name index = let
                      pos n (nm:nms) = if name == nm
                                       then n
                                       else pos (n+1) nms
                      in pos 1 index

fetch :: Location -> Stack -> Int
fetch n (v:vs)   =  if n == 1 then v else fetch (n-1) vs

put :: Location -> Int -> Stack -> Stack
put n x (v:vs) = if n==1
                 then x:vs
                 else v:(put (n-1) x vs)

newtype M a = StOut (Stack -> (a, Stack, String))

instance Functor M where
  fmap = liftM

instance Applicative.Applicative M where
  pure  = return
  (<*>) = ap

instance Monad M where
  return x = StOut (\n -> (x,n, ""))
  e >>=  f = StOut (\n -> let (a,n1,s1) = (unStOut e) n
                              (b,n2,s2) = unStOut (f a) n1
                          in (b,n2,s1++s2) )

unStOut (StOut f) = f

getFrom   :: Location -> M Int
getFrom i = StOut (\ns -> (fetch i ns, ns, ""))

write     :: Location -> Int -> M ()
write i v = StOut (\ns -> ( (), put i v ns, "") )

push :: Int -> M ()
push x = StOut(\ns -> ((), x:ns, "") )

pop :: M ()
pop = StOut (\m -> let  (n:ns) = m
                   in   ( () , ns ,"" )
            )

eval1 :: Op -> Index -> M Int
eval1 exp index = case exp of
                    Constant n   -> return n
                    Variable x   -> let loc = position x index
                                    in getFrom loc
                    Minus x y    -> do { a <- eval1 x index ;
                                         b <- eval1 y index ;
                                         return (a-b) }
                    Greater x y  -> do { a <- eval1 x index ;
                                         b <- eval1 y index ;
                                         return (if a > b
                                                 then 1
                                                 else 0) }
                    Times x y    -> do { a <- eval1 x index ;
                                         b <- eval1 y index ;
                                         return ( a * b )  }
interpret1 :: ByteCode -> Index -> M ()
interpret1 stmt index = case stmt of
         Assign name e -> let loc = position name index
                          in do { v <- eval1 e index ;
                                       write loc v }
         Seq s1 s2 -> do { x <- interpret1 s1 index ;
                           y <- interpret1 s2 index ;
                           return () }
         Cond e s1 s2 -> do { x <- eval1 e index ;
                              if x == 1
                              then interpret1 s1 index
                              else interpret1 s2 index }
         While e b -> let loop () = do { v <- eval1 e index ;
                                         if v==0 then return ()
                                         else do {interpret1 b index ;
                                         loop () } }
                                         in loop ()
         Let nm e stmt -> do { v <- eval1 e index ;
                                        push v ;
                                        interpret1 stmt (nm:index) ;
                                        pop }
         Return e -> do { v <- eval1 e index ;
                              output v}

output :: Show a => a -> M ()
output v = StOut (\n -> ((),n,show v))

interpreter a = unStOut (interpret1 a []) []

test a = unStOut (eval1 a []) []


newtype Parser a = Parser (String -> [(a,String)] )

parse :: Parser a -> String -> [(a,String)]
parse (Parser p) = p

instance Functor Parser where
  fmap = liftM

instance Applicative.Applicative Parser where
  pure  = return
  (<*>) = ap

instance Monad Parser where
   return a = Parser (\cs -> [(a,cs)] )
   p >>= f  = Parser (\cs ->  concat [ parse (f a) cs' | (a,cs') <- parse p cs] )

instance MonadPlus Parser where
    p `mplus` q = Parser (\cs -> parse p cs ++ parse q cs)
    mzero = Parser (const [])

instance Applicative.Alternative Parser where
    (<|>) = mplus
    empty = mzero

item :: Parser Char
item = Parser (\xs -> case xs of
                         ""     -> []
                         (c:cs) -> [ (c,cs) ] )

(+++)   :: Parser a -> Parser a -> Parser a
p +++ q = Parser (\cs -> case parse (p `mplus` q) cs of
                           [] -> []
                           (x:xs) -> [x]  )

sat :: (Char -> Bool) -> Parser Char
sat p = do { c <- item ; if p c then return c else mzero }

infix  7 ?

(?) :: Parser a -> ( a -> Bool) -> Parser a
p ? test = do { b <- p ; if test b then return b else mzero }

char :: Char -> Parser Char
char c = sat (c ==)

string :: String -> Parser String
string ""        = return ""
string (c:cs)    = do { char c ; string cs; return (c:cs)}

many1 :: Parser a -> Parser [a]
many1 p = do { a<-p ; as <- many p ;
return (a:as) }

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

space :: Parser String
space = many (sat isSpace)

token :: Parser a -> Parser a
token p = do { a<- p ; space ; return a}

symbol :: String -> Parser String
symbol cs = token (string cs)

apply :: Parser a -> String -> [(a,String)]
apply p = parse (do {space ; p } )

ident :: Parser [Char]
ident = do { l   <- sat isAlpha ;
             lsc <- many (sat (\a -> isAlpha a || isDigit a)) ;
             return (l:lsc) }

identif :: Parser [Char]
identif = token ident

var :: Parser Op
var = do { v <-  identif ; return (Variable v) }

chainl :: Parser a -> Parser (a->a->a) -> a -> Parser a
chainl p op a = (p `chainl1` op) +++ return a

chainl1 :: Parser a -> Parser (a->a->a) -> Parser a
p `chainl1` op = do { a <- p; rest a }
                 where
                   rest a = (do  f <- op
                                 b <-p
                                 rest (f a b) )
                                  +++ return a

digit  :: Parser Op
digit  = do { x <- token (sat isDigit) ;
              return  (Constant ( ord x - ord '0' ) ) }

digiti :: Parser Op
digiti = do{ p <- digit;
             l <- many digit;
             return( foldl (\a b -> let Constant nra = a
                                        Constant nrb = b
                                    in Constant  (10*nra + nrb))
                           (Constant 0)
                           (p:l) )}

rexp  :: Parser Op
rexp  = expr `chainl1` relop

expr :: Parser Op
expr = term `chainl1` addop

term :: Parser Op
term = factor `chainl1` mulop

factor :: Parser Op
factor = var +++
         digiti +++
         do {  symbol "(" ; n <- rexp; symbol ")" ; return n }

addop :: Parser (Op -> Op -> Op)
addop = do { symbol "-" ; return (Minus) }
        +++
        do { symbol "+" ; return (Plus) }

mulop :: Parser (Op -> Op -> Op)
mulop = do { symbol "*" ; return (Times) }
       +++
       do { symbol "/" ; return (Div) }

relop :: Parser (Op -> Op -> Op)
relop = do { symbol ">" ; return (Greater) }
        +++
        do { symbol "<" ; return (Less) }
        +++
        do { symbol "=" ; return (Equal) }

returne :: Parser ByteCode
returne = do { symbol "return" ; x <- rexp ; return (Return x) }

assign :: Parser ByteCode
assign = do{x <- identif; symbol "="; e <- rexp; return ( Assign x e)}

seqv  :: Parser ByteCode
seqv = do { symbol "{" ; c <- instruction ; symbol ";"  ; d <- instruction ; symbol "}"
; return (Seq c d) }

cond  :: Parser ByteCode
cond  = do {  symbol "if"   ; e <- rexp ;
              symbol "then" ; c <- instruction ;
              symbol "else" ; d <- instruction ;
              return (Cond e c d) }
while  :: Parser ByteCode
while = do { symbol "while" ;
             e <- rexp ;
             symbol "do" ;
             c <- instruction ;
             return (While e c)  }

declare :: Parser ByteCode
declare = do { symbol "let" ;
               x <- identif ;
               symbol "=" ;
               e <- rexp  ;
               symbol ";" ;
               c <- instruction ;
               return  (Let x e c ) }

instruction :: Parser ByteCode
instruction = assign +++ seqv +++ cond +++ while +++ declare +++ returne


maximum' :: (Ord a) => [a] -> a
maximum' [] = error "sdgsdgfs"
maximum' [x] = x
maximum' (x:xs)
          | x > maxTail = x
          | otherwise = maxTail
          where maxTail = maximum' xs

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Eq, Read)

main :: IO ()
main = do
  let program = "let x = 10; let y = 200; {while x > 0 do { x=x-1; y=y*2 };return y}"
  let parsedPr = apply instruction program
  let out = (\[(a,b)] -> interpreter a ) parsedPr
  (\(c,d,e) -> putStrLn e) out
  let m = maximum' [1,3,67,8,88,9,9,999,999,667678,454]
  print m
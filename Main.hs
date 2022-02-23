{-# LANGUAGE TypeApplications #-}
module Main where

import System.Environment
import Parser
import Control.Monad
import Data.List
import System.Exit
import Data.Char
import Data.Word
import Data.Maybe
import Data.Functor
import Control.Monad.Trans.State.Lazy

help :: String
help = "Usage:      ./Main [-h|--help] input_file\n\n"
    ++ "Syntax cheatsheat: First column shows valid assembly operations.\n"
    ++ "Byte ::= hex | bin | oct | naturalnumber\n"
    ++ "Addr ::= Byte\n"
    ++ "Offset ::= Byte\n"
    ++ "literal prefixes:  bin = `0b`, hex = `0x`, oct = `0o`\n"
    ++ "Use `//` or `;` for line comments.\n"
    ++ "Use `ORG Addr` to start writing from a specific address memory.\n"
    ++ "Use `FCB Byte` to put a byte of data in the current memory cell.\n"
    ++ unlines (map ('\t':) [
    "NOOP                       No Operation        Do Nothing",
    "ADD ACC, M[Addr]           Add                 ACC = ACC + Memory[Addr]; Set C and Z flags",
    "SUB ACC, M[Addr]           Subtract            ACC = ACC - Memory[Addr]; Set C and Z flags",
    "NOT ACC                    Logical NOT         ACC = ACC’; Set Z flag",
    "AND ACC, M[Addr]           Logical AND         ACC = ACC & Memory[Addr]; Set Z flag",
    "CMP ACC, M[Addr]           Compare             if (ACC == Memory[Addr]): E flag=1 else: E flag=0",
    "LB ACC, M[Addr]            Load Byte           ACC = Memory[Addr]",
    "LBI ACC, M[M[Addr]]        Load Byte Index     ACC = Memory[Memory[Addr]]",
    "SB M[Addr], ACC            Store Byte          Memory[Addr]=ACC",
    "SBI M[M[Addr]], ACC        Store Byte Index    Memory[Memory[Addr]]=ACC",
    "IN M[Addr], IO_BUS         Input               Memory[Addr] = Value at IO_BUS",
    "JA Addr                    Jump Address        PC = Addr",
    "J Offset                   Jump                PC = (PC+1) ± Offset",
    "JEQ Offset                 Jump Equal          if (E flag == 1): PC = (PC+1) ± Offset",
    "JNE Offset                 Jump Not Equal      if (E flag == 0): PC = (PC+1) ± Offset",
    "DS                         Display             DS (display register) = ACC"
    ])

data Reg         = ACC               deriving Show
data IOBus       = IOBus             deriving Show
newtype Addr     = Addr     Word8
newtype AddrAddr = AddrAddr Word8
newtype Offset   = Offset   Word8

instance Show Addr where
    show (Addr x) = "M["++show x++"]"

instance Show AddrAddr where
    show (AddrAddr x) = "M[M["++show x++"]]"

instance Show Offset where
    show (Offset x) = "+"++show x

data Opcode = NOOP         | ADD Reg Addr     | SUB Reg Addr  | NOT Reg
            | AND Reg Addr | CMP Reg Addr     | LB Reg Addr   | LBI Reg AddrAddr
            | SB Addr Reg  | SBI AddrAddr Reg | IN Addr IOBus | JA Addr
            | J Offset     | JEQ Offset       | JNE Offset    | DS
        deriving Show



-- ############################# Parsing #############################

uLToken :: Parser String
uLToken = do xs <- token (some (alphanum <|> char '_'))
             let rs = filter isAlpha xs
             guard (all isUpper rs || all isLower rs)
             return xs

theULToken :: String -> Parser String
theULToken s = do t <- uLToken
                  guard (map toUpper t == map toUpper s)
                  return t

reg :: Parser Reg
reg = theULToken "ACC" $> ACC

-- | expects a base under 36?? and that the string only includes valid digits
-- that are allowed in the base
readBase :: Int -> String -> Int
readBase base = sum . zipWith (\i d -> d*base^i) [0..] . reverse . ys
    where ys = map (\x ->
                if x `elem` ['0'..'9'] then
                    ord x - ord '0'
                else ord (toLower x) - ord 'a' + 10)

hex , bin, oct :: Parser Int
hex = do string "0x"
         xs <- some (sat (\x -> toLower x `elem` "0123456789abcdef"))
         return (readBase 16 xs)
bin = do string "0b"
         xs <- some (sat (\x -> toLower x `elem` "01"))
         return (readBase 2 xs)
oct = do string "0o"
         xs <- some (sat (\x -> toLower x `elem` "01234567"))
         return (readBase 8 xs)

isWord8 :: Int -> Bool
isWord8 n
  | n > 0     = minB <= n && n <= maxB
  | otherwise = minB - 128 <= n && n <= maxB - 128
    where maxB = fromIntegral (maxBound @Word8)
          minB = fromIntegral (minBound @Word8)

byte :: Parser Word8
byte = fromIntegral <$> do
    n <- hex <|> bin <|> oct <|> nat <* space
    guard (isWord8 n)
    return n

addr :: Parser Addr
addr = Addr <$> (symbol "M[" *> byte <* symbol "]")

addraddr :: Parser AddrAddr
addraddr = AddrAddr <$> (symbol "M[M[" *> byte <* symbol "]]")

addrLiteral :: Parser Addr
addrLiteral = Addr <$> byte

offset :: Parser Offset
offset = Offset <$> byte

ioBus :: Parser IOBus
ioBus = theULToken "IO_BUS" $> IOBus

newline :: Parser Char
newline = (space *> char '\n')
      <|> (space *> string "\r\n" $> '\n')

noop, add, sub, nott, andd, cmp, lb, lbi, sb, sbi, inn, ja, j, jeq, jne, ds :: Parser Opcode
noop = theULToken "NOOP" $> NOOP
add  = ADD <$> (theULToken "ADD" *> reg      <* symbol ",") <*> addr
sub  = SUB <$> (theULToken "SUB" *> reg      <* symbol ",") <*> addr
nott = NOT <$> (theULToken "NOT" *> reg)
andd = AND <$> (theULToken "AND" *> reg      <* symbol ",") <*> addr
cmp  = CMP <$> (theULToken "CMP" *> reg      <* symbol ",") <*> addr
lb   = LB  <$> (theULToken "LB"  *> reg      <* symbol ",") <*> addr
lbi  = LBI <$> (theULToken "LBI" *> reg      <* symbol ",") <*> addraddr
sb   = SB  <$> (theULToken "SB"  *> addr     <* symbol ",") <*> reg
sbi  = SBI <$> (theULToken "SBI" *> addraddr <* symbol ",") <*> reg
inn  = IN  <$> (theULToken "IN"  *> addr     <* symbol ",") <*> ioBus
ja   = JA  <$> (theULToken "JA"  *> addrLiteral)
j    = J   <$> (theULToken "J"   *> offset)
jeq  = JEQ <$> (theULToken "JEQ" *> offset)
jne  = JNE <$> (theULToken "JNE" *> offset)
ds   = theULToken "DS" $> DS

lineComment :: Parser String
lineComment = do space
                 c <- string "//" <|> string ";"
                 xs <- many (sat (/= '\n'))
                 return (c++xs)

lineExpr :: Parser Expr
lineExpr = Op <$> (opcode <* (some newline <|> (lineComment *> newline $> " ")))
       <|> lineComment *> newline *> lineExpr
       <|> space *> some newline *> lineExpr
       <|> Org <$> (theULToken "ORG" *> addrLiteral)
       <|> Byte <$> (theULToken "FCB" *> byte)

opcode :: Parser Opcode
opcode = noop <|> add <|> sub <|> nott <|> andd <|> cmp <|> lb <|>
    lbi <|> sb <|> sbi <|> inn <|> ja <|> j <|> jeq <|> jne <|> ds
 
toBin :: (Show a, Integral a) => Int -> a -> String
toBin p 0 = replicate p '0'
toBin p n = let xs = concatMap show $ reverse (helper n)
            in  replicate (p - length xs) '0' ++ xs
    where
        helper 0 = []
        helper n = let (q,r) = n `divMod` 2 in r : helper q



-- ###################################################################

zeros :: Int -> String
zeros n = replicate n '0'

machineCode :: Opcode -> String
machineCode NOOP                 = "0000" ++ zeros 8
machineCode (ADD r (Addr a))     = "0001" ++ toBin 8 a
machineCode (SUB r (Addr a))     = "0010" ++ toBin 8 a
machineCode (NOT r)              = "0011" ++ zeros 8
machineCode (AND r (Addr a))     = "0100" ++ toBin 8 a
machineCode (CMP r (Addr a))     = "0101" ++ toBin 8 a
machineCode (LB  r (Addr a))     = "0110" ++ toBin 8 a
machineCode (LBI r (AddrAddr a)) = "0111" ++ toBin 8 a
machineCode (SB  (Addr a) r)     = "1000" ++ toBin 8 a
machineCode (SBI (AddrAddr a) r) = "1001" ++ toBin 8 a
machineCode (IN  (Addr a) b)     = "1010" ++ toBin 8 a
machineCode (JA  (Addr a))       = "1011" ++ toBin 8 a
machineCode (J   (Offset o))     = "1100" ++ toBin 8 o
machineCode (JEQ (Offset o))     = "1101" ++ toBin 8 o
machineCode (JNE (Offset o))     = "1110" ++ toBin 8 o
machineCode DS                   = "1111" ++ zeros 8

data Expr = Op Opcode | Org Addr | Byte Word8 deriving (Show)

data Value = Value String | Empty deriving (Show)

type Result = [Value]

eval :: Expr -> (Addr, Result) -> (Addr, Result)
eval (Op code) (Addr adr, res) = (Addr (adr + 1), res ++ [Value $ machineCode code])
eval (Org (Addr adr')) (Addr adr, res) = (Addr adr', res ++ replicate (fromIntegral adr' - length res) Empty)
eval (Byte w) (Addr adr, res) = (Addr (adr+1), res ++ [Value $ toBin 12 w])

assemble' :: [Expr] -> Result
assemble' = helper (Addr 0, [])
  where
    helper :: (Addr, Result) -> [Expr] -> Result
    helper (adr, res) [] = res
    helper (adr, res) (x : xs) = helper (adr', res') xs
      where (adr', res') = eval x (adr, res)

assemble :: Result -> String
assemble = unlines . (\xs -> xs ++ replicate (256 - length xs) (zeros 12)) . map foo
    where foo Empty     = zeros 12
          foo (Value s) = s


main :: IO ()
main = do
    args <- getArgs
    when (null args) (die "No input file provided. Use `-h` to see help page.")
    when (head args `elem` ["-h", "--help"]) (do {putStr help; exitSuccess})

    contents <- readFile (head args)
    let [(parsed, unparsed)] = parse (many lineExpr) contents
        line                 = head (lines unparsed)
        lineNumber           = (+1) <$> elemIndex line (lines contents)
    unless (null unparsed) (die $ "Invalid syntax at line "
                                 ++ maybe "" show lineNumber++": \n"
                                 ++ line ++ "\n\n"
                                 ++ "Use `-h` to see help page.")

    let res = assemble' parsed
        mem = length res
    when (mem > 256) (die $ "Out of memory, the current program needs "
                           ++ show mem
                           ++ " memory cells, memory has 256 cells.")

    writeFile "memory.mif" (assemble res)
    putStrLn "Wrote to memory.mif"


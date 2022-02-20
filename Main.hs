module Main where

import System.Environment
import Debug.Trace
import Parser
import Control.Monad
import Data.List
import System.Exit
import Data.Char
import Data.Word
import Data.Maybe

{-
NOOP
ADD ACC, Mem[Addr]
SUB ACC, Mem[Addr]
NOT ACC
AND ACC, Mem[Addr]
CMP ACC, Mem[Addr]
LB ACC, Mem[Addr]
LBI ACC, Mem[Mem[Addr]]
SB Mem[Addr], ACC
SBI Mem[Mem[Addr]], ACC
IN Mem[Addr], IO_BUS
JA Addr
J Offset
JEQ Offset
JNE Offset
DS
-}

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
    show (Offset x) = "±"++show x

data Opcode = NOOP
            | ADD Reg Addr
            | SUB Reg Addr
            | NOT Reg
            | AND Reg Addr
            | CMP Reg Addr
            | LB Reg Addr
            | LBI Reg AddrAddr
            | SB Addr Reg
            | SBI AddrAddr Reg
            | IN Addr IOBus
            | JA Addr
            | J Offset
            | JEQ Offset
            | JNE Offset
            | DS
        deriving Show

 -- ordered after opcode in binary
opcodes = [
    "NOOP",
    "ADD",
    "SUB",
    "NOT",
    "AND",
    "CMP",
    "LB",
    "LBI",
    "SB",
    "SBI",
    "IN",
    "JA",
    "J",
    "JEQ",
    "JNE",
    "DS"
    ]

upperLowerToken :: Parser String
upperLowerToken = do xs <- some alphanum
                     guard (all isUpper xs || all isLower xs)
                     space
                     return xs

opcode :: Parser String
opcode = do xs <- upperLowerToken
            guard (map toUpper xs `elem` opcodes)
            return xs

reg :: Parser Reg
reg = do r <- upperLowerToken
         if map toUpper r == "ACC"
            then return ACC
            else error "unknown reg"

readHex :: String -> Int
readHex = sum . zipWith (*) [1,16..] . reverse . ys
    where
        ys = map (\x -> if x `elem` ['0'..'9'] then
                            ord x - ord '0'
                        else
                            ord x - ord 'a' + 10)

--hex = do { string "0x"; xs <- some (sat (`elem` (['0'..'9']++['a'..'f']) . toLower)); return (readHex xs)}
--bin = do { string "0b"; nat }

addr :: Parser Addr
addr = (Addr . fromIntegral <$> do { char 'M'; space; char '['; space; n <- nat; space; char ']'; space; return n })
       <|> Addr . fromIntegral <$> nat
       -- <|> Addr . fromIntegral <$> hex

addraddr :: Parser AddrAddr
addraddr = AddrAddr . fromIntegral <$> nat

offset :: Parser Offset
offset = Offset . fromIntegral <$> nat

ioBus :: Parser IOBus
ioBus = do b <- upperLowerToken
           if map toUpper b == "IO_BUS"
              then return IOBus
              else error "unknown IO_Bus"

newline :: Parser Char
newline = do { space; char '\n' }

noop = return NOOP
add  = do { r <- reg; space; char ','; space; ADD r <$> addr; }
sub  = do { r <- reg; space; char ','; space; SUB r <$> addr; }
nott  = NOT <$> reg
andd  = do { r <- reg; space; char ','; space; AND r <$> addr; }
cmp  = do { r <- reg; space; char ','; space; CMP r <$> addr; }
lb   = do { r <- reg; space; char ','; space; LB r <$> addr; }
lbi  = do { r <- reg; space; char ','; space; LBI r <$> addraddr; }
sb   = do { a <- addr; space; char ','; space; SB a <$> reg; }
sbi  = do { a <- addraddr; space; char ','; SBI a <$> reg; }
inn   = do { a <- addr; space; char ','; IN a <$> ioBus; }
ja   = JA <$> addr
j    = J <$> offset
jeq  = JEQ <$> offset
jne  = JNE <$> offset
ds   = return DS

lineComment :: Parser String
lineComment = do space
                 c <- string "//"
                 xs <- many (sat (/= '\n'))
                 return (c++xs)

lineWithComment = do x <- line
                     some newline <|> (do {lineComment; newline; return " "})
                     return (Just x)
                  <|> do {lineComment; newline; return Nothing}
                  <|> do {space; newline; return Nothing}


line :: Parser Opcode
line = do op <- opcode
          case map toUpper op of
            "NOOP" -> noop
            "ADD"  -> add
            "SUB"  -> sub
            "NOT"  -> nott
            "AND"  -> andd
            "CMP"  -> cmp
            "LB"   -> lb
            "LBI"  -> lbi
            "SB"   -> sb
            "SBI"  -> sbi
            "IN"   -> inn
            "JA"   -> ja
            "J"    -> j
            "JEQ"  -> jeq
            "JNE"  -> jne
            "DS"   -> ds
            op -> error "hello"

main = do
    args <- getArgs
    when (null args) (die "No input file provided.")

    contents <- readFile (head args)
    --let line = head (lines contents)
    print contents
    print . catMaybes . fst . head $ parse (many lineWithComment) contents
    print $ parse (many lineWithComment) contents



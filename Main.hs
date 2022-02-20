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

help = "Usage:      ./Main [-h|--help] input_file\n\n"
    ++ "Syntax cheatsheat: First column shows valid assembly operations.\n"
    ++ "Addr ::= hex | bin | oct | naturalnumber\n"
    ++ "Offset ::= int\n"
    ++ "literal prefixes:  bin = `0b`, hex = `0x`, oct = `0o`\n"
    ++ "Use `//` for line comments.\n"
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
upperLowerToken = do xs <- some (alphanum <|> char '_')
                     let rs = filter isAlpha xs
                     guard (all isUpper rs || all isLower rs)
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

-- | expects a base under 36?? and that the string only includes valid digits
-- that are allowed in the base
readBase :: Int -> String -> Int
readBase base = sum . zipWith (\i d -> d*base^i) [0..] . reverse . ys
    where
        ys = map (\x -> if x `elem` ['0'..'9'] then
                            ord x - ord '0'
                        else
                            ord (toLower x) - ord 'a' + 10)

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
  | n > 0 = fromIntegral (minBound :: Word8) <= n
            && n <= fromIntegral (maxBound :: Word8)
  | otherwise = fromIntegral (minBound :: Word8) - 128 <= n
                && n <= fromIntegral (maxBound :: Word8) - 128

addr :: Parser Addr
addr = do string "M["
          space
          n <- bin <|> hex <|> oct <|> nat
          guard (isWord8 n)
          space
          char ']'
          space
          return (Addr . fromIntegral $ n)

addraddr :: Parser AddrAddr
addraddr = do string "M[M["
              space
              n <- hex <|> bin <|> oct <|> nat
              guard (isWord8 n)
              space
              string "]]"
              space
              return (AddrAddr . fromIntegral $ n)

addrLiteral :: Parser Addr
addrLiteral = Addr . fromIntegral <$> do {n <- hex <|> bin <|> oct <|> nat;
                                         guard (isWord8 n);
                                         return n}

offset :: Parser Offset
offset = Offset . fromIntegral <$> do { n <- int;
                                        guard (isWord8 n);
                                        return n }

ioBus :: Parser IOBus
ioBus = do b <- upperLowerToken
           if map toUpper b == "IO_BUS"
              then return IOBus
              else error $ "unknown IO_Bus" ++ show b

newline :: Parser Char
newline = do { space; char '\n' }
          <|> do { space; string "\r\n"; return '\n'}

noop, add, sub, nott, andd, cmp, lb, lbi, sb, sbi, inn, ja, j, jeq, jne, ds :: Parser Opcode
noop = return NOOP
add  = do { r <- reg      ; space ; char ',' ; space ; ADD r <$> addr     }
sub  = do { r <- reg      ; space ; char ',' ; space ; SUB r <$> addr     }
nott = NOT <$> reg
andd = do { r <- reg      ; space ; char ',' ; space ; AND r <$> addr     }
cmp  = do { r <- reg      ; space ; char ',' ; space ; CMP r <$> addr     }
lb   = do { r <- reg      ; space ; char ',' ; space ; LB r  <$> addr     }
lbi  = do { r <- reg      ; space ; char ',' ; space ; LBI r <$> addraddr }
sb   = do { a <- addr     ; space ; char ',' ; space ; SB a  <$> reg      }
sbi  = do { a <- addraddr ; space ; char ',' ; space ; SBI a <$> reg      }
inn  = do { a <- addr     ; space ; char ',' ; space ; IN a  <$> ioBus    }
ja   = JA  <$> addrLiteral
j    = J   <$> offset
jeq  = JEQ <$> offset
jne  = JNE <$> offset
ds   = return DS

lineComment :: Parser String
lineComment = do space
                 c <- string "//"
                 xs <- many (sat (/= '\n'))
                 return (c++xs)

lineWithComment :: Parser (Maybe Opcode)
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
            op -> error $ "unknwn opcode  `"++ op ++ "`"

toBin :: (Show a, Integral a) => Int -> a -> String
toBin p 0 = replicate p '0'
toBin p n = let xs = concatMap show $ reverse (helper n)
            in  replicate (p - length xs) '0' ++ xs
    where
        helper 0 = []
        helper n = let (q,r) = n `divMod` 2 in r : helper q

assemble :: [Opcode] -> String
assemble = unlines . (\xs -> xs ++ replicate (256 - length xs) (zeros 12)) . map helper
    where
        zeros n = replicate n '0'
        helper x = case x of
            NOOP                 -> "0000" ++ zeros 8
            (ADD r (Addr a))     -> "0001" ++ toBin 8 a
            (SUB r (Addr a))     -> "0010" ++ toBin 8 a
            (NOT r)              -> "0011" ++ zeros 8
            (AND r (Addr a))     -> "0100" ++ toBin 8 a
            (CMP r (Addr a))     -> "0101" ++ toBin 8 a
            (LB  r (Addr a))     -> "0110" ++ toBin 8 a
            (LBI r (AddrAddr a)) -> "0111" ++ toBin 8 a
            (SB  (Addr a) r)     -> "1000" ++ toBin 8 a
            (SBI (AddrAddr a) r) -> "1001" ++ toBin 8 a
            (IN  (Addr a) b)     -> "1010" ++ toBin 8 a
            (JA  (Addr a))       -> "1011" ++ toBin 8 a
            (J   (Offset o))     -> "1100" ++ toBin 8 o
            (JEQ (Offset o))     -> "1101" ++ toBin 8 o
            (JNE (Offset o))     -> "1110" ++ toBin 8 o
            DS                   -> "1111" ++ zeros 8

main :: IO ()
main = do
    args <- getArgs
    when (null args) (die "No input file provided. Use `-h` to see help page.")
    when (head args `elem` ["-h", "--help"]) (do {putStr help; exitSuccess})

    contents <- readFile (head args)
    let [(res, unparsed)] = parse (many lineWithComment) contents
        line = head (lines unparsed)
        lineNumber = (+1) . fromJust $ elemIndex line (lines contents)
    unless (null unparsed) (die $ "Invalid syntax at line "
                                 ++ show lineNumber ++": \n"
                                 ++ line ++ "\n\n"
                                 ++ "Use `-h` to see help page.")

    let parsed = catMaybes res
    writeFile "memory.mif" (assemble parsed)
    putStrLn "Wrote to memory.mif"


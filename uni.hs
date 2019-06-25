import Text.Parsec
import System.Environment

data Uni = Uni Version Leader [Field]
data Leader = Leader Header
data Field = ControlField Code Value | DataField Code Idx1 Idx2 [SubField]
data SubField = SubField Code Value

type Version = String
type Header = String
type Code = String
type Value = String
type Idx1 = Char
type Idx2 = Char

-- ZA JSON
instance Show Uni where
    show (Uni mark lead fields) = "{\n" ++ "\"version\":" ++ show mark ++ ",\n" ++ show lead ++ show fields ++ "\n}"

instance Show Leader where
    show (Leader header) = "\"leader\":\"" ++ header ++ "\",\n\"fields\":\n"

instance Show Field where
    show (ControlField code value) = "\n\t{\n\t\t\"" ++ code ++ "\":\"" ++ value ++ "\"\t\n\t}"
    show (DataField code idx1 idx2 (x:xs)) = "\n{\n\"" ++ code ++ "\":\n{\n" ++ "\"subfields\":\n" ++ show (x:xs) ++ ",\n\"idx1\":\"" ++ [idx1] ++"\",\n\"idx2\":\"" ++ [idx2] ++ "\"\n}\n}"  

instance Show SubField where
    show (SubField code value) = "\n{\n\"" ++ code ++ "\":\"" ++ value ++ "\"\n}"





document :: Parsec String Int Uni
document = do version <- get_version
              ld <- get_leader
              fields <- many get_field
              eof
              return (Uni version ld fields)


get_field :: Parsec String Int Field
get_field = do first <- count 3 digit
               spaces
               second <- many (noneOf "\n ")
               if (length second) > 2
                  then 
                      do   third <- many (noneOf ['\n'])
                           try (char '\n')
                           return (ControlField first (second++third))
               else
                      do   spaces
                           subfields <- many get_subfield
                           try (char '\n')
                           return (DataField first (head second) (last second) subfields)


get_subfield:: Parsec String Int SubField
get_subfield =  do try (char '[')
                   idx <- many (noneOf "]\n")
                   ostalo <- many (noneOf "[\n")
                   return (SubField idx (tail ostalo))

get_leader :: Parsec String Int Leader
get_leader = return (Leader " ")
    
get_version :: Parsec String Int Version
get_version = return ("UNIMARC")

main :: IO ()
main = do (input:output:[]) <- getArgs
          cont <- readFile input
          case (runParser document 0 input cont) of
            Left err -> putStrLn . show $ err
            Right ldr -> writeFile output . show $ ldr

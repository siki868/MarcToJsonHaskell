import Text.Parsec
import System.Environment

data Marc21 = Marc21 Version Leader [Field]
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
instance Show Marc21 where
    show (Marc21 mark lead fields) = "{\n" ++ "\"version\":" ++ show mark ++ ",\n" ++ show lead ++ show fields ++ "\n}"

instance Show Leader where
    show (Leader header) = "\"leader\":\"" ++ header ++ "\",\n\"fields\":\n"

instance Show Field where
    show (ControlField code value) = "\n\t{\n\t\t\"" ++ code ++ "\":\"" ++ value ++ "\"\t\n\t}"
    show (DataField code idx1 idx2 (x:xs)) = "\n{\n\"" ++ code ++ "\":\n{\n" ++ "\"subfields\":\n" ++ show (x:xs) ++ ",\n\"idx1\":\"" ++ [idx1] ++"\",\n\"idx2\":\"" ++ [idx2] ++ "\"\n}\n}"
    show (DataField code idx1 idx2 []) = "\n{\n\"" ++ code ++ "\":\n{\n" ++ "\"subfields\":\n{},\n\"idx1\":\"" ++ [idx1] ++"\",\n\"idx2\":\"" ++ [idx2] ++ "\"\n}\n}"  

instance Show SubField where
    show (SubField code value) = "\n{\n\"" ++ code ++ "\":\"" ++ value ++ "\"\n}"

-- ZA TXT
-- instance Show Marc21 where
--     show (Marc21 mark lead fields) = "Verzija: " ++ show mark ++ "\n" ++ show lead ++ show fields

-- instance Show Leader where
--     show (Leader header) = "Zaglavlje: " ++ header ++ "\nPolja: :"

-- instance Show Field where
--     show (ControlField code value) = "\n" ++ code ++ ":" ++ value
--     show (DataField code idx1 idx2 (x:xs)) = "\n" ++ code ++ ":\n" ++ "Potpolja: :" ++ show (x:xs) ++ ",\n idx1:" ++ [idx1] ++",\nidx2\":" ++ [idx2] 

-- instance Show SubField where
--     show (SubField code value) = "\n" ++ code ++ ":" ++ value




document :: Parsec String Int Marc21
document = do version <- get_version
              if version == "LEADER"
                    then
                        do  ld <- get_leader
                            spaces
                            fields <- many get_field_leader
                            eof
                            return (Marc21 version ld fields)
                    else
                        do  spaces
                            ld <- get_leader
                            spaces
                            fields <- many get_field
                            eof
                            return (Marc21 version ld fields)


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


get_field_leader :: Parsec String Int Field
get_field_leader = do   first <- count 3 digit
                        spaces
                        second <- many (noneOf "\n ")
                        if (length second) > 2
                            then 
                                do      third <- many (noneOf ['\n'])
                                        try (char '\n')
                                        return (ControlField first (second++third))
                        else if (head second) == '$'
                            then
                                do      spaces
                                        nesto <- many (noneOf "$\n")
                                        subfields <- many get_subfield_leader
                                        let sve = [SubField [last second] (nesto)] ++ subfields
                                        try (char '\n')
                                        return (DataField first 'a' 'a' sve)
                        else 
                                do      spaces
                                        subfields <- many get_subfield_leader
                                        try (char '\n')
                                        return (DataField first (head second) (last second) subfields)


get_subfield:: Parsec String Int SubField
get_subfield =  do try (char '$') <|> (char '|')
                   sf <- many (noneOf "|$\n")
                   return (SubField [head sf] (tail sf))




get_subfield_leader:: Parsec String Int SubField
get_subfield_leader =  do   try (char '$')
                            sf <- many (noneOf "$\n")
                            return (SubField [head sf] (tail (tail (init sf))))


get_leader :: Parsec String Int Leader
get_leader = do h <- count 24 anyChar
                return (Leader h)
    
get_version :: Parsec String Int Version
get_version = do v <- try (string "LDR") <|> (string "000") <|> (string "LEADER")
                 return (v)





main :: IO ()
main = do (input:output:[]) <- getArgs
          cont <- readFile input
          case (runParser document 0 input cont) of
            Left err -> putStrLn . show $ err
            Right ldr -> writeFile output . show $ ldr

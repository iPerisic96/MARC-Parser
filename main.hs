import Text.Parsec
import System.Environment

data Document = Document Leader [Record]

data Leader = Leader Code Value

data Record = Record [Field]

data Field = ControlField Code Value | DataField Code Indicator1 Indicator2 [Subfield]

data Subfield = Subfield CodeC Value 

type Indicator1 = Char
type Indicator2 = Char
type Code = String
type Value = String
type CodeC = Char
                                    
instance Show Document where
    show (Document leader records) =
                        "{\n" ++
                        "\"leader\": " ++ (show leader) ++ ",\n" ++
                        "\"records\":\n" ++ (show records) ++
                        "\n}"

instance Show Leader where
    show (Leader code value) = "\"" ++ value ++ "\""


instance Show Record where
    show (Record fields) = "{\n" ++ 
                            insertTabs 1 ++ "\"fields\": \n" ++ 
                            insertTabs 2 ++ (show fields) ++ 
                            insertTabs 1 ++ "}\n"

instance Show Field where
    show (ControlField code value) = "\n" ++ --[ iz show fields '[', zato new line
                                     insertTabs 3 ++ "{\n" ++ 
                                     insertTabs 4 ++ "\"" ++ code ++ "\":\"" ++ value ++ "\"\n" ++ 
                                     insertTabs 3 ++ "}"                                                                                                       -- cast char to string ->  string = [char]              
    show (DataField code indicator1 indicator2 subfields) = "\n" ++ insertTabs 3 ++ "{\n" ++ 
                                                         insertTabs 4 ++ "\"" ++ code ++ "\":\n" ++ 
                                                         insertTabs 4 ++ "{\n" ++ insertTabs 4 ++ "\"subfields\":\n" ++ 
                                                         insertTabs 5 ++ (show subfields) ++ ",\n" ++ 
                                                         insertTabs 6 ++ "\"ind1\":\"" ++ [indicator1] ++"\",\n" ++ 
                                                         insertTabs 6 ++ "\"ind2\":\"" ++ [indicator2] ++ "\"\n" ++ 
                                                         insertTabs 4 ++ "}\n" ++ insertTabs 3 ++ "}"  

instance Show Subfield where
    show (Subfield codeC value) = "\n" ++ insertTabs 6 ++ "{\n" ++ 
                                  insertTabs 7 ++ "\"" ++ [codeC] ++ "\":\"" ++ value ++ "\"\n" ++
                                  insertTabs 6 ++ "}"

insertTabs :: Int -> String
insertTabs 0 = ""
insertTabs c = "\t" ++ insertTabs (c - 1)

document :: Parsec String Int Document
document = do 
              leader <- try leader <|> return (Leader "" "")
              records <- many1 record
              eof
              return (Document leader records)

leader :: Parsec String Int Leader
leader = do 
            code <- try (string "LDR") <|> try (string "000") <|> try (string "LEADER")
            spaces
            value <- count 24 anyChar
            try (char '\r') <|> try (char '\n') 
            return (Leader code value)
            
record :: Parsec String Int Record
record = do 
            fields <- many1 (try field)
            spaces
            return (Record fields)

field :: Parsec String Int Field
field = do 
            spaces
            code <- count 3 digit
            --indicator <- lookAhead (anyChar)--
           -- indicators <- many (noneOf "\n ")--
            indicators <- try (indicators1) <|> try (indicators2) <|> try (indicators3) <|> return (take 2 (repeat '\0'))
            
            {--if (length indicators < 3 && length indicators > 0) then do -- 010 ## $a92005291 // DataField--         
                space 
                subfields <- many1 subfield 
                return (DataField code (indicators !! 0) (indicators !! 1) subfields)
            else do
                spaces
                value <- many (noneOf "\n")
                spaces
                return (ControlField code (indicators ++ value))
                --}
            
            if (length indicators <= 0) then do
                subfields <- many1 subfield 
                return (DataField code ('\0') ('\0') subfields)
            else do
                if (indicators !! 0 == '\0') then do
                    spaces 
                    value <- many (noneOf "\n")
                    return (ControlField code value)
                else do
                   {--subfields <- many1 subfield
                    field5Code <- lookAhead anyChar
                    if (field5Code == '\RS') then do
                        field5Code <- char '\RS'
                        return (DataField code (indicators !! 0) (indicators !! 1) subfields)
                    else do --}
                    field5Code <- lookAhead anyChar
                    if (field5Code == '\US') then do
                        subfields <- many1 subfield
                        many (char '\RS') 
                        return (DataField code (indicators !! 0) (indicators !! 1) subfields)    
                    else do
                        subfields <- many1 subfield 
                        return (DataField code (indicators !! 0) (indicators !! 1) subfields)
                
indicators1 :: Parsec String Int [Char]  
indicators1 = do 
                space
                indicator <- lookAhead anyChar
                if (indicator == '$' ) then 
                    return []
                else do
                    indicator11 <- anyChar
                    indicator22 <- anyChar
                    if (indicator22 == ' ') then do
                        return [indicator11, '\0']
                    else do
                        space
                        return [indicator11, indicator22] -- vrati listu indikatora --
        
indicators2 :: Parsec String Int [Char]
indicators2 = do
                indicator1 <- anyChar
                indicator2 <- anyChar
                space
                return [indicator1, indicator2]
             
indicators3 :: Parsec String Int [Char]
indicators3 = do
                indicator1 <- anyChar
                indicator2 <- anyChar
                us <- lookAhead anyChar
                if (us == '\US') then do
                    return [indicator1, indicator2]
                else do
                    fail "Cant do that"
                
subfield :: Parsec String Int Subfield
subfield = do 
            brackets <- lookAhead anyChar
            if (brackets == '[') then do
                brackets <- char '['
                codeD <- anyChar
                closeBrackets <- char ']'
                value <- many (noneOf "$\n|[")
                return (Subfield codeD value)
            else do     
                char '$' <|> char '|' <|> char '\US'
                codeD <- anyChar
                value <- many (noneOf ['$', '\n', '|', '\US', '\RS'])
                return (Subfield codeD value)
                
main :: IO ()
main = do
        (input:output:[]) <- getArgs
        cont <- readFile input
        case (runParser document 0 input cont) of
            Left err -> putStrLn . show $ err
            Right rss -> writeFile output . show $ rss


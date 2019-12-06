import Data.List
import Data.Char
import Data.Maybe

text = "He who controls the past controls the future. He who controls the present controls the past."

data Token = Word String | Blank | HypWord String deriving (Eq, Show)

str2line :: String -> [Token]
str2line = \s ->
    case words s of
      [] -> []
      x:xs -> [Word x] ++ str2line (unwords xs)

line2str :: [Token] -> String
line2str = \tokens ->
    case tokens of
      [] -> ""
      (Word x):xs -> x ++ (if xs /= [] then " " else "") ++ line2str xs
      (HypWord x):xs -> x ++ (if xs /= [] then "- " else "-") ++ line2str xs

tokLen :: Token -> Int
tokLen = \t ->
    case t of
      Word w -> length w
      HypWord w -> length w + 1
      Blank -> 1

lineLen :: [Token] -> Int
lineLen = \tokens ->
    case tokens of
      [] -> -1 
      x:xs -> tokLen x + lineLen xs + 1

breakLine :: Int -> [Token] -> ([Token],[Token])
breakLine = \lim -> \tokens ->
    case tokens of
      [] -> ([],[])
      x:xs | tokLen x <= lim -> let (a, b) = breakLine (lim - 1 - tokLen x) xs in ([x] ++ a, b)
      _ -> ([], tokens)

mergers :: [String] -> [(String, String)]
mergers = \s ->
    case s of
      [] -> []
      x:[] -> []
      xa:xb:[] -> [(xa, xb)]
      xa:xb:xs -> [(xa, concat (xb:xs))] ++ mergers ([xa ++ xb] ++ xs)


enHyp = [("controls",["co","nt","ro","ls"]), ("future",["fu","tu","re"]),("present",["pre","se","nt"])]

tokenize :: String ->(String,String) -> (Token, Token)
tokenize = \punc -> \(a,b) -> (HypWord a, Word (b ++ punc))

parse :: String -> (String,String)
parse = \w -> span isAlpha w

hyphenate :: [(String, [String])] -> Token -> [(Token,Token)]
hyphenate = \hmap -> \token -> 
    case token of
      Word w -> map (tokenize (snd (parse w))) (mergers (snd (fromJust (find (\a -> fst a == fst (parse w)) hmap))))
      _ -> error "die"


fn = \(x,y) -> \(a, b) -> (x ++ [a], b:y)


lineBreaks :: [(String,[String])] -> Int -> [Token] -> [([Token],[Token])]
lineBreaks = \hmap -> \len -> \line ->
    case breakLine len line of
      (_,[]) -> []
      (l, hyp:xs) -> let combos = (filter (\(x,_) -> (len - lineLen l - 1 - tokLen x) >= 0) (hyphenate hmap hyp)) in
                         [(l, hyp:xs)] ++ map (fn (l, xs)) combos


insertions :: a -> [a] -> [[a]]
insertions = \e -> \arr ->
    case arr of
      [] -> [[e]]
      x:xs -> [e:arr] ++ (map (\s -> x:s) (insertions e xs))


rmDups :: [Token] -> [Token]
rmDups = \list ->
    case list of
      [] -> []
      x:xs -> let comp = (\(Word w1) -> \(Word w2) -> w1 /= w1) in filter (comp x) (rmDups xs)

insertBlanks :: Int -> [Token] -> [[Token]]
insertBlanks = \n -> \tokens ->
    case tokens of
      [] -> []
      _ | n == 0 -> [tokens]
      x:xs -> let mid = init xs in 
                  let wrap = (\t -> (x:t) ++ [last xs]) in
                          let combos = insertions Blank mid in
                              let perms = concat (map (insertions Blank) combos) in
                                  rmDups (map wrap perms)

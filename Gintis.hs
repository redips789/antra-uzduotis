module Gintis
where
import Data.Char
import Data.List
import Network.HTTP.Client
 

message :: String
message = "(l (m \"x\" 0 \"y\" 0 \"v\" \"x\") (m \"x\" 0 \"y\" 1 \"v\" \"o\") (m \"x\" 0 \"y\" 2 \"v\" \"x\"))"

move :: String -> Maybe (Int, Int, Char)
move str = calculteMove (pirmasZingsnis str)

calculteMove :: [(Char,Char,Char)] -> Maybe(Int, Int, Char)
calculteMove moves = findMove moves

parseMove :: Maybe (Int, Int, Char) -> Maybe String
parseMove Nothing = Nothing
parseMove (Just (x,y,z)) = Just (" (m \"x\" " ++ [(intToDigit x)] ++ " \"y\" " ++ [(intToDigit y)] ++  "\"v\" \"" ++ [z] ++"\"))")		

turn :: String -> Maybe String
turn msg = parseMove $ move msg 

findMove :: [(Char,Char,Char)] -> Maybe(Int, Int, Char)
findMove moves  
	| (filter(\(a,b,c)-> a=='1' && b=='1') moves)==[] = Just (digitToInt '1',digitToInt '1',whoseTurn moves)
	| (filter(\(a,b,c)-> a=='0' && b=='0') moves)==[] = Just (digitToInt '0',digitToInt '0',whoseTurn moves) 
	| (filter(\(a,b,c)-> a=='0' && b=='2') moves)==[] = Just (digitToInt '0',digitToInt '2',whoseTurn moves)
	| (filter(\(a,b,c)-> a=='2' && b=='0') moves)==[] = Just (digitToInt '2',digitToInt '0',whoseTurn moves)
	| (filter(\(a,b,c)-> a=='1' && b=='0') moves)==[] = Just (digitToInt '1',digitToInt '0',whoseTurn moves)
	| (filter(\(a,b,c)-> a=='1' && b=='2') moves)==[] = Just (digitToInt '1',digitToInt '2',whoseTurn moves)
	| (filter(\(a,b,c)-> a=='2' && b=='1') moves)==[] = Just (digitToInt '2',digitToInt '1',whoseTurn moves)
	| (filter(\(a,b,c)-> a=='0' && b=='1') moves)==[] = Just (digitToInt '0',digitToInt '1',whoseTurn moves)
	| (filter(\(a,b,c)-> a=='2' && b=='2') moves)==[] = Just (digitToInt '2',digitToInt '2',whoseTurn moves)
	| otherwise = Nothing

whoseTurn :: [(Char,Char,Char)] -> Char
whoseTurn moves
	|(mod (length moves) 2) == 0 = 'x'
	|(mod (length moves) 2) == 1 = 'o'
pirmasZingsnis :: String -> [(Char,Char,Char)]
pirmasZingsnis "" = error "no list"
pirmasZingsnis ('(' : 'l' : ' ' :rest) = antrasZingsnis (take ((length rest) -1) rest)
pirmasZingsnis _ = error "netinkamas tipas"

antrasZingsnis :: String -> [(Char,Char,Char)]
antrasZingsnis "" = []
antrasZingsnis a = treciasZingsnis a []

treciasZingsnis :: String -> [(Char,Char,Char)] -> [(Char,Char,Char)]
treciasZingsnis a [] = 
    let
		x = take 1 (drop 7 a)
		x' = head x
		y = take 1 (drop 13 a)
		y' = head y
		v = take 1 (drop 20 a)
		v' = head v
		rest = drop 24 a
	in treciasZingsnis rest ((x',y',v') : [])
treciasZingsnis ('(':a) acc =
	let
		x = take 1 (drop 6 a)
		x' = head x
		y = take 1 (drop 12 a)
		y' = head y
		v = take 1 (drop 19 a)
		v' = head v
		rest = drop 23 a
	in treciasZingsnis rest ((x',y',v'):acc)
treciasZingsnis "" acc = acc
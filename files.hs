import IO







----------------------------------------------------------------------------------------------------------- 
------------------------------------RADNA 2012 ------------------------------------------- 
-----------------------------------------------------------------------------------------------------------
filllogins :: FilePath -> FilePath -> FilePath -> IO ()
filllogins log txt res = do
  logH <- openFile log ReadMode -- Handlery souboru
  txtH <- openFile txt ReadMode -- pro cteni
  resH <- openFile res WriteMode -- a pro zapis
  logC <- hGetContents logH -- Nacteni obsahu souboru s loginy
  txtC <- hGetContents txtH -- Nacteni obsahu sablony
  hPutStr resH $ unlines $ map (subs txtC) (lines logC) -- Nahrazeni loginu, unline, zapis
  hClose logH -- Close
  hClose txtH -- all the
  hClose resH -- files
  where
      subs [] _ = [] -- Kdyz neni co nahrazovat tak konec
      subs line@(x:xs) login = if take 8 line == "xzzzzz99" -- Kdyz retezec zacina "xzzzzz99"...
				    then login ++ subs (drop 8 line) login -- Tak ho nahradime za login a rekurzime zbytek retezce
				    else x:subs xs login -- Jinak preskocime jeden znak a rekurzime zbytek retezce

-----------------------------------------------------------------------------------------------------------
io :: FilePath -> FilePath -> FilePath -> IO()
io f1 f2 f3 = do
  f1h <- openFile f1 ReadMode
  f1c <- hGetContents f1h
  f2h <- openFile f2 ReadMode
  f2c <- hGetContents f2h
  f3h <- openFile f3 WriteMode
  let st = substitute (lines f1c) (lines f2c)
  hPutStr f3h (unlines st)
  hClose f1h
  hClose f2h
  hClose f3h
 
substitute :: [String] -> [String] -> [String]
substitute [] _ = []
substitute (x:xs) text = (unlines $ subs x text) : substitute xs text
 
subs :: String -> [String] -> [String]
subs _ [] = []
subs login (x:xs) = subs' login x:subs login xs
 
subs' :: String -> String -> String
subs' _ [] = []
subs' login@(a':b':c':d':e':f':g':h':[]) (a:b:c:d:e:f:g:h:xs) = if a=='x' && b=='z' && c=='z' && d=='z' && e=='z' && f=='z' && g=='9' && h=='9'
                                   then a':b':c':d':e':f':g':h':subs' login xs
                                   else a:subs' login (b:c:d:e:f:g:h:xs)
subs' login (x:xs) = x:subs' login xs

------------------------------------------------ Ne uplne kotretni verze (nize)----------------------------------------------------------- 
 
 -- pouziti: logins "xlogin" "text" "text2"
logins file1 file2 file3 = do
		f1 <- openFile file1 ReadMode
		f2 <- openFile file2 ReadMode
		f3 <- openFile file3 AppendMode
		cf1 <- hGetContents f1
		cf2 <- hGetContents f2
		let seznamLoginu = words cf1
		--   let pocetRadku_F1 = length (lines cf1)
		--   print pocetRadku_F1
		solveWrite seznamLoginu cf2 f3
		hClose f1
		hClose f2
		hClose f3
		
solveWrite [] _ f3 = return () 
solveWrite seznamLoginu cf2 f3 = do
  let seznamSlov = words cf2
--   print seznamSlov
--   print seznamLoginu
  let onelogin = concat $ take 1 seznamLoginu
  solveCompare onelogin seznamSlov f3
  let slnew = drop 1 seznamLoginu
--   print slnew
  solveWrite slnew cf2 f3
  
-- VYPIS na do souboru  
solveCompare :: [Char] -> [[Char]] -> Handle -> IO ()
solveCompare x [] f3 = hPutStr f3 "\n"
solveCompare x (y:ys) f3 =if check   then do nahrad
					     solveCompare x ys f3
				     else do hPutStr f3 (y ++ " ")
					     solveCompare x ys f3
    where
      check = (==) "xzzzzz99" y
      nahrad = hPutStr f3 (x ++ " ")

-- VYPIS na obrazovku 
{-solveCompare2 x []= putStr ("\n")
solveCompare2 x (y:ys) = if check   then do nahrad
					    solveCompare2 x ys
				     else do putStr (y ++ " ")
					     solveCompare2 x ys
    where
      check = (==) "xzzzzz99" y
      nahrad = putStr (x ++ " ")   -}     
      
      
      
-- TEST      
-- solveS (x:xs) = show (x ++ "\n") : solveS xs
-----------------------------------------------------------------------------------------------------------
-- pouziti: logins "xlogin" "text" "text2"
logins2 :: FilePath -> FilePath -> FilePath -> IO ()
logins2 file1 file2 file3 = do
		f1 <- openFile file1 ReadMode
		f2 <- openFile file2 ReadMode
		f3 <- openFile file3 WriteMode
		cf1 <- hGetContents f1
		cf2 <- hGetContents f2
		let seznamLoginu = words cf1
		solveWrite2 seznamLoginu cf2 f3
		hClose f1
		hClose f2
		hClose f3
		
solveWrite2 :: [[Char]] -> String -> Handle -> IO ()
solveWrite2 [] _ f3 = return () 
solveWrite2 seznamLoginu cf2 f3 = do
    let onelogin = concat $ take 1 seznamLoginu
    hPutStrLn f3 (solveCompareZnak2Znak onelogin cf2)
--     putStrLn (solveCompareZnak2Znak onelogin cf2)
    solveWrite2 (drop 1 seznamLoginu) cf2 f3

solveCompareZnak2Znak _ [] = []    
solveCompareZnak2Znak onelogin z@(x:xs) =  if (compText "xzzzzz99" z) 
					    then onelogin ++ (solveCompareZnak2Znak onelogin (drop 8 z))
					    else x : solveCompareZnak2Znak onelogin xs
-- 		
compText :: [Char] -> [Char] -> Bool
compText [] _ = True
compText (x:xs) (y:ys) = (x == y) && compText xs ys
compText _ _ = False 
----------------------------------------------------------------------------------------------------------- 
-- pouziti: gen "xlogin" "text" "text2"
gen :: FilePath -> FilePath -> FilePath -> IO ()
gen log txt res = do
		  logH <- openFile log ReadMode
		  txtH <- openFile txt ReadMode
		  resH <- openFile res WriteMode
		  logC <- hGetContents logH
		  txtC <- hGetContents txtH
		  --
		  hPutStr resH $ mkCont (lines logC) txtC
		  hClose resH
		  hClose txtH
		  hClose logH

mkCont [] _ = []
mkCont (l:ls) tx = modif l tx $ mkCont ls tx

modif _ [] app = app
modif l b@(c:cs) app =
		    if lEq "xzzzzz99" b
		    then l ++ modif l (drop 8 b) app
		    else c : modif l cs app
		       
lEq [] _ = True
lEq (x:xs) (y:ys) = x==y && lEq xs ys
lEq _ _ = False


 
----------------------------------------------------------------------------------------------------------- 
------------------------------------RADNA 2011 ------------------------------------------- 
----------------------------------------------------------------------------------------------------------- 

-- RADNA 2011 my 
-- pouziti: checkContents "test" "test2" 
checkContents fi fo = do
  f1 <- openFile fi ReadMode
  f2 <- openFile fo WriteMode
  cf1 <- hGetContents f1
  let (stav, pozice) = solveCheck cf1
--   let poziceR = snd (stav, pozice)
  if stav then do hPutStr f2 (show pozice)
	  else do putStr "Error\n" 
		  hPutStr f2 (show pozice)
-- 		  putStrLn (show pozice)
  hClose f1
  hClose f2


solveCheck cf
  | ((==) lenA lenB) && ((==) lenB lenC) && ((==) 0 (length zbytek3)) = (True,poz)
  | True = (False, pozErr)
  where
    -- delka pro C
    lenA = length $ fst $ span (== 'a') cf
    zbytek = snd $ span (== 'a') cf
    -- delka pro B
    lenB = length $ fst $ span (== 'b') zbytek
    zbytek2 = snd $ span (== 'b') zbytek
    -- delka pro C
    lenC = length $ fst $ span (== 'c') zbytek2
    zbytek3 = snd $ span (== 'c') zbytek2
    -- zjisteni pozice

    poz = lenA
    pozErr = if ((/=) lenA poz) then 1 else if ((/=) lenB poz) then poz+1 else if ((/=) lenC poz) then (poz*3)+1 else (poz*2)+1 
 


----------------------------------------------------------------------------------------------------------- 
-- pouziti: file_abc "test" "test2"
file_abc fi fo = do
		  handle_in <- openFile fi ReadMode
		  hadnle_out <- openFile fo WriteMode
		  content_filesIN <- hGetContents handle_in
		  -- Zkontrolovani posctu A^n B^n C^n
		  let (res,val) = solveVyrazABC content_filesIN
		  -- zapis do souboru
		  hPutStr hadnle_out (show val)
-- 		  putStrLn (show val)
		  if res then return () 
			 else hPutStr stderr "Error\n"
		  hClose hadnle_out
		  hClose handle_in
  
solveVyrazABC l
  | las==lbs && lbs==lcs && (length nocs==0) = (True,las)
  | True = (False,err)
  where
    (als,noas) = span (=='a') l
    las = length als
    --
    (bs,nobs) = span (=='b') noas
    lbs = length bs
    --
    (cs,nocs) = span (=='c') nobs
    lcs = length cs
    --
    err
      | las==0 = 1
      | las==lbs = if lcs>las then las+las+las+1 else las+las+lcs+1
      | True = if lbs>las then las+las+1 else las+lbs+1
      
----------------------------------------------------------------------------------------------------------- 
------------------------------------OPRAVKA 2011 ------------------------------------------- 
-----------------------------------------------------------------------------------------------------------

-- OPRAVKA 2011 my
wraps n fi fo = do
  f1 <- openFile fi ReadMode
  f2 <- openFile fo WriteMode
  cf1 <- hGetContents f1
  let seznamSlov = words cf1
--   print seznamSlov
  let pocatecniDelka = 0
  solveWraps n seznamSlov pocatecniDelka f2 
  hClose f1
  hClose f2

solveWraps n [] vs f2  = return ()
solveWraps 0 (x:xs) 0 f2 = do
  hPutStr f2 x
  solveWraps 0 xs 0 f2
  
solveWraps n (x:xs) vs f2  = do
  if check 
    then do hPutStr f2 (x ++ " ")
	    putStr (x ++ " ")
	    solveWraps n xs vsdelka f2 
    else do hPutStrLn f2 x
	    putStrLn x
	    solveWraps n xs 0 f2
  where
      vsdelka = (+) (length x) vs
      check = (<=) vsdelka n


-----------------------------------------------------------------------------------------------------------

shrink n fi fo = do
	hi <- openFile fi ReadMode
	ho <- openFile fo WriteMode
	cont <- hGetContents hi
	hPutStr ho $ unlines $ concat $ map (shrinkP n) $ par $ lines cont
	hClose ho
	hClose hi
 
par ls
	| null rst = [concat p1]
	| null p1 = [concat nosp]
	| True = concat p1 : par rst
	where
		nosp = dropWhile (== []) ls
		(p1, rest) = span (/= []) nosp
		rst = dropWhile (== []) rest
 
shrinkP _ [] = []
shrinkP width cs
	| null wds = [cs, ""]
	| True = join (head wds) (tail wds)
	where
		wds = words cs
		join l (w:ws)
			| length l + 1 + length w > width = l : join w ws
			| True = join (l ++ (' ':w)) ws
		join l [] = [l, ""]



      
----------------------------------------------------------------------------------------------------------- 
------------------------------------ RUZNE OSTATNI ------------------------------------------- 
-----------------------------------------------------------------------------------------------------------
{-
-- Jen pro pripomenuti:
data IOMode =  ReadMode | WriteMode | AppendMode | ReadWriteMode
getLine :: IO String
putStrLn :: String -> IO ()
type FilePath = [Char] -- tj. jméno souboru
openFile :: FilePath -> IOMode -> IO Handle
hIsEOF :: Handle -> IO Bool
hGetLine :: Handle -> IO String
hClose :: Handle -> IO ()
hGetContents :: Handle -> IO String
readFile :: FilePath -> IO String
lines :: String -> [String]
unlines :: [String] -> String
words :: String -> [String]
unwords :: [String] -> String
-}
 
-- Spocte pocet radku v souboru.
countLines file = do
  content <- readFile file
  putStrLn $ show $ length $ lines content
 
-- Spocte pocet slov v prvnich n radcich v souboru.
countWordsN file n = do
    content <- readFile file
    putStrLn $ show $ length $ words $ unlines $ take n $ lines content
 
-- Prokladane vypise obsah souboru na vystup.
prokladane file1 file2 = do
     h1 <- openFile file1 ReadMode
     h2 <- openFile file2 ReadMode
     c1 <- hGetContents h1
     c2 <- hGetContents h2
     write (lines c1) (lines c2)
     hClose h1
     hClose h2
   where
    write [] _ = return ()
    write _ [] = return ()
    write (x:xs) (y:ys) = do
      putStrLn x
      putStrLn y
      write xs ys 
 
 
 
-- Vytiskne dva soubory za sebou
vystup2souboru file1 file2 = do
     h1 <- openFile file1 ReadMode
     h2 <- openFile file2 ReadMode
     c1 <- hGetContents h1
     c2 <- hGetContents h2
     write (lines c1)
     write (lines c2)
     hClose h1
     hClose h2
      where
	write [] = return ()
	write (x:xs) = do
			putStrLn x
			write xs
 
-- Vypise obsah souboru s cisly radky.
printWithLineNumber file = do
    h <- openFile file ReadMode
    c <- hGetContents h
    write (lines c) 1
    hClose h
  where
    write [] _ = return ()
    write (x:xs) n = do
      putStrLn $ (show n) ++ ". " ++ x
      write xs (n+1)
 
-- Vypise radky na vystup, ktere jsou v obou souborech, ve stejnem poradi.
copyOut file1 file2 = do
    h1 <- openFile file1 ReadMode
    h2 <- openFile file2 ReadMode
    c1 <- hGetContents h1
    c2 <- hGetContents h2
    putStr $ unlines $ [x | x <- lines c1, y <- lines c2, x == y]
    hClose h1
    hClose h2
 
-- Nacte radek a slova vypise v opacnem poradi.
reverseOut = do
  line <- getLine
  if null line
    then return ()
    else do
      putStrLn $ rev line
      reverseOut
  where rev l = unwords $ foldl (\acc x -> x : acc) []  (words l)
  
  

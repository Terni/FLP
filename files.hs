import IO

-- main = file_abc test test2

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
  
 
solveCompare2 x []= putStr ("\n")
solveCompare2 x (y:ys) = if check   then do nahrad
					    solveCompare2 x ys
				     else do putStr (y ++ " ")
					     solveCompare2 x ys
    where
      check = (==) "xzzzzz99" y
      nahrad = putStr (x ++ " ")  

  
  
  
-- solveCompare :: Char -> [Char] -> IO ()
solveCompare x [] f3 = hPutStr f3 "\n"
solveCompare x (y:ys) f3 =if check   then do nahrad
					     solveCompare x ys f3
				     else do hPutStr f3 (y ++ " ")
					     solveCompare x ys f3
    where
      check = (==) "xzzzzz99" y
      nahrad = hPutStr f3 (x ++ " ")

      
-- TEST      
-- solveS (x:xs) = show (x ++ "\n") : solveS xs
 
		

file_abc fi fo = do
		  handle_in <- openFile fi ReadMode
		  hadnle_out <- openFile fo WriteMode
		  content_filesIN <- hGetContents handle_in
		  -- Zkontrolovani posctu A^n B^n B^n
		  let (res,val) = solveVyrazABC content_filesIN
		  -- zapis do souboru
		  hPutStr hadnle_out (show val)
		  if res then return () 
			 else hPutStr stderr "Error"
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
  
  

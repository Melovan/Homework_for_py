module Main 
    where

---------------------------------------------------------------------------------------------------------------------------------------

import System.Environment
import Data.Char ( digitToInt, ord, chr, isDigit )
import Control.Arrow (first,second,(&&&),(|||))
import Data.Array.Unboxed
    ( (!), (//), array, IArray(bounds), UArray )
import Control.Monad.State.Strict
    ( replicateM_,
      unless,
      when,
      MonadIO(liftIO),
      StateT (runStateT),
      gets,
      modify,
      evalStateT )
import System.Random
    ( getStdGen, mkStdGen, Random(randomR), StdGen )
import System.IO
    ( latin1,
      hSetEncoding,
      openFile,
      stderr,
      hGetContents,
      hGetLine ,
      hPutStr,
      hPutChar ,
      IOMode(ReadMode,WriteMode,ReadWriteMode,AppendMode), hClose )
import System.IO.Error ()
import Control.Exception ( try )

---------------------------------------------------------------------------------------------------------------------------------------

type Position = (Int,Int)

type Direction = Position -> Position

type Code = UArray Position Int

type Stack = [ Int ]

data ProgramState = 
    ES { 
         code :: Code,    
         stack :: Stack,
         position :: Position, 
         direction :: Direction,
         haltBit :: Bool,
         randGen :: StdGen 
        } 

defaultState =  ES  { 
         haltBit    = False,
         position   = (0,0), 
         direction  = right,
         stack      = [],
         randGen    = mkStdGen 31337, 
         code       = array ((0,0),(0,0)) []  }

type REPL a = StateT ProgramState IO a

newtype Opts = Opts { srcFile :: FilePath  }

defaultOpts = Opts "" 


--------------------------------------------------------------------------------

main :: IO ()
main = do 
    hdl <- openFile "C:\\Users\\Admin\\Desktop\\pr\\myproject\\output\\output.txt" WriteMode
    hPutStr hdl ""
    hPutStr stderr "Type a filename: "
    args <- getLine 
    opts <- either parsingErrors return (parseArgs args)
    state_i <- try $ processOpts opts
    either processingErrors (evalStateT evalLoop)  state_i
    
    
   
   
-------------------------------------------------------------------------------

evalLoop :: REPL ()
evalLoop = do
    getCmd >>= execute
    halting <- gets haltBit
    unless halting  (move >> evalLoop) 
                

stringModeLoop :: REPL ()
stringModeLoop = do 
    c <- getCmd
    unless (c == '"') $ do
           push (ord c)
           move
           stringModeLoop
    
    
---------------------------------------------------------------------------------------------------------------------------------------
execute :: Char -> REPL ()
execute c = 

    case c of
         '+' -> do (a,b) <- pop2
                   push (a+b)

         '-' -> do (a,b) <- pop2
                   push (b-a)
         
         '*' -> do (a,b) <- pop2
                   push (a*b)
         
         '/' -> do (a,b) <-pop2
                   safeZero  b quot a  >>= push 
         
         '%' -> do (a,b) <- pop2
                   safeZero  b rem a  >>= push  
                
         '!' -> do i <- pop
                   if i == 0 
                      then push 1
                      else push 0

         '`' -> do (a,b) <- pop2 
                   if b>a
                      then push 1
                      else push 0

         '$' -> do pop
                   return ()
         
         '.' -> do i <- pop
                   output1 i

         ',' -> do i <- pop
                   output2 i

         '\\' -> do (a,b) <- pop2
                    push a
                    push b
         
         ':' -> do a <- pop
                   replicateM_ 2 (push a)
         
         ' ' -> return ()    

         '>' -> setDirection right
        
         '<' -> setDirection left
       
         '^' -> setDirection up
      
         'v' -> setDirection down
         
         '?' -> getRandomDirection >>= setDirection 
     
         '#' -> move
    
         '@' -> halt
         
         '"' -> move  >>  stringModeLoop
                   
         
         '_' -> do a <- pop
                   if a == 0
                      then setDirection right
                      else setDirection left
         
         '|' -> do a <- pop
                   if a == 0
                      then setDirection down
                      else setDirection up
         

         'p' -> do (y,x) <- pop2
                   pop  >>= putCell (x,y) 
                   
         'g' -> do (y,x) <- pop2
                   getCell (x,y) >>= push

         '&' -> liftIO askInt >>= push
         
         '~' -> liftIO askChar >>= push

         x -> if isDigit x 
               then push (digitToInt x)
               else do loc <- gets position
                       halt
                       maybeSay$ "unknown command '" ++ show x ++ 
                                 "' in code path at " ++ show loc ++
                                 ". Maybe you want a funge-98 interpreter?"
    
--------------------------------------------------------------------------------

safeZero :: Int -> (Int -> Int -> Int) -> Int -> REPL Int
safeZero b func a =
       if a /= 0
          then return (b `func` a)
          else do liftIO $ putStr 
                     "\ndiv by zero; enter desired Int result: "
                  liftIO askInt
                  

pop :: REPL Int
pop = do 
    st <- gets stack                                    
    if null st                                      
       then return 0                                
       else do modify $ \s-> s{stack = tail st}     
               return (head st)                     

pop2 :: REPL (Int,Int)
pop2 = do a <- pop
          b <- pop
          return (a,b)

push :: Int -> REPL ()
push c = modify $ \s-> s{stack = c : stack s}
    
--------------------------------------------------------------------------------------------------------------------------------------

getCmd :: REPL Char    
getCmd = do p <- gets position 
            gets (chr . (! p) . code)    

getCell :: Position -> REPL Int
getCell xy = do
    xy' <- wrap xy
    unless (xy == xy') (maybeSay "warning: getCell out of bounds. wrapping.")
    gets ((! xy') . code)

putCell :: Position -> Int -> REPL ()
putCell xy c = do
    xy' <- wrap xy
    unless (xy == xy') (maybeSay "warning: putCell out of bounds. wrapping.")
    a <- gets code
    modify $ \s-> s{code = a//[(xy',c)] }

getRandomDirection :: REPL Direction
getRandomDirection = do
    (i,g) <- gets (randomR (0,3) . randGen)
    modify $ \s-> s{randGen = g}
    return ([up,down,left,right] !! i)

---------------------------------------------------------------------------------------------------------------------------------------

halt :: REPL ()
halt = modify $ \s-> s{ haltBit = True }

setDirection :: Direction -> REPL ()
setDirection d = modify $ \s-> s{direction = d}
       
move :: REPL ()
move = do
    (pos,mv) <- gets (position &&& direction)
    pos' <- wrapSimple (mv pos)
    modify $ \s-> s{position = pos'}

wrap :: Position -> REPL Position
wrap (pX,pY) = do
    (bX,bY) <- gets (snd . bounds . code)
    let x = pX `mod` (bX+1)
        y = pY `mod` (bY+1)
    return (x,y)

wrapSimple :: Position -> REPL Position
wrapSimple (pX,pY) = do
    (bX,bY) <- gets (snd . bounds . code)
    let x | pX < 0  = bX
          | pX > bX = 0
          | otherwise = pX
        y | pY < 0  = bY
          | pY > bY = 0
          | otherwise = pY
    return (x,y)

down, up, left, right :: Direction
down  = second (+1)
right = first  (+1)
left  = first  (subtract 1)
up    = second (subtract 1)

askInt :: IO Int
askInt = do hPutStr stderr "enter the INT\n"
            n <- getLine 
            if isInt n 
               then return (read n)
               else hPutStr stderr "NOT AN INT. Try again: \n" >> askInt
                    
askChar :: IO Int
askChar = do hPutStr stderr "enter the Char\n" 
             getChar >>= return . ord    

isInt :: String -> Bool
isInt = all isDigit

---------------------------------------------------------------------------------------------------------------------------------------

maybeSay :: String -> REPL ()
maybeSay m = do
    liftIO $ putStrLn m 
output1 :: Int -> REPL ()
output1 i = do
    handle <- liftIO $ openFile "C:\\Users\\Admin\\Desktop\\pr\\myproject\\output\\output.txt" AppendMode  
    liftIO $ hPutStr handle  $ show i ++ " "
    liftIO $ hClose handle

output2 :: Int-> REPL ()
output2 i = do
    handle <- liftIO $ openFile "C:\\Users\\Admin\\Desktop\\pr\\myproject\\output\\output.txt" AppendMode  
    liftIO $ hPutChar handle  $ chr i
    liftIO $ hClose handle    


---------------------------------------------------------------------------------------------------------------------------------------

processOpts :: Opts -> IO ProgramState
processOpts os = do  
   
   hdl <- openFile (srcFile os) ReadMode

   hSetEncoding hdl latin1 

   src <- hGetContents hdl

   let bigOK =  length src > 2000 

   let (wasBig,arr) = buildGrid bigOK src                   
   
   return$ defaultState{ code    = arr}

parseArgs :: FilePath -> Either IOError Opts
parseArgs [] = Left$ userError "SOURCE FILE REQUIRED:"
parseArgs as = parse as defaultOpts
    where  -- the source code file:
          parse src opts = Right$ opts{srcFile = src}
          parse _ _ = Left$ userError "ERROR PARSING OPTIONS"

---------------------------------------------------------------------------------------------------------------------------------------

buildGrid :: Bool -> String -> (Bool,Code)
buildGrid allowing_oversize str =
    let ls = lines $ fixLineBreaks str
        sxN = maximum $ 79 : map (subtract 1 . length) ls
        syN = max 24 (length ls - 1)
        (xN,yN) = if allowing_oversize == False 
                     then (sxN, syN)
                     else (79,24)
        pad = repeat ' '
        ls' = take (yN+1) (ls ++ repeat pad)
        coords = [ (x,y) | y <- [0..yN], x <- [0..xN] ]
        cells  = map ord $ concatMap (take (xN+1) . (++pad)) ls'
        big = sxN > 79 || syN > 24
        arr = array ((0,0),(xN,yN)) (zip coords cells)
     in (big, arr)

---------------------------------------------------------------------------------------------------------------------------------------

fixLineBreaks :: String -> String
fixLineBreaks [] = []
fixLineBreaks ('\r':'\n':xs) = '\n' : fixLineBreaks xs 
fixLineBreaks (x:xs)         = x : fixLineBreaks xs

processingErrors :: IOError -> IO ()
processingErrors e = do
    print e
    putStrLn "* Please report this bug to me so I can fix it *"

parsingErrors :: IOError -> IO a
parsingErrors = ioError

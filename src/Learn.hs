-- https://www.fpcomplete.com/school/to-infinity-and-beyond/pick-of-the-week/conduit-overview

import           Control.Monad.IO.Class
import           Control.Monad.State
import           Control.Monad.Trans.Resource
import           Data.Conduit
import qualified Data.Conduit.Binary          as CB
import qualified Data.Conduit.List            as CL
import           System.IO

source :: Source IO Int -- produces a stream of Ints
source = CL.sourceList [1..4]

sink :: Sink String IO () -- consumes a stream of Strings, no result
sink = CL.mapM_ putStrLn

by2 :: Monad m => Conduit Int m Int
by2 = CL.map (*2)

conduit :: Conduit Int IO String -- converts Ints into Strings
conduit = CL.map show

main :: IO ()
main = forM_ (zip [1..] [main1, main2, main3, main4, main5 4, main6, main7, main8, main9]) $ \(num, mainN) -> do
  putStrLn $ "main" ++ show num
  mainN
  putStrLn ""

main2 = do
  source $$ by2 =$= conduit =$ sink
  -- alternatively, with the same meaning
  source $= by2 =$= by2 =$= conduit $$ sink

main1 = do
  -- show Pure operations: summing numbers.
  result <- CL.sourceList [1..10] $$ CL.fold (+) 0
  print result
  -- /show

  -- show Exception safe file access: copy a file.
  writeFile "input.txt" "This is a test."
  runResourceT $ CB.sourceFile "input.txt" $$ CB.sinkFile "output.txt"
  readFile "output.txt" >>= putStrLn
  -- /show

  -- show Perform transformations.
  result <- CL.sourceList [1..10] $$ CL.map (+ 1) =$ CL.consume
  print result
  -- /show

source3 :: Source IO Int
source3 = do
    yield 1
    yield 2
    yield 3
    yield 4

conduit3 :: Conduit Int IO String
conduit3 = do
    -- Get all of the adjacent pairs from the stream
    mi1 <- await
    mi2 <- await
    case (mi1, mi2) of
        (Just i1, Just i2) -> do
            yield $ show (i1, i2)
            leftover i2
            conduit3
        _ -> return ()

sink3 :: Sink String IO ()
sink3 = do
    mstr <- await
    case mstr of
        Nothing -> return ()
        Just str -> do
            liftIO $ putStrLn str
            sink

sink3' :: Sink String IO ()
sink3' = awaitForever $ liftIO . putStrLn

sink3'' :: Sink String IO ()
sink3'' = myAwaitForever $ liftIO . putStrLn

main3 = source3 $$ conduit3 =$ sink3''

sourceList :: Monad m => [a] -> Source m a
sourceList [] = return ()
sourceList (x:xs) = yield x >> sourceList xs

myAwaitForever :: Monad m => (a -> Conduit a m b) -> Conduit a m b
myAwaitForever f = maybe (return ()) (\x -> f x >> myAwaitForever f) =<< await

triple :: Monad m => Conduit a m a
triple = do
    ma <- await
    case ma of
        Nothing -> return ()
        Just a -> do
            CL.sourceList [a, a, a]
            triple

triple' :: Monad m => Conduit a m a
triple' = awaitForever (sequence . map yield . replicate 3)

ex :: Monad m => Conduit Int m Int
ex = do
  mult <- await
  case mult of
    Nothing -> return ()
    Just m -> CL.map (*m)

so4 :: Source (State Int) Int
so4 = do
    x <- lift get
    if x <= 0
        then return ()
        else do
            yield x
            lift $ modify (\x -> x - 2)
            so4

co4 :: Conduit Int (State Int) (Int, Int)
co4 = awaitForever $ \i -> do
    lift $ modify (+ 1)
    x <- lift get
    yield (i, x)

main4 :: IO ()
main4 = do
    let result :: State Int [(Int, Int)]
        result = so4 $$ co4 =$ CL.consume
    print $ runState result 5


so5 = do
  liftIO $ putStrLn "source: yielding 1"
  yield 1
  liftIO $ putStrLn "source: yielding 2"
  yield 2
  liftIO $ putStrLn "source: yielding 3"
  yield 3

co5 = do
  liftIO $ putStrLn "conduit calling await"
  mx <- await
  case mx of
      Nothing -> liftIO $ putStrLn "Nothing left, exiting"
      Just x -> do
        liftIO $ putStrLn $ "conduit yielding " ++ show x
        yield x
        co5

si5 0 = liftIO $ putStrLn "sink is finished, terminating"
si5 i = do
  liftIO $ putStrLn $ "sink: still waiting for " ++ show i
  mx <- await
  case mx of
   Nothing -> liftIO $ putStrLn "sink: Nothing from upstream, exiting"
   Just x -> do
     liftIO $ putStrLn $ "sink received: " ++ show x
     si5 (i - 1)

main5 :: Int -> IO ()
main5 i = so5 $$ co5 =$ si5 i


so6 =
    loop 1
  where
    loop i = do
        yieldOr i $ putStrLn $ "Terminated when yielding: " ++ show i
        loop $ i + 1

main6 :: IO ()
main6 = so6 $$ CL.isolate 7 =$ CL.mapM_ print


so7 = do
    handle <- liftIO $ openFile "src/Learn.hs" ReadMode
    addCleanup (const $ putStrLn "Closing handle" >> hClose handle) $ loop handle
  where
    loop handle = do
        eof <- liftIO $ hIsEOF handle
        if eof
            then return ()
            else do
                c <- liftIO $ hGetChar handle
                yield c
                loop handle

main7 :: IO ()
main7 = so7 $$ CL.isolate 10 =$ CL.mapM_ print


so8 =
    bracketP
        (openFile "src/Learn.hs" ReadMode)
        (\handle -> putStrLn "Closing handle" >> hClose handle)
        loop
  where
    loop handle = do
        eof <- liftIO $ hIsEOF handle
        if eof
            then return ()
            else do
                c <- liftIO $ hGetChar handle
                yield c
                loop handle

exceptionalSink = do
    c <- await
    liftIO $ print c
    error "This throws an exception"

main8 :: IO ()
main8 = runResourceT $ so8 $$ exceptionalSink

main9 :: IO ()
main9 = do
    (rsrc1, result1) <- CL.sourceList [1..10] $$+ CL.take 3
    (rsrc2, result2) <- rsrc1 $$++ CL.take 3
    result3 <- rsrc2 $$+- CL.consume
    print (result1, result2, result3)

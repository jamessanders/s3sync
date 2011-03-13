module ArgumentParser (parseArgs) where

import Control.Monad.State
import System.Directory
import System.Environment
import System.FilePath 
import Types
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Maybe
import Data.List

tail' (x:xs) = xs
tail' []     = []


getConfig = do
  home <- getEnv "HOME"
  let configPath = home </> ".s3sync"
  configExist <- doesFileExist configPath
  if configExist 
    then readFile configPath >>= return . Just . parseConfig
    else return Nothing
  where 
    parseConfig config = ((takeWhile (/= ':') config),(tail $ dropWhile (/=':') config))
    
parseArgs = getArgs >>= parseArgs'    
parseArgs' args = do 
  config <- getConfig
  let env = case config of
        Just (ak, sk) -> blankEnv { accessKey = ak, secretKey = sk }
        Nothing       -> blankEnv
  penv <- checkEnv $ evalState (buildEnv env) args
  case penv of
    Left er -> error er
    Right x -> return x
  where
    checkEnv env | (accessKey env == "")                  = return (Left "No access key provided")
                 | (secretKey env == "")                  = return (Left "No secret key provided")
                 | (sourceResources env == [])            = return (Left "No source provided")
                 | (getBucket (targetResource env) == "") = return (Left "No local path provided")
                 | (archiveMode env == True)              = return (Left "Archive mode not yet implemented")

    
    checkEnv env = do
      now <- getCurrentTime >>= return . utcTimeToPOSIXSeconds
      return (Right $ env {  
                 backupTime = show now
                 })
    ------------------------------------------------------------------------
    
    getNextArg = do
      args <- get
      case args of
        [] -> return Nothing
        _  -> do
          put $ tail' args
          return $ Just $  head args
      
    getNext = do
      args <- get
      case args of
        [] -> return Nothing
        _ -> do
          let c = head args
          case take 2 c of
            "--" -> do
              put $ tail' args
              return $ Just (Flag $ drop 2 c)
            _ -> case take 1 c of
              "-" -> do
                let flag = head (drop 1 c)
                case tail' (drop 1 c) of
                  [] -> put (tail' args )
                  rest -> put (("-" ++ rest) : tail' args)
                return $ Just (Flag [flag])
              _ -> do 
                put (tail' args) 
                return $ Just (Arg $ head args)
              
    getRest = do
      l <- get
      put []
      return l
      
    buildEnv env = do
      next' <- getNext
      case next' of
        Just next -> do
          env' <- case next of
            Flag "a"          -> archiveMode
            Flag "r"          -> recursiveMode
            Flag "v"          -> verboseMode
            Flag "delete"     -> deleteMode
            Flag "secret-key" -> secretKey
            Flag "access-key" -> accessKey
            Flag "dry-run"    -> dryRunMode
            Flag "rr-storage" -> reducedRedMode    
            Flag "backup"     -> backupMode
            Arg x             -> getPaths x
            badArg            -> error $ "Invalid argument" ++ (show badArg)
          buildEnv env'
        Nothing -> return env
      where 
        archiveMode    = return $ env { archiveMode = True }
        recursiveMode  = return $ env { recursiveMode = True }
        deleteMode     = return $ env { deleteMode = True }
        verboseMode    = return $ env { verboseMode = True } 
        dryRunMode     = return $ env { dryRunMode = True }
        reducedRedMode = return $ env { reducedRedMode = True }
        
        accessKey     = do
          next <- getNextArg
          case next of
            (Just x) -> return $ env { accessKey = x }
            _ -> error "Invalid argument for access-key"
        
        secretKey = do
          next <- getNextArg
          case next of
            (Just x) -> return $ env { secretKey = x }
            _ -> error "Invalid argument for secret-key"
        
        backupMode = do
          next <- getNextArg
          case next of 
            (Just x) -> return $ env { backupResource = Just $ makeResource x }
            _ -> error "Invalid argument for backup"
        
        getPaths x = do
          rest  <- getRest 
          let paths = ([x] ++ rest)
          let source = map makeResource $ init paths
          let target = makeResource $ last paths
          return $ env { 
            sourceResources = source,
            targetResource  = target
            }
        
makeResource str | ":" `isInfixOf` str = 
  RemoteResource (takeWhile (/= ':') str) (fixRemotePath $ tail $ dropWhile (/= ':') str)
                 | otherwise           = LocalResource str
                                         
    where fixRemotePath = dropWhile (== '/') 

module Main where

import ArgumentParser
import Control.Monad
import Data.Foldable (foldlM, foldrM)
import Data.List (foldl')
import Network.AWS.S3Bucket
import Network.AWS.S3Object
import System.Directory
import System.FilePath 
import System.FilePath.Find hiding (fileSize)
import System.Posix.Files
import Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M

debug env st = when (verboseMode env) (putStrLn st)

runNewActions env = do
  debug env " = Getting remote listing..."
  realRemoteDirs <- filterM (doesDirectoryExist) (localPaths env) 
                    >>= return . map (\x-> if (last x == '/')
                                             then (remotePath env)
                                             else (remotePath env </> takeBaseName x))
  remoteFileList <- concat `fmap` mapM getS3Files realRemoteDirs 
  debug env " = Examining local filesystem..."
  expanded <- concat `fmap` foldrM expandAll [] (localPaths env)
  
  when (deleteMode env) $ do
    debug env " = Performing deletions..."
    runDeletions expanded remoteFileList

  debug env " = Running updates..."
  mapM_ (runNewUploads (makeMap remoteFileList)) expanded
  return ()
  
  where
    makeMap = foldl' (\a b -> M.insert (key b) b a) M.empty 
    expand path = do
      isFile <- isFile path
      case isFile of
        True  -> return [path]
        False -> expandDirectory path
        
    expandDirectory path = do
      find (return (recursiveMode env)) (fileType ==? RegularFile) path 
      
    isFile path = do
      doesFileExist path
        
    getS3Files path = do
      Right x <- listAllObjects 
                 (awsConnect env) 
                 (bucketName env) 
                 (ListRequest path "" "" 1)
      return x
      
    expandAll path accum = do
      let newDir = if last path == '/' then "" else takeBaseName path
      fileList <- expand path
      remoteNames <- forM fileList (\x -> do 
                                       isFile <- isFile path
                                       if (isFile) 
                                         then return ((remotePath env) </> takeFileName x)
                                         else return ((remotePath env) </> newDir </> makeRelative path x)) 
      return ((zip fileList remoteNames) : accum)


    runNewUploads s3fs x = do
      findChanges s3fs x >>= runAction 

    findInRemoteList lf rl = M.lookup lf rl

    inLocalList [] _ = False
    inLocalList ((_,rn):xs) s3f | rn == (key s3f) = True
                                | otherwise = inLocalList xs s3f

    runDeletions fl = do
      mapM_ (runAction . makeRemoteDeletion . key) . filter (not . inLocalList fl) 
                                        
    findChanges rfs (lf, rf) = do
      case (rf `findInRemoteList` rfs) of
        Nothing  -> (makeUpload lf rf)
        Just s3f -> do 
          dif <- compareMetaData lf s3f
          if dif 
            then makeUpload lf rf
            else return (Skip lf)          
            
    makeUpload lf rf = do
      let sm = if (reducedRedMode env) 
                 then REDUCED_REDUNDANCY
                 else STANDARD
      return $ Upload lf $ setStorageClass sm $ S3Object {
        obj_bucket   = (bucketName env),
        obj_name     = rf,
        content_type = "",
        obj_headers  = [],
        obj_data     = BL.empty       
        }
          
    makeRemoteDeletion s3f = RemoteDelete $ S3Object { 
      obj_bucket   = bucketName env,
      obj_name     = s3f,
      content_type = "",
      obj_headers  = [],
      obj_data     = BL.empty
      }
      
    compareMetaData lf s3f = do
      fs <- getFileStatus lf >>= return . fileSize
      return $ if (fromIntegral fs == size s3f) 
                 then False
                 else True

    runAction (Upload lf obj) = do                                                    
      putStrLn $ " + Uploading: " ++ lf ++ " -> " ++ obj_bucket obj ++ ":" ++ obj_name obj
      when (not $ dryRunMode env) $ do                                                    
        fdata <- BL.readFile lf                                                           
        sendObject (awsConnect env) (obj { obj_data = fdata })                            
        return ()                                                                         
              
    runAction (RemoteDelete obj) = do                                                 
      putStrLn $ " - Deleting: " ++ obj_bucket obj ++ ":" ++ obj_name obj                 
      when (not $ dryRunMode env) $ do                                                    
        deleteObject (awsConnect env) obj                                                 
        return ()                                                                         

    runAction (Skip _) = return ()

skips (Skip _) = True
skips _ = False

main = do
  env <- parseArgs 
  runNewActions env 
  putStrLn "Finished."

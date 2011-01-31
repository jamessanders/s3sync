import ArgumentParser
import Control.Monad
import Data.Foldable (foldlM)
import Network.AWS.S3Bucket
import Network.AWS.S3Object
import System.Directory
import System.FilePath 
import System.FilePath.Find hiding (fileSize)
import System.Posix.Files
import Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

getNewActions env = do
  realRemoteDirs <- filterM (doesDirectoryExist) (localPaths env) 
                    >>= return . map (\x-> if (last x == '/')
                                             then (remotePath env)
                                             else (remotePath env </> takeBaseName x))
  remoteFileList <- concat `fmap` mapM getS3Files realRemoteDirs 
  expanded <- foldM expandAll [] (localPaths env)
  updates  <- foldlM (getNewActions' remoteFileList) [] expanded
  deletions  <- if (deleteMode env) 
                  then findDeletions expanded remoteFileList 
                  else return ([] :: [Action])
  return (updates ++ deletions)
  where
    expand path = do
      isFile <- isFile path
      case isFile of
        True -> return [path]
        False -> expandDirectory path
        
    expandDirectory path = do
      find (return (recursiveMode env))  (return True) path >>= filterM isFile
        
      
    isFile path = do
      doesFileExist path
        
    getS3Files path = do
      Right x <- listAllObjects 
                 (awsConnect env) 
                 (bucketName env) 
                 (ListRequest path "" "" 1)
      return x
      
    expandAll accum path = do
      let newDir = if last path == '/' then "" else takeBaseName path
      fileList <- expand path
      remoteNames <- forM fileList (\x -> do 
                                       isFile <- isFile path
                                       if (isFile) 
                                         then return ((remotePath env) </> takeFileName x)
                                         else return ((remotePath env) </> newDir </> makeRelative path x)) 
      return (accum ++ zip fileList remoteNames)


    getNewActions' s3fs alist x = do
      diffs <- findChanges s3fs x
      return (alist ++ [diffs])

    findInRemoteList _ []      = Nothing
    findInRemoteList lf (x:xs) = 
      if (key x) == lf then Just x else findInRemoteList lf xs

    inLocalList [] _ = False
    inLocalList ((_,rn):xs) s3f | rn == (key s3f) = True
                                | otherwise = inLocalList xs s3f

    findDeletions fl = return . map (makeRemoteDeletion . key) . filter (not . inLocalList fl) 
                                        
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

runAction env (Upload lf obj) = do
  putStrLn $ " + Uploading: " ++ lf ++ " -> " ++ obj_bucket obj ++ ":" ++ obj_name obj
  when (not $ dryRunMode env) $ do
    fdata <- BL.readFile lf
    sendObject (awsConnect env) (obj { obj_data = fdata })
    return ()
    
runAction env (RemoteDelete obj) = do
  putStrLn $ " - Deleting: " ++ obj_bucket obj ++ ":" ++ obj_name obj
  when (not $ dryRunMode env) $ do
    deleteObject (awsConnect env) obj
    return ()

skips (Skip _) = True
skips _ = False

main = do
  env <- parseArgs 
  putStrLn "Calculating uploads..."
  new <- getNewActions env >>= return . filter (not . skips)
  case new of
    [] -> putStrLn " + No new changes."
    _  -> do
      mapM (runAction env) new 
      return ()
  putStrLn "Finished."

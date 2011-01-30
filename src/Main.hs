import ArgumentParser
import Control.Monad
import Data.Digest.Pure.MD5
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

hashFileLazyBS p = BL.readFile p >>= return . md5
               
getNewActions env = do
  let lfs = localPaths env
  Right rfs <- getS3Files
  foldlM (getNewActions' rfs) [] lfs 
  
  where
    expand f = do
      isfile <- isFile f
      case isfile of
        True -> return [f]
        False -> expandDirectory f
        
    expandDirectory f = do
      find (return (recursiveMode env))  (return True) f >>= filterM isFile
      
    isFile f = do
      doesFileExist f     
        
    getS3Files = do
      listAllObjects (awsConnect env) (bucketName env) (ListRequest (remotePath env) "" "" 1)
      
    getNewActions' s3fs alist lf = do
      let newDir = if last lf == '/' then "" else takeBaseName lf
      fl <- expand lf
      rns <- forM fl (\x -> do 
                         isfile <- isFile lf
                         if (isfile) 
                           then return ((remotePath env) </> takeFileName x)
                           else return ((remotePath env) </> newDir </> makeRelative lf x)) 
      
      diffs <- mapM (findChanges s3fs) $ zip fl rns
      return (alist ++ diffs)

    notInRemoteList _ []      = Nothing
    notInRemoteList lf (x:xs) = 
      if (key x) == lf then Just x else notInRemoteList lf xs
                                                                              
    findChanges rfs (lf, rf) = do
      case (rf `notInRemoteList` rfs) of
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
        obj_bucket = (bucketName env),
        obj_name   = rf,
        content_type = "",
        obj_headers = [],
        obj_data = BL.empty       
        }
          
    compareMetaData lf s3f = do
      fs <- getFileStatus lf >>= return . fileSize
      return $ if (fromIntegral fs == size s3f) 
                 then False
                 else True

runAction env (Upload lf obj) = do
  putStrLn $ " + Uploading: " ++ lf ++ " -> " ++ obj_name obj
  when (not $ dryRunMode env) $ do
    fdata <- BL.readFile lf
    sendObject (awsConnect env) (obj { obj_data = fdata })
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

{-# LANGUAGE OverlappingInstances #-}
module Main where

import ArgumentParser
import Control.Applicative ((<$>))
import Control.Monad
import Data.Foldable (foldlM, foldrM)
import Data.List (foldl')
import Data.Maybe (isJust, fromMaybe)
import Network.AWS.S3Bucket
import Network.AWS.S3Object
import System.Directory
import System.FilePath 
import System.FilePath.Find hiding (fileSize)
import System.Posix.Files
import Types
import Utils
import Data.Traversable (sequenceA)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M

debug env st = when (verboseMode env) (putStrLn st)

runNewActions env = do
  debug' " = Getting remote listing..."
  Right remoteFileList <- getRemoteFileList
  
  debug' " = Examining local filesystem..."
  localFileList <- getLocalFilesList
  
  if length localFileList < 1 
    then putStrLn "No local files found perhaps you meant to use -r" 
    else do
      when (deleteMode env) $ do                                   
        debug' " = Performing deletions..."                     
        runDeletions (makeLocalMap localFileList) remoteFileList        
                                                                   
      debug' " = Running updates..."                            
      runNewUploads (makeRemoteMap remoteFileList) localFileList
      return ()                                                    
  
  where
        
    debug' = debug env
    
    getRemoteFileList = do
      remoteDirs <- getRemoteDirectoryNames
      mapM getS3Files remoteDirs >>= return . fmap concat . sequenceA
    
    getRemoteDirectoryNames = do 
      filterM (doesDirectoryExist) (localPaths env) 
        >>= return . map determineRemoteDirectoryName
    
    determineRemoteDirectoryName path = 
      if (last path == '/')
        then (remotePath env)
        else (remotePath env </> takeBaseName path)

    getS3Files path = 
      listAllObjects 
        (awsConnect env) 
        (bucketName env) 
        (ListRequest path "" "" 1)
    
    makeRemoteMap = foldl' (\a b -> M.insert (key b) b a) M.empty 
    
    makeLocalMap  = foldl' (\a b -> M.insert (remoteName b) b a) M.empty

    getLocalFilesList = concat `fmap` foldrM expandAll [] (localPaths env)
    
    expandAll parent accum = do
      let newDir = if last parent == '/' then "" else takeBaseName parent
      let makeRemoteName child = 
            ifM (isFile parent)
            (return (remotePath env </> takeFileName child))
            (return (remotePath env </> newDir </> makeRelative parent child))
      fileList    <- expandPath parent
      remoteNames <- mapM makeRemoteName fileList 
      return ((zipWith makeFileMapping fileList remoteNames) : accum)

    expandPath path = do
      ifM (isFile path) (return [path]) (expandDirectory path)
        
    expandDirectory path = do
      find (return $ recursiveMode env) (fileType ==? RegularFile) path 
      
    isFile path = do
      doesFileExist path

    runDeletions lookupMap remoteFiles = 
      let 
        todel = filter ((flip M.notMember) lookupMap . key) remoteFiles
        makeAction path =
          (if (isJust $  backupBucket env)
             then [(makeRemoteCopy . key) path]
             else []) ++ [(makeRemoteDeletion . key) path] 
            
      in mapM_ runAction (concatMap makeAction todel)
                                    
    runNewUploads remoteLookupMap localFiles = 
      forM_ localFiles $ \localFile -> do
        changes <- findChanges localFile
        forM_ changes runAction
        where
          
        findChanges path = do                                                                               
          case (M.lookup (remoteName path) remoteLookupMap) of                                              
            Nothing  -> sequence [makeUpload path]                                                          
            Just remoteFile -> do                                                                           
              changed <- compareMetaData (localName path) remoteFile                                        
              if changed                                                                                    
                then uploadTheFile path                                                                     
                else return [Skip $ localName path]                                                         
                                                                                                            
        compareMetaData localFile remoteFile = do                                                                         
          fs <- fileSize <$> getFileStatus localFile 
          return $ (fromIntegral fs /= size remoteFile)                                                         
                                                                                                            
        uploadTheFile fileMapping = do                                                                      
          let backupAction = if isJust $ backupBucket env  
                               then [return $ makeRemoteCopy (remoteName fileMapping)] 
                               else []  
          sequence $ backupAction ++ [makeUpload fileMapping]                                               
                                                                                                            
    makeRemoteCopy rf = do       
      let path = fromMaybe "" (backupPath env)
          
          obj = S3Object { 
            obj_bucket   = bucketName env,
            obj_name     = rf,
            content_type = "",
            obj_headers  = [],
            obj_data     = BL.empty
            }
      
          cobj = S3Object { 
            obj_bucket   = fromMaybe "" (backupBucket env),
            obj_name     = path </> (backupTime env) </> rf,
            content_type = "",
            obj_headers  = [],
            obj_data     = BL.empty
            }
                 
        in RemoteCopy obj cobj

    makeUpload fileMapping = do
      let sm = if (reducedRedMode env) 
                 then REDUCED_REDUNDANCY
                 else STANDARD
      return $ Upload (localName fileMapping) $ setStorageClass sm $ S3Object {
        obj_bucket   = (bucketName env),
        obj_name     = remoteName fileMapping,
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
      
    runAction (Upload lf obj) = do                                                    
      putStrLn $ " + Uploading: " ++ lf ++ " -> " ++ obj_bucket obj ++ ":" ++ obj_name obj
      when (not $ dryRunMode env) $ do                                                    
        fdata <- BL.readFile lf                                                           
        sendObject (awsConnect env) (obj { obj_data = fdata })                            
        return ()                                                                         
              
    runAction (RemoteCopy obj cobj) = do
      putStrLn $ " + Copying: " ++ obj_bucket obj ++ ":" ++ obj_name obj ++ " -> " ++ obj_bucket cobj ++ ":" ++ obj_name cobj
      when (not $ dryRunMode env) (copyObject (awsConnect env) obj cobj >> return ())

    runAction (RemoteDelete obj) = do                                                
      putStrLn $ " - Deleting: " ++ obj_bucket obj ++ ":" ++ obj_name obj                 
      when (not $ dryRunMode env) $ do                                                    
        deleteObject (awsConnect env) obj                                                 
        return ()                                                                         

    runAction (Skip _) = return ()

main = do
  env <- parseArgs 
  runNewActions env 
  putStrLn "Finished."

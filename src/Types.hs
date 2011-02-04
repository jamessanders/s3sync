module Types where

import Network.AWS.AWSConnection
import Network.AWS.S3Object

type Bucket = String

data Resource = LocalResource FilePath
              | RemoteResource Bucket FilePath
              deriving (Show, Eq)

isLocal :: Resource -> Bool
isLocal (LocalResource _) = True
isLocal _ = False

isRemote :: Resource -> Bool
isRemote = not . isLocal

getBucket :: Resource -> Bucket
getBucket (RemoteResource b _) = b
getBucket _ = error "getBucket on local resource" 

getPath (RemoteResource _ a) = a
getPath (LocalResource  a)   = a

data S3SyncEnvironment = S3SyncEnvironment {
  sourceResources :: [Resource],  
  targetResource  :: Resource,    
  accessKey       :: String,      
  secretKey       :: String,      
  archiveMode     :: Bool,        
  recursiveMode   :: Bool,        
  deleteMode      :: Bool,        
  verboseMode     :: Bool,        
  dryRunMode      :: Bool,        
  reducedRedMode  :: Bool,        
  backupResource  :: Maybe Resource,   
  backupTime      :: String       
} deriving (Show)

blankEnv :: S3SyncEnvironment
blankEnv = S3SyncEnvironment {
  sourceResources = [],
  targetResource  = RemoteResource "" "",        
  accessKey       = "",     
  secretKey       = "",     
  archiveMode     = False,  
  recursiveMode   = False,  
  deleteMode      = False,  
  verboseMode     = False,  
  dryRunMode      = False,  
  reducedRedMode  = False,  
  backupResource  = Nothing,
  backupTime      = ""      
  }

awsConnect :: S3SyncEnvironment -> AWSConnection
awsConnect env = amazonS3Connection (accessKey env) (secretKey env)

data Arg = Arg String 
         | Flag String
         deriving (Show)

data Action = Skip FilePath
            | Upload FilePath S3Object
            | RemoteDelete S3Object
            | RemoteCopy S3Object S3Object
            deriving (Show)
                     
newtype FileMapping = FM (FilePath, FilePath) deriving (Show)

localName :: FileMapping -> FilePath
localName  (FM (ln, _)) = ln

remoteName :: FileMapping -> FilePath
remoteName (FM (_, rn)) = rn

makeFileMapping :: FilePath -> FilePath -> FileMapping
makeFileMapping a b = FM (a,b)
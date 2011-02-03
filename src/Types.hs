module Types where

import Network.AWS.AWSConnection
import Network.AWS.S3Object

data S3SyncEnvironment = S3SyncEnvironment {
  localPaths     :: [FilePath],
  bucketName     :: String,
  remotePath     :: FilePath,
  accessKey      :: String,
  secretKey      :: String,
  archiveMode    :: Bool,
  recursiveMode  :: Bool,
  deleteMode     :: Bool,
  verboseMode    :: Bool,
  dryRunMode     :: Bool,
  reducedRedMode :: Bool,
  backupBucket   :: Maybe String,
  backupPath     :: Maybe String,
  backupTime     :: String
} deriving (Show)

blankEnv = S3SyncEnvironment {
  localPaths     = [],
  bucketName     = "",
  remotePath     = [],
  accessKey      = "",
  secretKey      = "",
  archiveMode    = False,
  recursiveMode  = False,
  deleteMode     = False,
  verboseMode    = False,
  dryRunMode     = False,
  reducedRedMode = False,
  backupBucket   = Nothing,
  backupPath     = Nothing,
  backupTime     = ""
  }

awsConnect env = amazonS3Connection (accessKey env) (secretKey env)

data Arg = Arg String 
         | Flag String
         deriving (Show)

data Action = Skip FilePath
            | Upload FilePath S3Object
            | RemoteDelete S3Object
            | RemoteCopy S3Object S3Object
            deriving (Show)
                     

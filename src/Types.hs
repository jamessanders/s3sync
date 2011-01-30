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
  reducedRedMode :: Bool
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
  reducedRedMode = False
  }

awsConnect env = amazonS3Connection (accessKey env) (secretKey env)

data Arg = Arg String 
         | Flag String
         deriving (Show)

data Action = Skip FilePath
            | Upload FilePath S3Object
            | Delete FilePath S3Object
            deriving (Show)
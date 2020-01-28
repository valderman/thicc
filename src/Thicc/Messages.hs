-- | Message passing between thicc and thicc-rest.
--   The thicc-rest program sends requests over standard output, and receives
--   responses over standard input.
module Thicc.Messages
  ( Message (..), AppName, ApiKey
  , ThiccRequest (..), ThiccResponse (..)
  , PrivRequest (..), PrivResponse (..)
  ) where
import Thicc.Config
import Thicc.Pipes

-- | Requests that can be sent by the @thicc-rest@ daemon.
data ThiccRequest = Update AppName ApiKey
  deriving (Show, Read, Eq)
instance Message ThiccRequest where

-- | The possible responses to 'ThiccRequest's.
data ThiccResponse
  = UpdateOK
  | UpdateFailed
    deriving (Show, Read, Eq)
instance Message ThiccResponse where

-- | Message received over privileged channel.
data PrivRequest
  = Quit
  | Add AppName FilePath
  | Remove AppName
  | Enable AppName
  | Disable AppName
  | ListEnabled
  | ListAvailable
  | PrivUpdate AppName
  | URL AppName
    deriving (Show, Read, Eq)
instance Message PrivRequest

-- | Response to 'PrivRequest'.
data PrivResponse
  = OK (Maybe String)
  | Fail String
    deriving (Show, Read, Eq)
instance Message PrivResponse
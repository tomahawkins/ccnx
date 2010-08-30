-- | Content centric networking (CCN) is a network paradigm that
--   places the emphasis on data (content), not host connections.
--
--   This library implements the CCNx protocol developed by Parc:
--
--   <http://www.ccnx.org/>
module Network.CCNx
  ( module Network.CCNx.CCNb
  , module Network.CCNx.Messages
  ) where

import Network.CCNx.CCNb
import Network.CCNx.Messages


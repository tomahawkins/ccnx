module Network.CCN
  ( Message   (..)
  , Interest  (..)
  , Content   (..)
  , Qualifier (..)
  , Author
  , Title
  , Signature
  , Timestamp
  ) where

import Data.ByteString (ByteString)
import Data.Word

-- | Globally unique name associated with author of content.  Each author has a public key.
type Author = String

-- | Unique name of content by the author.  Author + Title = Globally unqiue content name.
type Title = String

-- | Signature algorithm:  hash author, title, timestamps, and content, then encrypt/decrypt with private/public key.
type Signature = String
type Timestamp = Word64

-- | An interest message consists of a content name (author + title) and a list of qualifications.
data Interest = Interest Author Title [Qualifier]

-- | A content message is the author, title, timestamp of content creation, time when to mark stale, the content, and the signature.
data Content  = Content Author Title Timestamp (Maybe Timestamp) ByteString Signature

-- | Content qualifiers in 'Interest' messages.
data Qualifier
  = SignatureIs Signature    -- ^ 'Content' signature equals ...
  | TimestampGT Timestamp    -- ^ 'Content' timestamp is greater than...
  | TimestampLT Timestamp    -- ^ 'Content' timestamp is less than...

data Message
  = InterestMessage Interest
  | ContentMessage  Content


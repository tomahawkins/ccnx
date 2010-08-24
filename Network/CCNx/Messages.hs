module Network.CCNx.Messages
  ( Message   (..)
  , ContentObject (..)
  , Content
  , Name
  , Component
  , SignedInfo (..)
  , Interest (..)
  , PublisherPublicKeyDigest
  , Exclude (..)
  , AnyBloom (..)
  , ChildSelector
  , AnswerOriginKind
  , Scope
  , Nonce
  , MinSuffixComponents
  , MaxSuffixComponents
  , Timestamp
  , FreshnessSeconds
  , FinalBlockID
  , Type (..)
  , KeyLocator (..)
  , Signature (..)
  , DigestAlgorithm
  , Witness
  , SignatureBits
  , PublisherID (..)
  ) where

import Data.ByteString (ByteString)
import Data.Word

-- | The two types of CCNx messages.
data Message
  = ContentObjectMessage  ContentObject
  | InterestMessage Interest

data ContentObject = ContentObject
  Signature
  Name
  SignedInfo
  Content

type Content = ByteString

type Name = [Component]

type Component = String

data SignedInfo = SignedInfo
  PublisherPublicKeyDigest
  Timestamp
  (Maybe Type)
  (Maybe FreshnessSeconds)
  (Maybe FinalBlockID)
  (Maybe KeyLocator)

data Interest = Interest
  Name
  (Maybe MinSuffixComponents)
  (Maybe MaxSuffixComponents)
  (Maybe PublisherID)
  (Maybe Exclude)
  (Maybe ChildSelector)
  (Maybe AnswerOriginKind)
  (Maybe Scope)
  (Maybe Nonce)

type PublisherPublicKeyDigest = String

data Exclude = Exclude (Maybe AnyBloom) [(Component, Maybe AnyBloom)]   

data AnyBloom = Any | Bloom String

type ChildSelector = Int
type AnswerOriginKind = Int
type Scope = Int
type Nonce = String
type MinSuffixComponents = Int
type MaxSuffixComponents = Int
type Timestamp = Word16  -- 12 bit fraction
type FreshnessSeconds = Int
type FinalBlockID = String

data Type = DATA | ENCR | GONE | KEY | LINK | NACK

data KeyLocator
  = Key String
  | Certificate String
  | KeyName Name (Maybe PublisherID)

data Signature = Signature
  (Maybe DigestAlgorithm)
  (Maybe Witness)
  SignatureBits

type DigestAlgorithm = String
type Witness = String
type SignatureBits = String

data PublisherID
  = PublisherIDPublisherPublicKeyDigest PublisherPublicKeyDigest
  | PublisherCertificateDigest String
  | PublisherIssuerKeyDigest String
  | PublisherIssuerCertificateDigest String


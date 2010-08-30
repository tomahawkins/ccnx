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

-- | The two types of CCNx messages.
data Message
  = ContentObjectMessage  ContentObject
  | InterestMessage Interest
  deriving Show

data ContentObject = ContentObject
  Signature
  Name
  SignedInfo
  Content
  deriving Show

type Content = ByteString

-- | The name of the content.
type Name = [Component]

-- | A component of a 'Name'.
type Component = ByteString

data SignedInfo = SignedInfo
  PublisherPublicKeyDigest
  Timestamp
  (Maybe Type)
  (Maybe FreshnessSeconds)
  (Maybe FinalBlockID)
  (Maybe KeyLocator)
  deriving Show

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
  deriving Show

type PublisherPublicKeyDigest = String

data Exclude = Exclude (Maybe AnyBloom) [(Component, Maybe AnyBloom)]   deriving Show

data AnyBloom = Any | Bloom String deriving Show

type ChildSelector = Int
type AnswerOriginKind = Int
type Scope = Int
type Nonce = String
type MinSuffixComponents = Int
type MaxSuffixComponents = Int

-- | Seconds since the start of Unix time.
type Timestamp = Int

type FreshnessSeconds = Int
type FinalBlockID = String

data Type = DATA | ENCR | GONE | KEY | LINK | NACK deriving Show

data KeyLocator
  = Key String
  | Certificate String
  | KeyName Name (Maybe PublisherID)
  deriving Show

data Signature = Signature
  (Maybe DigestAlgorithm)
  (Maybe Witness)
  SignatureBits
  deriving Show

type DigestAlgorithm = String
type Witness = String
type SignatureBits = String

data PublisherID
  = PublisherIDPublisherPublicKeyDigest PublisherPublicKeyDigest
  | PublisherCertificateDigest String
  | PublisherIssuerKeyDigest String
  | PublisherIssuerCertificateDigest String
  deriving Show


-- | Parsing and printing the CCNx binary format (ccnb).
module Network.CCNx.CCNb
  ( parseCCNb
  , printCCNb
  ) where

import Data.ByteString (ByteString)

import Network.CCNx.BinaryXML
import Network.CCNx.Messages

-- | Parse a ccnb message.
parseCCNb :: ByteString -> Message
parseCCNb = parseMessage . parseBinaryXML

parseMessage :: Block -> Message
parseMessage a = case a of
  DTAG i _ | tag i == Interest' -> InterestMessage $ Interest [] Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
  a -> error $ "unexpected block: " ++ show a

-- | Print a ccnb message.
printCCNb :: Message -> ByteString
printCCNb a = printBinaryXML $ case a of
  ContentObjectMessage a -> printContentObject a
  InterestMessage      a -> printInterest      a

printContentObject (ContentObject sig name signed content) =
 DTAG (tagCode ContentObject') [printSignature sig, printName name, printSignedInfo signed, printContent content]

data Tag
  = Any'
  | Name'
  | Component'
  | Certificate'
  | Collection'
  | CompleteName'
  | Content'
  | SignedInfo'
  | ContentDigest'
  | ContentHash'
  | Count'
  | Header'
  | Interest'
  | Key'
  | KeyLocator'
  | KeyName'
  | Length'
  | Link'
  | LinkAuthenticator'
  | NameComponentCount'
  | RootDigest'
  | Signature'
  | Start'
  | Timestamp'
  | Type'
  | Nonce'
  | Scope'
  | Exclude'
  | Bloom'
  | BloomSeed'
  | AnswerOriginKind'
  | Witness'
  | SignatureBits'
  | DigestAlgorithm'
  | BlockSize'
  | FreshnessSeconds'
  | FinalBlockID'
  | PublisherPublicKeyDigest'
  | PublisherCertificateDigest'
  | PublisherIssuerKeyDigest'
  | PublisherIssuerCertificateDigest'
  | ContentObject'
  | WrappedKey'
  | WrappingKeyIdentifier'
  | WrapAlgorithm'
  | KeyAlgorithm'
  | Label'
  | EncryptedKey'
  | EncryptedNonceKey'
  | WrappingKeyName'
  | Action'
  | FaceID'
  | IPProto'
  | Host'
  | Port'
  | MulticastInterface'
  | ForwardingFlags'
  | FaceInstance'
  | ForwardingEntry'
  | MulticastTTL'
  | MinSuffixComponents'
  | MaxSuffixComponents'
  | ChildSelector'
  | RepositoryInfo'
  | Version'
  | RepositoryVersion'
  | GlobalPrefix'
  | LocalName'
  | Policy'
  | Namespace'
  | GlobalPrefixName'
  | PolicyVersion'
  | KeyValueSet'
  | KeyValuePair'
  | IntegerValue'
  | DecimalValue'
  | StringValue'
  | BinaryValue'
  | NameValue'
  | Entry'
  | ACL'
  | ParameterizedName'
  | Prefix'
  | Suffix'
  | Root'
  | ProfileName'
  | Parameters'
  | CCNProtocolDataUnit'
  deriving (Show, Eq)

tagCodes :: [(Int, Tag)]
tagCodes =
  [ (13       , Any'                               )
  , (14       , Name'                              )
  , (15       , Component'                         )
  , (16       , Certificate'                       )
  , (17       , Collection'                        )
  , (18       , CompleteName'                      )
  , (19       , Content'                           )
  , (20       , SignedInfo'                        )
  , (21       , ContentDigest'                     )
  , (22       , ContentHash'                       )
  , (24       , Count'                             )
  , (25       , Header'                            )
  , (26       , Interest'                          )
  , (27       , Key'                               )
  , (28       , KeyLocator'                        )
  , (29       , KeyName'                           )
  , (30       , Length'                            )
  , (31       , Link'                              )
  , (32       , LinkAuthenticator'                 )
  , (33       , NameComponentCount'                )
  , (36       , RootDigest'                        )
  , (37       , Signature'                         )
  , (38       , Start'                             )
  , (39       , Timestamp'                         )
  , (40       , Type'                              )
  , (41       , Nonce'                             )
  , (42       , Scope'                             )
  , (43       , Exclude'                           )
  , (44       , Bloom'                             )
  , (45       , BloomSeed'                         )
  , (47       , AnswerOriginKind'                  )
  , (53       , Witness'                           )
  , (54       , SignatureBits'                     )
  , (55       , DigestAlgorithm'                   )
  , (56       , BlockSize'                         )
  , (58       , FreshnessSeconds'                  )
  , (59       , FinalBlockID'                      )
  , (60       , PublisherPublicKeyDigest'          )
  , (61       , PublisherCertificateDigest'        )
  , (62       , PublisherIssuerKeyDigest'          )
  , (63       , PublisherIssuerCertificateDigest'  )
  , (64       , ContentObject'                     )
  , (65       , WrappedKey'                        )
  , (66       , WrappingKeyIdentifier'             )
  , (67       , WrapAlgorithm'                     )
  , (68       , KeyAlgorithm'                      )
  , (69       , Label'                             )
  , (70       , EncryptedKey'                      )
  , (71       , EncryptedNonceKey'                 )
  , (72       , WrappingKeyName'                   )
  , (73       , Action'                            )
  , (74       , FaceID'                            )
  , (75       , IPProto'                           )
  , (76       , Host'                              )
  , (77       , Port'                              )
  , (78       , MulticastInterface'                )
  , (79       , ForwardingFlags'                   )
  , (80       , FaceInstance'                      )
  , (81       , ForwardingEntry'                   )
  , (82       , MulticastTTL'                      )
  , (83       , MinSuffixComponents'               )
  , (84       , MaxSuffixComponents'               )
  , (85       , ChildSelector'                     )
  , (86       , RepositoryInfo'                    )
  , (87       , Version'                           )
  , (88       , RepositoryVersion'                 )
  , (89       , GlobalPrefix'                      )
  , (90       , LocalName'                         )
  , (91       , Policy'                            )
  , (92       , Namespace'                         )
  , (93       , GlobalPrefixName'                  )
  , (94       , PolicyVersion'                     )
  , (95       , KeyValueSet'                       )
  , (96       , KeyValuePair'                      )
  , (97       , IntegerValue'                      )
  , (98       , DecimalValue'                      )
  , (99       , StringValue'                       )
  , (100      , BinaryValue'                       )
  , (101      , NameValue'                         )
  , (102      , Entry'                             )
  , (103      , ACL'                               )
  , (104      , ParameterizedName'                 )
  , (105      , Prefix'                            )
  , (106      , Suffix'                            )
  , (107      , Root'                              )
  , (108      , ProfileName'                       )
  , (109      , Parameters'                        )
  , (17702112 , CCNProtocolDataUnit'               )
  ]

-- Tag from tag code.
tag :: Int -> Tag
tag a = case lookup a tagCodes of
  Nothing -> error $ "unkown tag code: " ++ show a
  Just t  -> t

-- Code from tag.
tagCode :: Tag -> Int
tagCode a = case lookup a table of
  Nothing -> error $ "unkown tag: " ++ show a
  Just c  -> c
  where
  (a, b) = unzip tagCodes
  table = zip b a


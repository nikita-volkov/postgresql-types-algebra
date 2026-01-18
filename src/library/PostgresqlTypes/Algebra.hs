module PostgresqlTypes.Algebra where

import qualified Data.Attoparsec.Text as Attoparsec
import Data.ByteString (ByteString)
import Data.Tagged (Tagged (..), untag)
import Data.Text (Text)
import Data.Word (Word32)
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import TextBuilder (TextBuilder)
import qualified TextBuilder
import Prelude

-- | Evidence that a type maps to a PostgreSQL scalar value.
class IsScalar a where
  -- | PostgreSQL schema name, if applicable.
  schemaName :: Tagged a (Maybe Text)

  -- | PostgreSQL type name.
  typeName :: Tagged a Text

  -- | PostgreSQL type OID, if known at compile time.
  baseOid :: Tagged a (Maybe Word32)
  baseOid = Tagged Nothing

  -- | PostgreSQL array type OID, if known at compile time.
  arrayOid :: Tagged a (Maybe Word32)
  arrayOid = Tagged Nothing

  -- | Static type parameters. E.g., the @n@ in @char(n)@.
  typeParams :: Tagged a [Text]
  typeParams = Tagged []

  -- | Get the PostgreSQL type signature for a given Haskell type.
  --
  -- In case of parameterized types, the type parameters are included in the signature.
  -- For example, for @'PostgresqlTypes.Types.Bpchar.Bpchar' 10@, the signature will be @bpchar(10)@.
  typeSignature :: Tagged a Text
  typeSignature =
    let params = untag (typeParams @a)
        name = untag (typeName @a)
     in Tagged
          if null params
            then name
            else
              TextBuilder.toText
                ( mconcat
                    [ TextBuilder.text name,
                      "(",
                      TextBuilder.intercalateMap ", " TextBuilder.text params,
                      ")"
                    ]
                )

  -- | Encode the value in PostgreSQL binary format.
  binaryEncoder :: a -> Write.Write

  -- | Decode the value from PostgreSQL binary format.
  binaryDecoder :: PtrPeeker.Variable (Either DecodingError a)

  -- | Represent the value in PostgreSQL textual format.
  textualEncoder :: a -> TextBuilder.TextBuilder

  -- | Decode the value from PostgreSQL textual format.
  textualDecoder :: Attoparsec.Parser a

-- | Evidence that a type can be used as an element of a PostgreSQL range type.
class (IsScalar a, Ord a) => IsRangeElement a where
  -- | PostgreSQL range type name.
  rangeTypeName :: Tagged a Text

  -- | Statically known OID for the range type.
  rangeBaseOid :: Tagged a (Maybe Word32)

  -- | Statically known OID for the range array-type.
  rangeArrayOid :: Tagged a (Maybe Word32)

-- | Evidence that a type can be used as an element of a PostgreSQL multirange type.
class (IsRangeElement a) => IsMultirangeElement a where
  -- | PostgreSQL multirange type name.
  multirangeTypeName :: Tagged a Text

  -- | Statically known OID for the multirange type.
  multirangeBaseOid :: Tagged a (Maybe Word32)

  -- | Statically known OID for the multirange array-type.
  multirangeArrayOid :: Tagged a (Maybe Word32)

class (IsScalar scalar) => ProjectsToScalar scalar subject | subject -> scalar where
  projectToScalar :: subject -> Either RefinementError scalar
  ejectFromScalar :: scalar -> Either RefinementError subject

-- | Automatically provides instances via the identity projection for all scalar types.
--
-- Thus it serves as the preferred more general interface for adapters.
-- It however requires support for richer semantics than the ones provided by 'IsScalar' alone from the adapters.
-- It adds encoding errors, which are inevitable for mappings to many practical types as they are often wider than their closest Postgres counterparts.
instance (IsScalar scalar) => ProjectsToScalar scalar scalar where
  projectToScalar = Right
  ejectFromScalar = Right

data DecodingError = DecodingError
  { location :: [Text],
    reason :: DecodingErrorReason
  }
  deriving stock (Show, Eq)

data DecodingErrorReason
  = ParsingDecodingErrorReason
      -- | Details.
      Text
      -- | Input.
      ByteString
  | UnexpectedValueDecodingErrorReason
      -- | Expected.
      Text
      -- | Actual.
      Text
  | -- | Unsupported server-side value.
    --
    -- Useful for defining decoders on a narrower type than the underlying PostgreSQL type.
    --
    -- This one does not signal a problem with the data itself, but rather that the data does not meet
    -- the additional constraints imposed by the refinement.
    RefinementDecodingErrorReason RefinementError
  deriving stock (Show, Eq)

data RefinementError = RefinementError
  { location :: [Text],
    reason :: Text,
    subject :: Text
  }
  deriving stock (Show, Eq)

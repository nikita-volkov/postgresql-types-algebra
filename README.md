# postgresql-types-algebra

[![Hackage](https://img.shields.io/hackage/v/postgresql-types-algebra.svg)](https://hackage.haskell.org/package/postgresql-types-algebra)
[![Continuous Haddock](https://img.shields.io/badge/haddock-master-blue)](https://nikita-volkov.github.io/postgresql-types-algebra/)

Core type classes and algebra for PostgreSQL type mappings, extracted from [postgresql-types](https://github.com/nikita-volkov/postgresql-types).

## Overview

This package provides the fundamental abstractions for mapping Haskell types to PostgreSQL types:

- **Type classes** defining the algebra for PostgreSQL type mappings
- **Error types** for decoding failures
- **Metadata support** for type names, OIDs, and parameters
- **No concrete implementations** - just the algebra

## Type Classes

### `IsStandardType`

The core type class for types that map to PostgreSQL values:

```haskell
class IsStandardType a where
  -- Type metadata
  typeName :: Tagged a Text
  baseOid :: Tagged a (Maybe Word32)
  arrayOid :: Tagged a (Maybe Word32)
  typeParams :: Tagged a [Text]
  typeSignature :: Tagged a Text
  
  -- Binary format
  binaryEncoder :: a -> Write.Write
  binaryDecoder :: PtrPeeker.Variable (Either DecodingError a)
  
  -- Textual format
  textualEncoder :: a -> TextBuilder.TextBuilder
  textualDecoder :: Attoparsec.Parser a
```

This class enables:
- **Binary encoding/decoding** using PostgreSQL's native binary format
- **Textual encoding/decoding** using PostgreSQL's text representation  
- **Type metadata** including OIDs and type signatures for parameterized types
- **Round-trip fidelity** through all encoding combinations

### `IsRangeElement`

For types that can be elements of PostgreSQL range types:

```haskell
class (IsStandardType a, Ord a) => IsRangeElement a where
  rangeTypeName :: Tagged a Text
  rangeBaseOid :: Tagged a (Maybe Word32)
  rangeArrayOid :: Tagged a (Maybe Word32)
```

### `IsMultirangeElement`

For types that can be elements of PostgreSQL multirange types (PostgreSQL 14+):

```haskell
class IsRangeElement a => IsMultirangeElement a where
  multirangeTypeName :: Tagged a Text
  multirangeBaseOid :: Tagged a (Maybe Word32)
  multirangeArrayOid :: Tagged a (Maybe Word32)
```

## Error Handling

The package defines structured error types for decoding failures:

```haskell
data DecodingError = DecodingError
  { location :: [Text]  -- Error location path
  , reason :: DecodingErrorReason
  }

data DecodingErrorReason
  = ParsingDecodingErrorReason Text ByteString
  | UnexpectedValueDecodingErrorReason Text Text
  | UnsupportedValueDecodingErrorReason Text Text
  | RefinementDecodingErrorReason Text Text
```

These error types provide:
- **Location tracking** for nested types (arrays, composites)
- **Detailed failure reasons** distinguishing parsing, validation, and refinement errors
- **Server value preservation** for debugging

## Use Cases

### For Library Authors

Use this package to:
- Define custom PostgreSQL type mappings compatible with the postgresql-types ecosystem
- Create adapter libraries for different PostgreSQL client libraries
- Build generic tools that work with any `IsStandardType` instance

### For Application Developers

This package is typically used indirectly through:
- [postgresql-types](https://github.com/nikita-volkov/postgresql-types) - Concrete type implementations
- [hasql-postgresql-types](https://github.com/nikita-volkov/hasql-postgresql-types) - Hasql integration
- [postgresql-simple-postgresql-types](https://github.com/nikita-volkov/postgresql-simple-postgresql-types) - postgresql-simple integration

## Design Philosophy

This package extracts the pure algebra from postgresql-types to:

1. **Minimize dependencies** - No concrete PostgreSQL types, just the abstraction
2. **Enable extensibility** - Define your own types implementing the algebra
3. **Support multiple backends** - Adapter packages can use this common algebra
4. **Maintain type safety** - All conversions are explicit and lawful

## Related Packages

- [postgresql-types](https://github.com/nikita-volkov/postgresql-types) - Complete PostgreSQL type mappings using this algebra
- [hasql-postgresql-types](https://github.com/nikita-volkov/hasql-postgresql-types) - Hasql integration
- [postgresql-simple-postgresql-types](https://github.com/nikita-volkov/postgresql-simple-postgresql-types) - postgresql-simple integration

# postgresql-types-algebra

[![Hackage](https://img.shields.io/hackage/v/postgresql-types-algebra.svg)](https://hackage.haskell.org/package/postgresql-types-algebra)
[![Continuous Haddock](https://img.shields.io/badge/haddock-master-blue)](https://nikita-volkov.github.io/postgresql-types-algebra/)

Core type classes and algebra for PostgreSQL type mappings, extracted from ["postgresql-types"](https://github.com/nikita-volkov/postgresql-types).

## Overview

This package provides the fundamental abstractions for mapping Haskell types to PostgreSQL types:

- **Type classes** defining the algebra for PostgreSQL type mappings
- **Error types** for decoding failures
- **Metadata support** for type names, OIDs, and parameters
- **No concrete implementations** - just the algebra

## Type Classes

### `IsScalar`

The core type class for types that map to PostgreSQL values:

```haskell
class IsScalar a where
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

## Use Cases

### For Library Authors

Use this package to:
- Define custom PostgreSQL type mappings compatible with the "postgresql-types" ecosystem
- Create adapter libraries for different PostgreSQL client libraries
- Build generic tools that work with any `IsScalar` instance

### For Application Developers

This package is typically used indirectly through:
- ["postgresql-types"](https://github.com/nikita-volkov/postgresql-types) - Concrete type implementations
- ["hasql-postgresql-types"](https://github.com/nikita-volkov/hasql-postgresql-types) - "hasql" integration
- ["postgresql-simple-postgresql-types"](https://github.com/nikita-volkov/postgresql-simple-postgresql-types) - "postgresql-simple" integration

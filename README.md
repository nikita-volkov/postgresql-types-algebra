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

### `IsPrimitive`

The core type class for types that map to PostgreSQL values:

```haskell
class IsPrimitive a where
  -- Type metadata
  typeName :: Tagged a Text
  baseOid :: Tagged a (Maybe Word32)
  arrayOid :: Tagged a (Maybe Word32)
  typeParams :: Tagged a [Text]
  typeSignature :: Tagged a Text

  -- Textual format
  textualEncoder :: a -> TextBuilder.TextBuilder
  textualDecoder :: Attoparsec.Parser a
```

### `IsBinaryPrimitive`

Evidence that a type additionally has a PostgreSQL binary wire format. Not every PostgreSQL type
supports binary transmission — some only have textual `send`/`receive` functions registered on the
server — so this is a separate, optional subclass rather than part of `IsPrimitive` itself:

```haskell
class (IsPrimitive a) => IsBinaryPrimitive a where
  binaryEncoder :: a -> Write.Write
  binaryDecoder :: PtrPeeker.Variable (Either DecodingError a)
```

These classes enable:
- **Binary encoding/decoding** using PostgreSQL's native binary format, where supported
- **Textual encoding/decoding** using PostgreSQL's text representation
- **Type metadata** including OIDs and type signatures for parameterized types
- **Round-trip fidelity** through all encoding combinations

## Use Cases

### For Library Authors

Use this package to:
- Define custom PostgreSQL type mappings compatible with the "postgresql-types" ecosystem
- Create adapter libraries for different PostgreSQL client libraries
- Build generic tools that work with any `IsPrimitive` (and, where applicable, `IsBinaryPrimitive`) instance

### For Application Developers

This package is typically used indirectly through:
- ["postgresql-types"](https://github.com/nikita-volkov/postgresql-types) - Concrete type implementations
- ["hasql-postgresql-types"](https://github.com/nikita-volkov/hasql-postgresql-types) - "hasql" integration
- ["postgresql-simple-postgresql-types"](https://github.com/nikita-volkov/postgresql-simple-postgresql-types) - "postgresql-simple" integration

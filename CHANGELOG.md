# Changelog

## Upcoming

### Breaking

- Renamed `IsScalar` to `IsPrimitive`.
- Split `binaryEncoder`/`binaryDecoder` out of `IsPrimitive` into a new subclass, `IsBinaryPrimitive`. A type that has no PostgreSQL binary wire format (some types only have textual `send`/`receive` functions registered on the server) simply omits an `IsBinaryPrimitive` instance, rather than being forced to provide one. Existing instances that define both binary and textual methods need to split their `instance IsScalar T where ...` into `instance IsPrimitive T where ...` (textual methods and metadata) and `instance IsBinaryPrimitive T where ...` (binary methods).

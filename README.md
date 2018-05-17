[![Build Status](https://travis-ci.org/zotonic/jsxrecord.svg?branch=master)](https://travis-ci.org/zotonic/jsxrecord)

# JSON encoding with records and 'null'/'undefined' mapping

This is a wrapper around `jsx` to handle encoding and decoding of Erlang records.

## JSON null handling

To ease the interop between Erlang and JSON the 'null' handling is changed:

    Erlang    ->    JSON     ->     Erlang

    undefined       null            undefined
    null            null            undefined


## How to use

Before records can be encoded or decoded the record definitions need to be loaded.

After the definitions are loaded then all encoding/decoding is done transparently.

### Loading record definitions

The record definitions are loaded from modules and compiled into a runtime loaded module
containing all field names and default values.

To add record definitions from `mymodule`

    ok = jsxrecord:load_records( [ mymodule ]).

To see the current record definitions:

    jsxrecord:record_defs()

This returns a map with all known record definitions.

### Encoding/decoding records

Let's assume the following record definition has been loaded:

    -record(test, { a = 1, b = 2, c }).

This can now be encoded with:

    jsxrecord:encode( #test{} ).

The resulting JSON is:

    {"_record":"test","a":1,"b":2,"c":null}

Decoding returns the `#test{}`:

    #test{} = jsxrecord:decode(<<"{\"_record\":\"test\",\"a\":1,\"b\":2,\"c\":null}">>).

Defaults are automatically added for fields missing in the JSON:

    #test{ a = 1, b = 2, c = undefined } = jsxrecord:decode(<<"{\"_record\":\"test\"}">>).


## Configuration

Set the application env `jsxrecord.record_modules` to a list of modules whose records need to
be loaded on first use of the encoder or decoder.


## Performance

The input of encode and the output of decode are parsed and expanded.
This makes the encoder and decoder slower than pure `jsx`.
Though the difference shouldn't be too bad in normal usage.

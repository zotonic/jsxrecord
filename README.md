![Test](https://github.com/zotonic/jsxrecord/workflows/Test/badge.svg)

# JSON encoding with records and 'null'/'undefined' mapping

Originally, this was a wrapper around `jsx` to handle encoding and decoding of Erlang records, but [euneus](https://github.com/williamthome/euneus) gives to
jsxrecord a better performance.

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

### Encoding and decoding datetime and timestamp tuples

Datetime tuples are assumed to be in UTC, and are converted into an ISO8601 string:

    <<"\"2008-12-10T13:30:00Z\"">> = jsxrecord:encode({{2008, 12, 10}, {13, 30, 0}})

They are converted back into a datetime tuple:

    {{2008, 12, 10}, {13, 30, 0}} = jsxrecord:decode(<<"\"2008-12-10T13:30:00Z\"">>)

Erlang timestamp tuples are also converted into an ISO8601 string, but with added precision:

    <<"\"2020-06-12T14:00:11.571Z\"">> = jsxrecord:encode({1591,970411,571321})

A little bit of precision is lost when converting it back to a timestamp tuple:

    {1591,970411,571000} = jsxrecord:decode(<<"\"2020-06-12T14:00:11.571Z\"">>)


## Configuration

Set the application env `jsxrecord.record_modules` to a list of modules whose records need to
be loaded on first use of the encoder or decoder.


## Performance

The input of encode and the output of decode are parsed and expanded.
This makes the encoder and decoder slower than pure `jsx`.
Though the difference shouldn't be too bad in normal usage.

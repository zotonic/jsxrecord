-module(jsxrecord_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-record(test, {
    a = 1,
    b = 2,
    c
}).

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

all() ->
    [
        undefined_value,
        records,
        records_nested,
        record_defaults,
        dates,
        proplist
    ].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

undefined_value(_Config) ->
    <<"{\"a\":null}">> = jsxrecord:encode( #{ a => undefined } ),
    #{ <<"a">> := undefined } = jsxrecord:decode( <<"{\"a\":null}">> ),
    ok.

records(_Config) ->
    ok = jsxrecord:load_records(?MODULE),
    #test{} = jsxrecord:decode( jsxrecord:encode( #test{} ) ),
    #test{ a = <<"a">> } = jsxrecord:decode( jsxrecord:encode( #test{ a = a } ) ),
    #test{ a = undefined } = jsxrecord:decode( jsxrecord:encode( #test{ a = undefined } ) ),
    ok.

records_nested(_Config) ->
    #test{ a = #test{} } = jsxrecord:decode( jsxrecord:encode( #test{ a = #test{} } ) ),
    <<"{\"a\":{\"_record\":\"test\",\"a\":1,\"b\":2,\"c\":null}}">> = jsxrecord:encode(#{ a => #test{} }),
    #{ <<"a">> := #test{} } = jsxrecord:decode( jsxrecord:encode(#{ a => #test{} }) ),
    ok.

record_defaults(_Config) ->
    #test{ a = 1, b = 2, c = undefined } = jsxrecord:decode( <<"{\"_record\":\"test\"}">> ),
    ok.

dates(_Config) ->
    <<"\"2008-12-10T13:30:00Z\"">> = jsxrecord:encode({{2008, 12, 10}, {13, 30, 0}}),
    <<"2008-12-10T13:30:00Z">> = jsxrecord:decode(<<"\"2008-12-10T13:30:00Z\"">>),
    ok.

proplist(_Config) ->
    <<"{\"a\":1}">> = jsxrecord:encode([ {a, 1} ]),
    ok.

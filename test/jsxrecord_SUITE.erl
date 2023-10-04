-module(jsxrecord_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-record(test, {
    a = 1,
    b = 2,
    c
}).

-record(trans, {
    tr = []
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
        times,
        proplist,
        record_proplist
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
    <<"{\"a\":{\"_type\":\"test\",\"a\":1,\"b\":2,\"c\":null}}">> = jsxrecord:encode(#{ a => #test{} }),
    #{ <<"a">> := #test{} } = jsxrecord:decode( jsxrecord:encode(#{ a => #test{} }) ),
    ok.

record_defaults(_Config) ->
    #test{ a = 1, b = 2, c = undefined } = jsxrecord:decode( <<"{\"_type\":\"test\"}">> ),
    ok.

dates(_Config) ->
    <<"\"2008-12-10T13:30:00Z\"">> = jsxrecord:encode({{2008, 12, 10}, {13, 30, 0}}),
    {{2008, 12, 10}, {13, 30, 0}} = jsxrecord:decode(<<"\"2008-12-10T13:30:00Z\"">>),

    <<"[\"2008-12-10T13:30:00Z\",\"2008-12-10T13:30:00Z\"]">> =
        jsxrecord:encode([{{2008, 12, 10}, {13, 30, 0}}, {{2008, 12, 10}, {13, 30, 0}}]),
    [{{2008, 12, 10}, {13, 30, 0}}, {{2008, 12, 10}, {13, 30, 0}}] =
        jsxrecord:decode(<<"[\"2008-12-10T13:30:00Z\",\"2008-12-10T13:30:00Z\"]">>),

    ok.

times(_Config) ->
    <<"\"2020-06-12T14:00:11.571Z\"">> = jsxrecord:encode({1591,970411,571321}),
    % We loose a little bit of precision, but that is ok.
    {1591,970411,571000} = jsxrecord:decode( <<"\"2020-06-12T14:00:11.571Z\"">> ),
    ok.

proplist(_Config) ->
    <<"{\"a\":1}">> = jsxrecord:encode([ {a, 1} ]),
    ok.

record_proplist(_Config) ->
    Tr = #trans{ tr = [ {en, <<"hello">>} ]},
    Json = jsxrecord:encode(Tr),
    Tr = jsxrecord:decode(Json),
    ok.

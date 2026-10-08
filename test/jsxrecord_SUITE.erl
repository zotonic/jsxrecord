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
        decode_options,
        records,
        records_nested,
        record_defaults,
        dates,
        times,
        proplist,
        atom_list,
        record_proplist,
        record_lists,
        heterogeneous_lists,
        mixed_list,
        unknown_term
    ].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

undefined_value(_Config) ->
    <<"{\"a\":null}">> = encode( #{ a => undefined } ),
    #{ <<"a">> := undefined } = decode( <<"{\"a\":null}">> ),
    ok.

decode_options(_Config) ->
    ok = jsxrecord:load_records(?MODULE),
    Plain = #{codecs => [], records => false, null => null},
    Json = <<"{\"date\":\"2008-12-10T13:30:00Z\",\"values\":[null,{\"_type\":\"test\"},{\"_type\":\"_tuple\",\"_list\":[1,2]}]}">>,
    #{<<"date">> := <<"2008-12-10T13:30:00Z">>,
      <<"values">> := [null, #{<<"_type">> := <<"test">>},
          #{<<"_type">> := <<"_tuple">>, <<"_list">> := [1,2]}]} = jsxrecord:decode(Json, Plain),
    #{<<"date">> := {{2008,12,10},{13,30,0}},
      <<"values">> := [undefined, #test{}, {1,2}]} = jsxrecord:decode(Json, #{}),
    #test{} = jsxrecord:decode(<<"{\"_type\":\"test\"}">>, #{codecs => []}),
    null = jsxrecord:decode(<<"null">>, #{null => null}),
    undefined = jsxrecord:decode(undefined, Plain),
    undefined = jsxrecord:decode(<<>>, Plain),
    ok.

records(_Config) ->
    ok = jsxrecord:load_records(?MODULE),
    #test{} = decode( encode( #test{} ) ),
    #test{ a = <<"a">> } = decode( encode( #test{ a = a } ) ),
    #test{ a = undefined } = decode( encode( #test{ a = undefined } ) ),
    ok.

records_nested(_Config) ->
    #test{ a = #test{} } = decode( encode( #test{ a = #test{} } ) ),
    JSON = encode(#{ a => #test{} }),
    #{
        <<"a">> := #{
            <<"_type">> := <<"test">>,
            <<"a">> := 1,
            <<"b">> := 2,
            <<"c">> := null
        }
    } = json:decode(JSON),
    #{ <<"a">> := #test{} } = decode( encode(#{ a => #test{} }) ),
    ok.

record_defaults(_Config) ->
    #test{ a = 1, b = 2, c = undefined } = decode( <<"{\"_type\":\"test\"}">> ),
    ok.

dates(_Config) ->
    <<"\"2008-12-10T13:30:00Z\"">> = encode({{2008, 12, 10}, {13, 30, 0}}),
    {{2008, 12, 10}, {13, 30, 0}} = decode(<<"\"2008-12-10T13:30:00Z\"">>),

    <<"[\"2008-12-10T13:30:00Z\",\"2008-12-10T13:30:00Z\"]">> =
        encode([{{2008, 12, 10}, {13, 30, 0}}, {{2008, 12, 10}, {13, 30, 0}}]),
    [{{2008, 12, 10}, {13, 30, 0}}, {{2008, 12, 10}, {13, 30, 0}}] =
        decode(<<"[\"2008-12-10T13:30:00Z\",\"2008-12-10T13:30:00Z\"]">>),

    ok.

times(_Config) ->
    <<"\"2020-06-12T14:00:11.571Z\"">> = encode({1591,970411,571321}),
    % We loose a little bit of precision, but that is ok.
    {1591,970411,571000} = decode( <<"\"2020-06-12T14:00:11.571Z\"">> ),
    ok.

proplist(_Config) ->
    <<"{\"a\":1}">> = encode([ {a, 1} ]),
    ok.

atom_list(_Config) ->
    <<"[\"a\",\"b\"]">> = encode([ a, b ]),
    ok.

record_proplist(_Config) ->
    Tr = #trans{ tr = [ {en, <<"hello">>} ]},
    Json = encode(Tr),
    Tr = decode(Json),
    ok.

%% Two-element records must remain records even at the start of a list.
record_lists(_Config) ->
    ok = jsxrecord:load_records(?MODULE),
    Tr = #trans{tr = [{nl, <<"Hallo">>}]},
    Caption = #{<<"caption">> => #trans{tr = [{en, <<"Hello">>}]}},
    lists:foreach(
        fun(Value) -> Value = decode(encode(Value)) end,
        [
            [Tr],
            [Tr, #trans{}],
            [Tr, Caption],
            [Caption, Tr],
            [{<<"key">>, 1}, Tr],
            #{<<"blocks">> => [#{<<"items">> => [Tr, Caption]}]}
        ]),
    % Binary keys remain available when a property name is also a record name.
    #{<<"trans">> := 1, <<"test">> := 2, <<"3">> := 3} =
        decode(encode([{<<"trans">>, 1}, {test, 2}, {3, 3}])),
    ok.

%% Checking just the first tuple silently discards later non-property values.
heterogeneous_lists(_Config) ->
    lists:foreach(
        fun(Value) -> Value = decode(encode(Value)) end,
        [
            [{<<"key">>, 1}, #{<<"caption">> => <<"Hello">>}],
            [{<<"key">>, 1}, 42],
            [{<<"key">>, 1}, [1, 2]]
        ]),
    ok.

mixed_list(_Config) ->
    L = [{n,7},
         {mean,166347},
         {min,750},
         {max,828167},
         {median,880},
         {50,880},
         {75,1143},
         {90,828167},
         {95,828167},
         {99,828167},
         {999,828167}],
    E = [
        "\"n\":7",
        "\"mean\":166347",
        "\"min\":750",
        "\"max\":828167",
        "\"median\":880",
        "\"50\":880",
        "\"75\":1143",
        "\"90\":828167",
        "\"95\":828167",
        "\"99\":828167",
        "\"999\":828167"
    ],
    JSON = encode(L),
    [ match = re:run(JSON, RE, [{capture, none}]) || RE <- E ],
    ok.

unknown_term(_Config) ->
    <<"null">> = encode(self()),
    <<"null">> = encode(make_ref()),
    <<"[null,1,null]">> = encode([ make_ref(), 1, self() ]).

%%--------------------------------------------------------------------
%% SUPPORT FUNCTIONS
%%--------------------------------------------------------------------

encode(Source) ->
    jsxrecord:encode(Source).

decode(Bin) ->
    jsxrecord:decode(Bin).

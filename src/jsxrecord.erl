%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2018-2024 Marc Worrell
%% @doc JSON with records and 'undefined'/'null' mapping.
%% @end

%% Copyright 2018-2024 Marc Worrell
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(jsxrecord).

-author('Marc Worrell <marc@worrell.nl>').

-export([
    encode/1,
    decode/1,

    load_records/1,
    record_defs/0
]).

-define(RECORD_TYPE, <<"_type">>).
-define(IS_PROPLIST_KEY(X), is_binary(X) orelse is_atom(X) orelse is_integer(X)).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% API
%%====================================================================

-spec encode( term() ) -> binary().
encode(Source) ->
    iolist_to_binary(encode_json(Source)).

-spec decode( binary() | undefined ) -> term().
decode(undefined) ->
    undefined;
decode(Bin) when is_binary(Bin) ->
    decode_json(Bin).

%% @doc Load all record definitions.
-spec record_defs() -> map().
record_defs() ->
    try jsxrecord_defs:defs()
    catch _:_ ->
        _ = application:start(jsxrecord),
        {ok, Ms} = application:get_env(jsxrecord, record_modules),
        ok = do_load_records(Ms, #{}),
        jsxrecord_defs:defs()
    end.

-spec load_records( module() | list( module( )) ) -> ok.
load_records(Module) when is_atom(Module) ->
    load_records([ Module ]);
load_records(Modules) ->
    do_load_records(Modules, record_defs_int()).

%%====================================================================
%% Internal
%%====================================================================

%% @doc Load all record definitions.
-spec record_defs_int() -> map().
record_defs_int() ->
    try
        erlang:apply(jsxrecord_defs, defs, [])
    catch _:_ ->
        #{}
    end.

do_load_records(Modules, CurrRecordDefs) ->
    Records = lists:flatten( lists:map( fun(M) -> extract_records(M) end, Modules ) ),
    New = lists:foldl(
        fun({Name, Fs}, Acc) ->
            FsB = [ {atom_to_binary(F, utf8), Init} || {F,Init} <- Fs ],
            Acc#{ atom_to_binary(Name, utf8) => FsB }
        end,
        CurrRecordDefs,
        Records),
    compile_module(New).

encode_json(Term) ->
    Options = #{
        codecs => [ timestamp, datetime ],
        nulls => [undefined, null],
        skip_values => [],
        key_to_binary => fun key_to_binary/1,
        proplists => {true, fun is_proplist/1},
        encode_tuple => fun encode_tuple/2,
        encode_pid => fun encode_unknown/2,
        encode_port => fun encode_unknown/2,
        encode_reference => fun encode_unknown/2,
        encode_term => fun encode_unknown/2
    },
    euneus:encode_to_iodata(Term, Options).

decode_json(<<>>) -> undefined;
decode_json(B) ->
    Options = #{
        codecs => [ timestamp, datetime ],
        null => undefined,
        object_finish => fun reconstitute_records/2
    },
    euneus:decode(B, Options).

key_to_binary(Bin) when is_binary(Bin) ->
    Bin;
key_to_binary(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, utf8);
key_to_binary(Int) when is_integer(Int) ->
    integer_to_binary(Int, 10).

is_proplist([{K, _} | _]) when ?IS_PROPLIST_KEY(K) ->
    true;
is_proplist(_List) ->
    false.

encode_tuple({struct, MochiJSON}, Opts) ->
    Map = mochijson_to_map(MochiJSON),
    euneus_encoder:encode_map(Map, Opts);
encode_tuple(R, _Opts) when is_tuple(R), is_atom(element(1, R)) ->
    T = atom_to_binary(element(1, R), utf8),
    case maps:find(T, record_defs()) of
        {ok, Def} ->
            encode_json(expand_record_1(
                Def, 2, R, #{ ?RECORD_TYPE => T }
            ));
        error ->
            encode_json(#{
                ?RECORD_TYPE => <<"_tuple">>,
                <<"_list">> => tuple_to_list(R)
            })
    end;
encode_tuple(T, _Opts) ->
    encode_json(#{
        ?RECORD_TYPE => <<"_tuple">>,
        <<"_list">> => tuple_to_list(T)
    }).

encode_unknown(Term, _State) ->
    ?LOG_WARNING(#{
        in => jsxrecord,
        text => <<"Error mapping value to JSON - replaced with 'null'">>,
        result => error,
        reason => json_token,
        token => Term
    }),
    <<"null">>.

reconstitute_records(Acc, OldAcc) ->
    MaybeRecord = case maps:from_list(Acc) of
        #{ ?RECORD_TYPE := <<"_tuple">>, <<"_list">> := Elts } ->
            list_to_tuple(Elts);
        #{ ?RECORD_TYPE := Type } = Map ->
            case maps:find(Type, record_defs_int()) of
                {ok, Def} ->
                    Rec = lists:foldl(
                        fun({F, Default}, RecAcc) ->
                            V1 = case maps:get(F, Map, Default) of
                                V when is_map(V), is_list(Default) ->
                                    make_proplist(V);
                                V ->
                                    V
                            end,
                            [ V1 | RecAcc ]
                        end,
                        [ binary_to_atom(Type, utf8) ],
                        Def),
                    list_to_tuple( lists:reverse(Rec) );
                error ->
                    Map
            end;
        Map ->
            Map
    end,
    {MaybeRecord, OldAcc}.

make_proplist(Map) ->
    L = maps:to_list(Map),
    lists:map(
        fun
            ({K,V}) when is_binary(K) ->
                try
                    {binary_to_existing_atom(K, utf8), V}
                catch
                    _:_ -> {K, V}
                end;
            (KV) ->
                KV
        end,
        L).

expand_record_1([ {F, _} | Fs ], N, R, Acc) ->
    Acc1 = Acc#{ F => element(N, R) },
    expand_record_1(Fs, N+1, R, Acc1);
expand_record_1([], _N, _R, Acc) ->
    Acc.

mochijson_to_map({struct, L}) ->
    maps:from_list([ mochijson_to_map(V) || V <- L ]);
mochijson_to_map({K, V}) ->
    {K, mochijson_to_map(V)};
mochijson_to_map(V) ->
    V.

%% @doc Compile the record defs to a module, for effictient caching of all definitions
-spec compile_module( map() ) -> ok.
compile_module( Defs ) ->
    {ok, Module, Bin} = compile(Defs),
    code:purge(Module),
    {module, _} = code:load_binary(Module, "jsxrecord_defs.erl", Bin),
    ok.

-spec compile( map() ) -> {ok, atom(), binary()}.
compile(Defs) ->
    ModuleAst = erl_syntax:attribute(erl_syntax:atom(module), [ erl_syntax:atom(jsxrecord_defs) ]),
    ExportAst = erl_syntax:attribute(
                    erl_syntax:atom(export),
                    [ erl_syntax:list([
                            erl_syntax:arity_qualifier(erl_syntax:atom(defs), erl_syntax:integer(0))
                        ])
                    ]),
    FunAst = erl_syntax:function(
            erl_syntax:atom(defs),
            [ erl_syntax:clause([], none, [ erl_syntax:abstract(Defs) ]) ]),
    Forms = [ erl_syntax:revert(X) || X <- [ ModuleAst, ExportAst, FunAst ] ],
    {ok, Module, Bin} = compile:forms(Forms, []),
    {ok, Module, Bin}.

-spec extract_records( module() ) -> list( {atom(), list(atom())} ).
extract_records(Module) ->
    case code:which(Module) of
        BeamFile when is_list(BeamFile) ->
            case beam_lib:chunks(BeamFile, [ abstract_code ]) of
                {ok, {_, [ {abstract_code, {_, AbstractCode }} ]} } ->
                    extract_records_abs(AbstractCode);
                _ ->
                    []
            end;

        _Other ->
            []
    end.

%% @doc Extract all record definitions from the abstract code
extract_records_abs( AbstractCode ) ->
   lists:filtermap(
        fun
            ({attribute, _Pos, record, {Name, Fields}}) ->
                {true, {Name, to_field_names(Fields)}};
            (_) ->
                false
        end,
        AbstractCode).

to_field_names(Fields) ->
    [ to_field_name(Field) || Field <- Fields ].

to_field_name({typed_record_field, RecField, _Type}) ->
    to_field_name(RecField);
to_field_name({record_field, _Line, {atom, _, FieldName}}) ->
    {FieldName, undefined};
to_field_name({record_field, _Line, {atom, _, FieldName}, InitExpr}) ->
    {FieldName, erl_syntax:concrete(InitExpr)}.

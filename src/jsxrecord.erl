%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2018-2023 Marc Worrell
%% @doc JSON with records and 'undefined'/'null' mapping. Wrapper around jsx.
%% @end

%% Copyright 2018-2023 Marc Worrell
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

-define(IS_NUMBER(C), C >= $0, C =< $9).


-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% API
%%====================================================================

-spec encode( term() ) -> binary().
encode(Source) ->
    encode_json(Source).

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


encode_json(undefined) -> <<"null">>;
encode_json(null) -> <<"null">>;
encode_json(true) -> <<"true">>;
encode_json(false) -> <<"false">>;
encode_json({struct, _} = MochiJSON) ->
    encode_json( mochijson_to_map(MochiJSON) );
encode_json(Term) ->
    Options = [
        {error_handler, fun jsx_error/3}
    ],
    jsx:encode(expand_records(Term), Options).

decode_json(<<>>) -> undefined;
decode_json(<<"null">>) -> undefined;
decode_json(<<"true">>) -> true;
decode_json(<<"false">>) -> false;
decode_json(B) -> reconstitute_records( jsx:decode(B, [return_maps]) ).

jsx_error([T|Terms], {parser, State, Handler, Stack}, Config) ->
    ?LOG_ERROR(#{
        in => jsxrecord,
        text => <<"Error mapping value to JSON">>,
        result => error,
        reason => json_token,
        token => T
    }),
    Config1 = jsx_config:parse_config(Config),
    jsx_parser:resume([null|Terms], State, Handler, Stack, Config1);
jsx_error(_Terms, _Error, _Config) ->
    erlang:error(badarg).


reconstitute_records( M ) when is_map(M) ->
    M1 = maps:map( fun(_K, V) -> reconstitute_records(V) end, M ),
    case maps:find(?RECORD_TYPE, M1) of
        {ok, Type} ->
            case maps:find(Type, record_defs_int()) of
                {ok, Def} ->
                    Rec = lists:foldl(
                        fun({F, Default}, Acc) ->
                            V1 = case maps:get(F, M1, Default) of
                                V when is_map(V), is_list(Default) ->
                                    make_proplist(V);
                                V ->
                                    V
                            end,
                            [ V1 | Acc ]
                        end,
                        [ binary_to_atom(Type, utf8) ],
                        Def),
                    list_to_tuple( lists:reverse(Rec) );
                error ->
                    M1
            end;
        error ->
            M1
    end;
reconstitute_records( L ) when is_list(L) ->
    [ reconstitute_records(X) || X <- L ];
reconstitute_records( null ) ->
    undefined;
reconstitute_records( <<Y4, Y3, Y2, Y1, $-, M2, M1, $-, D2, D1, $T, H2, H1, $:, Min2, Min1, $:, S2, S1, $., Mil3, Mil2, Mil1, $Z>> )
  when ?IS_NUMBER(Y4), ?IS_NUMBER(Y3), ?IS_NUMBER(Y2), ?IS_NUMBER(Y1),
       ?IS_NUMBER(M2), ?IS_NUMBER(M1),
       ?IS_NUMBER(D2), ?IS_NUMBER(D1),
       ?IS_NUMBER(H2), ?IS_NUMBER(H1),
       ?IS_NUMBER(Min2), ?IS_NUMBER(Min1),
       ?IS_NUMBER(S2), ?IS_NUMBER(S1),
       ?IS_NUMBER(Mil3), ?IS_NUMBER(Mil2), ?IS_NUMBER(Mil1) ->
    DateTime = {{chars_to_integer(Y4, Y3, Y2, Y1), chars_to_integer(M2, M1), chars_to_integer(D2, D1)},
                {chars_to_integer(H2, H1), chars_to_integer(Min2, Min1), chars_to_integer(S2, S1)}},
    MilliSeconds = chars_to_integer(Mil3, Mil2, Mil1),
    Seconds = calendar:datetime_to_gregorian_seconds(DateTime) - 62167219200,
    %% 62167219200 == calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}})
    {Seconds div 1000000, Seconds rem 1000000, MilliSeconds * 1000};
reconstitute_records( <<Y4, Y3, Y2, Y1, $-, M2, M1, $-, D2, D1, $T, H2, H1, $:, Min2, Min1, $:, S2, S1, $Z>> )
  when ?IS_NUMBER(Y4), ?IS_NUMBER(Y3), ?IS_NUMBER(Y2), ?IS_NUMBER(Y1),
       ?IS_NUMBER(M2), ?IS_NUMBER(M1),
       ?IS_NUMBER(D2), ?IS_NUMBER(D1),
       ?IS_NUMBER(H2), ?IS_NUMBER(H1),
       ?IS_NUMBER(Min2), ?IS_NUMBER(Min1),
       ?IS_NUMBER(S2), ?IS_NUMBER(S1) ->
    {{chars_to_integer(Y4, Y3, Y2, Y1), chars_to_integer(M2, M1), chars_to_integer(D2, D1)},
     {chars_to_integer(H2, H1), chars_to_integer(Min2, Min1), chars_to_integer(S2, S1)}};
reconstitute_records( T ) ->
    T.

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

expand_records(R) when is_tuple(R), is_atom(element(1, R)) ->
    T = atom_to_binary(element(1, R), utf8),
    case maps:find(T, record_defs()) of
        {ok, Def} ->
            expand_record_1(Def, 2, R, #{ ?RECORD_TYPE => T });
        error ->
            R
    end;
expand_records({MegaSecs, Secs, MicroSecs}=Timestamp) when is_integer(MegaSecs) andalso is_integer(Secs) andalso is_integer(MicroSecs) ->
    MilliSecs = MicroSecs div 1000, 
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:now_to_datetime(Timestamp),
    unicode:characters_to_binary(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B.~3.10.0BZ",
                                               [Year, Month, Day, Hour, Min, Sec, MilliSecs]));

expand_records({{Year,Month,Day},{Hour,Minute,Second}}) when is_integer(Year) andalso is_integer(Month) andalso is_integer(Second) andalso
                                           is_integer(Hour) andalso is_integer(Minute) andalso is_integer(Second) ->
    unicode:characters_to_binary(io_lib:format(
                                   "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ",
                                   [Year, Month, Day, Hour, Minute, Second]));

expand_records({K, V}) when is_number(K) ->
    [ K, V ];
expand_records({K, V}) ->
    {expand_records(K), expand_records(V)};
expand_records(L) when is_list(L) ->
    lists:map(
        fun
            ({K, V}) -> {K, expand_records(V)};
            (V) -> expand_records(V)
        end,
        L);
expand_records(M) when is_map(M) ->
    maps:map( fun(_K, V) -> expand_records(V) end, M );
expand_records(undefined) ->
    null;
expand_records({A, B, Params} = Mime) when is_binary(A), is_binary(B), is_list(Params) ->
    % Assume to be a MIME content type
    format_content_type(Mime);
expand_records(X) ->
    X.

expand_record_1([ {F, _} | Fs ], N, R, Acc) ->
    Acc1 = Acc#{ F => expand_records( element(N, R) ) },
    expand_record_1(Fs, N+1, R, Acc1);
expand_record_1([], _N, _R, Acc) ->
    Acc.


mochijson_to_map({struct, L}) ->
    maps:from_list([ mochijson_to_map(V) || V <- L ]);
mochijson_to_map({K, V}) ->
    {K, mochijson_to_map(V)};
mochijson_to_map(V) ->
    V.

-spec format_content_type(MediaType) -> Result when
    MediaType :: cow_http_hd:media_type(),
    Result :: binary().
format_content_type({T1, T2, []}) ->
    <<T1/binary, $/, T2/binary>>;
format_content_type({T1, T2, Params}) ->
    ParamsBin = [ [$;, Param, $=, Value] || {Param,Value} <- Params ],
    iolist_to_binary([T1, $/, T2, ParamsBin]).

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

chars_to_integer(N2, N1) ->
    ((N2 - $0) * 10) + (N1 - $0).

chars_to_integer(N3, N2, N1) ->
    ((N3 - $0) * 100) + ((N2 - $0) * 10) + (N1 - $0).

chars_to_integer(N4, N3, N2, N1) ->
    ((N4 - $0) * 1000) + ((N3 - $0) * 100) + ((N2 - $0) * 10) + (N1 - $0).


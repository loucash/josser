-module(josser_prop_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("proper/include/proper.hrl").

-define(NUMTESTS, 1000).
-define(PROPTEST(A), true = proper:quickcheck(A(),
                                              [{numtests, ?NUMTESTS},
                                               {constraint_tries, 1000}])).

all() -> [t_prop_jsonschema].

t_prop_jsonschema(_Config) ->
    ?PROPTEST(prop_jsonschema).

prop_jsonschema() ->
    ?FORALL(Json, 
            json(),
            begin
                {ok, JsonSchema} = josser:make_schema(Json),
                {ok, _Res} = jesse:validate_with_schema(JsonSchema, Json),
                true
            end).

utf8_bin() ->
    ?LET(S,
         list(integer(16#21, 16#7E)),
         list_to_binary(S)).

json() ->
    non_empty(oneof([json_object(), json_array()])).

json_values() ->
    oneof([
        integer(), 
        utf8_bin(), 
        boolean(),
        float(),
        ?LAZY(json_object()),
        ?LAZY(json_array())
        ]).

json_object() ->
    [{non_empty(utf8_bin()), json_values()}].

json_array() ->
    [json_values()].

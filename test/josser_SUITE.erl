-module(josser_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-define(DEFAULT_SCHEMA, {<<"$schema">>, <<"http://json-schema.org/draft-03/schema#">>}).

all() ->
    [
     t_simple_json_types,
     t_simple_json_types_no_validation,
     t_nested_objects,
     t_nested_arrays,
     t_options
    ].

t_simple_json_types(_Config) ->
    TestData = [
        {[{<<"key">>, <<"value">>}],
         [?DEFAULT_SCHEMA,
          {<<"type">>, <<"object">>},
          {<<"properties">>,[{<<"key">>,[{<<"type">>,<<"string">>}]}]}]},

        {[{<<"key">>, 1}],
         [?DEFAULT_SCHEMA,
          {<<"type">>, <<"object">>},
          {<<"properties">>,[{<<"key">>,[{<<"type">>,<<"integer">>}]}]}]},

        {[{<<"key">>, true}],
         [?DEFAULT_SCHEMA,
          {<<"type">>, <<"object">>},
          {<<"properties">>,[{<<"key">>,[{<<"type">>,<<"boolean">>}]}]}]},

        {[{<<"key">>, 3.14}],
         [?DEFAULT_SCHEMA,
          {<<"type">>, <<"object">>},
          {<<"properties">>,[{<<"key">>,[{<<"type">>,<<"number">>}]}]}]},

        {[{<<"key">>, null}],
         [?DEFAULT_SCHEMA,
          {<<"type">>, <<"object">>},
          {<<"properties">>,[{<<"key">>,[{<<"type">>,<<"null">>}]}]}]}
    ],
    lists:foreach(
      fun({Json, JsonSchema}) ->
              {ok, JS} = josser:make_schema(Json),
              {ok, _Res} = jesse:validate_with_schema(JS, Json),
              true = jsx:is_term(JS),
              true = JsonSchema =:= JS
      end,
      TestData),
    ok.

t_nested_objects(_Config) ->
    TestData = [
        {[{<<"key">>, [{<<"subkey">>, <<"value">>}]}],
         [?DEFAULT_SCHEMA,
          {<<"type">>, <<"object">>},
          {<<"properties">>,
            [{<<"key">>, [{<<"type">>, <<"object">>},
                          {<<"properties">>,[{<<"subkey">>,[{<<"type">>,<<"string">>}]}]}]}]}]}
    ],
    lists:foreach(
      fun({Json, JsonSchema}) ->
              {ok, JS} = josser:make_schema(Json),
              {ok, _Res} = jesse:validate_with_schema(JS, Json),
              true = jsx:is_term(JS),
              true = JsonSchema =:= JS
      end,
      TestData),
    ok.

t_simple_json_types_no_validation(_Config) ->
    TestData = [
        {123, [?DEFAULT_SCHEMA, {<<"type">>,<<"integer">>}]}
    ],
    lists:foreach(
      fun({Json, JsonSchema}) ->
              {ok, JS} = josser:make_schema(Json),
              true = jsx:is_term(JS),
              true = JsonSchema =:= JS
      end,
      TestData),
    ok.

t_nested_arrays(_Config) ->
    TestData = [
        {[{<<"key">>, [[{<<"subkey">>, <<"value">>}]]}],
         [?DEFAULT_SCHEMA,
          {<<"type">>, <<"object">>},
          {<<"properties">>,
            [{<<"key">>,
                [{<<"type">>, <<"array">>},
                 {<<"items">>,
                [[{<<"type">>, <<"object">>},
                  {<<"properties">>,
                    [{<<"subkey">>,[{<<"type">>,<<"string">>}]}]}]]}]}]}]},

        {[{<<"key">>, [1,2,3]}],
         [?DEFAULT_SCHEMA,
          {<<"type">>, <<"object">>},
          {<<"properties">>,
            [{<<"key">>,
              [{<<"type">>, <<"array">>},
               {<<"items">>,
                [[{<<"type">>,<<"integer">>}],
                 [{<<"type">>,<<"integer">>}],
                 [{<<"type">>,<<"integer">>}]]}]}]}]},

        {[1,2,3],
         [?DEFAULT_SCHEMA,
          {<<"type">>, <<"array">>},
          {<<"items">>,
            [[{<<"type">>,<<"integer">>}],
             [{<<"type">>,<<"integer">>}],
             [{<<"type">>,<<"integer">>}]]}]},

        {[3.14,true,null,<<"str">>],
         [?DEFAULT_SCHEMA,
          {<<"type">>, <<"array">>},
          {<<"items">>,
            [[{<<"type">>,<<"number">>}],
             [{<<"type">>,<<"boolean">>}],
             [{<<"type">>,<<"null">>}],
             [{<<"type">>,<<"string">>}]]}]}

    ],
    lists:foreach(
      fun({Json, JsonSchema}) ->
              {ok, JS} = josser:make_schema(Json),
              {ok, _Res} = jesse:validate_with_schema(JS, Json),
              true = jsx:is_term(JS),
              true = JsonSchema =:= JS
      end,
      TestData),
    ok.

t_options(_Config) ->
    Json = [{<<"key">>, <<"value">>}],
    {ok, JsonSchema} = josser:make_schema(Json,[{additional_properties, false},
                                                {encode_json, true}]),
    <<"{\"$schema\":\"http://json-schema.org/draft-03/schema#\","
       "\"additionalProperties\":false,\"type\":\"object\","
       "\"properties\":{\"key\":{\"type\":\"string\"}}}">> = JsonSchema,
    true = jsx:is_term(JsonSchema),

    {ok, JsonSchema1} = josser:make_schema(Json,[{additional_properties, true},
                                                 {encode_json, true}]),
    <<"{\"$schema\":\"http://json-schema.org/draft-03/schema#\","
       "\"additionalProperties\":true,\"type\":\"object\","
       "\"properties\":{\"key\":{\"type\":\"string\"}}}">> = JsonSchema1,
    true = jsx:is_term(JsonSchema1),
    ok.

-module(josser_metadata_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-define(DEFAULT_SCHEMA, {<<"$schema">>, <<"http://json-schema.org/schema#">>}).

all() ->
    [
     t_simple_json_types,
     t_custom_type_boolean,
     t_custom_type_integer,
     t_custom_type_string,
     t_custom_type_number,
     t_undefined_custom_type,
     t_multiple_types
    ].

t_simple_json_types(_Config) ->
    TestData = [
        {
            [{<<"key">>, <<"{\"type\": \"integer\"}">>}],

            [?DEFAULT_SCHEMA, 
             {<<"type">>, <<"object">>},
             {<<"properties">>,
              [{<<"key">>,[{<<"type">>,<<"integer">>}]}]}]
        },
        {
            [{<<"key">>, <<"{\"type\": \"any\"}">>}],

            [?DEFAULT_SCHEMA, 
             {<<"type">>, <<"object">>},
             {<<"properties">>,
              [{<<"key">>,[{<<"type">>,<<"any">>}]}]}]
        },
        {
            [{<<"key">>, <<"{\"type\": \"string\"}">>}],

            [?DEFAULT_SCHEMA, 
             {<<"type">>, <<"object">>},
             {<<"properties">>,
              [{<<"key">>,[{<<"type">>,<<"string">>}]}]}]
        },
        {
            [{<<"key">>, <<"{\"type\": \"number\"}">>}],

            [?DEFAULT_SCHEMA, 
             {<<"type">>, <<"object">>},
             {<<"properties">>,
              [{<<"key">>,[{<<"type">>,<<"number">>}]}]}]
        },
        {
            [{<<"key">>, <<"{\"type\": \"boolean\"}">>}],

            [?DEFAULT_SCHEMA, 
             {<<"type">>, <<"object">>},
             {<<"properties">>,
              [{<<"key">>,[{<<"type">>,<<"boolean">>}]}]}]
        },
        {
            [{<<"key">>, <<"{\"type\": \"array\"}">>}],

            [?DEFAULT_SCHEMA, 
             {<<"type">>, <<"object">>},
             {<<"properties">>,
              [{<<"key">>,[{<<"type">>,<<"array">>}]}]}]
        },
        {
            [{<<"key">>, <<"{\"type\": \"object\"}">>}],

            [?DEFAULT_SCHEMA, 
             {<<"type">>, <<"object">>},
             {<<"properties">>,
              [{<<"key">>,[{<<"type">>,<<"object">>}]}]}]
        },
        {
            [{<<"key">>, <<"{\"type\": null}">>}],

            [?DEFAULT_SCHEMA, 
             {<<"type">>, <<"object">>},
             {<<"properties">>,
              [{<<"key">>,[{<<"type">>,null}]}]}]
        }
    ],
    lists:foreach(
      fun({Json, JsonSchema}) ->
        {ok, JS} = josser:make_schema(Json, [{value_as_metadata, true}]),
        true = jsx:is_term(JS),
        true = JsonSchema =:= JS
      end,
      TestData),
    ok.

t_custom_type_boolean(Config) ->
    CustomTypesFile = filename:join(
                        ?config(data_dir, Config), 
                        "custom_type_boolean.json"),
    Json = [{<<"key">>, <<"{\"type\": \"custom_type1\"}">>}], 
    JsonSchema = [?DEFAULT_SCHEMA, 
                  {<<"type">>, <<"object">>},
                  {<<"properties">>,
                   [{<<"key">>,[{<<"type">>,<<"boolean">>}, 
                                {<<"enum">>, [true]},
                                {<<"required">>, true},
                                {<<"description">>, <<"desc">>}]}]}],
    {ok, JS} = josser:make_custom_schema(
                Json, 
                {file, CustomTypesFile}),
    true = jsx:is_term(JS),
    JsonSchema = JS,
    ok.

t_custom_type_integer(_Config) ->
    Json = [{<<"key">>, <<"{\"type\": \"custom_type1\"}">>}], 
    JsonSchema = [?DEFAULT_SCHEMA, 
                  {<<"type">>, <<"object">>},
                  {<<"properties">>,
                   [{<<"key">>,[{<<"type">>,<<"integer">>}, 
                                {<<"minimum">>, 0},
                                {<<"maximum">>, 10},
                                {<<"exclusiveMinimum">>, true},
                                {<<"exclusiveMaximum">>, false}]}]}],
    {ok, JS} = josser:make_custom_schema(
                Json, 
                {json_term, [{<<"custom_type1">>, 
                  [{<<"type">>, <<"integer">>},
                   {<<"minimum">>, 0},
                   {<<"maximum">>, 10},
                   {<<"exclusiveMinimum">>, true},
                   {<<"exclusiveMaximum">>, false}]}]}),
    true = jsx:is_term(JS),
    JsonSchema = JS,
    ok.

t_custom_type_number(Config) ->
    CustomTypesFile = filename:join(
                        ?config(data_dir, Config), 
                        "custom_type_number.json"),
    Json = [{<<"key">>, <<"{\"type\": \"custom_type1\"}">>}], 
    JsonSchema = <<"{\"$schema\":\"http://json-schema.org/schema#\","
                    "\"type\":\"object\",\"properties\":{\"key\":{\"type\":\"number\"}}}">>,
    {ok, JS} = josser:make_custom_schema(
                Json, {file, CustomTypesFile}, [{encode_json, true}]),
    true = jsx:is_json(JS),
    JsonSchema = JS,
    ok.

t_custom_type_string(_Config) ->
    Json = [{<<"key">>, <<"{\"type\": \"custom_type1\"}">>}], 
    JsonSchema = [?DEFAULT_SCHEMA, 
                  {<<"type">>, <<"object">>},
                  {<<"properties">>,
                   [{<<"key">>,[{<<"type">>,<<"string">>}, 
                                {<<"minLength">>, 0},
                                {<<"maxLength">>, 10},
                                {<<"pattern">>, <<"pattern">>}]}]}],
    {ok, JS} = josser:make_custom_schema(
                Json, 
                {json_term, [{<<"custom_type1">>, 
                  [{<<"type">>, <<"string">>},
                   {<<"minLength">>, 0},
                   {<<"maxLength">>, 10},
                   {<<"pattern">>, <<"pattern">>}]}]}),
    true = jsx:is_term(JS),
    JsonSchema = JS,
    ok.

t_undefined_custom_type(_Config) ->
    Json = [{<<"key">>, <<"{\"type\": \"custom_type1\"}">>}], 
    {error, {missing_type, _CType}} = josser:make_custom_schema(
                Json, 
                {json_term, [{<<"custom_type2">>, [{<<"type">>, <<"boolean">>}]}]}),
    ok.

t_multiple_types(Config) ->
    TestData = [
        {
            [{<<"key">>, <<"{\"type\": [\"integer\", \"string\",\"object\",\"integer\"]}">>}],

            [?DEFAULT_SCHEMA, 
                  {<<"type">>,<<"object">>},
                  {<<"properties">>,
                   [{<<"key">>,[{<<"type">>,[<<"string">>,
                                             <<"object">>,
                                             <<"integer">>]}]}]}]
        },
        {
            [{<<"key">>, <<"{\"type\": [\"string\", \"string\"]}">>}],

            [?DEFAULT_SCHEMA, 
                  {<<"type">>,<<"object">>},
                  {<<"properties">>,
                   [{<<"key">>,[{<<"type">>,<<"string">>}]}]}]
        },
        {
            [{<<"key">>, <<"{\"type\": [\"integer\", \"ct1\"], \"enum\": [1,2,3], \"description\": \"d\"}">>}],

            [?DEFAULT_SCHEMA, 
                  {<<"type">>,<<"object">>},
                  {<<"properties">>,
                   [{<<"key">>,[{<<"description">>, <<"d">>},
                                {<<"enum">>, [4,1,2,3]},
                                {<<"type">>,<<"integer">>}
                                ]}]}]
        }
    ],
    lists:foreach(
      fun({Json, JsonSchema}) ->
        {ok, JS} = josser:make_custom_schema(
                     Json, 
                     {json_term, 
                       [{<<"ct1">>, [{<<"type">>, <<"integer">>}, 
                                     {<<"enum">>, [4]},
                                     {<<"description">>, <<"e">>}]}]}),
        true = jsx:is_term(JS),
        JsonSchema = JS
      end,
      TestData),
    ok.

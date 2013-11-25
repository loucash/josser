%%%-------------------------------------------------------------------
%%% @author Łukasz Biedrycki <lukasz.biedrycki@gmail.com>
%%% @doc
%%% JoSSER - Json Schema Generator
%%% @end
%%%-------------------------------------------------------------------
-module(josser).
-author('Łukasz Biedrycki <lukasz.biedrycki@gmail.com>').

-export([make_schema/1, 
         make_schema/2,
         make_custom_schema/2,
         make_custom_schema/3]).

-include("josser.hrl").

-type options()                 :: {additional_properties, boolean()}
                                 | {encode_json, boolean()}
                                 | {value_as_metadata, boolean()}.
-type option_list()             :: [options()].
-type josser_errors()           :: {missing_type, binary()}
                                 | {invalid_custom_type_attr, binary()}.
-type josser_result()           :: {ok, jsx:json_text()}
                                 | {ok, jsx:json_term()}
                                 | {error, josser_errors()}.
-type custom_types()            :: [{binary(), custom_type_values()}].

-export_type([custom_types/0]).

-record(options, {
          custom_types = []         :: custom_types(), 
          additional_properties     :: boolean(),
          encode_json = false       :: boolean(),
          value_as_metadata = false :: boolean()
         }).

-define(DEFAULT_SCHEMA, {<<"$schema">>, <<"http://json-schema.org/schema#">>}).

%%%===================================================================
%%% API
%%%===================================================================

-spec make_schema(jsx:json_term()) -> josser_result().
make_schema(JSON) ->
    generate(JSON, [], []).

-spec make_schema(jsx:json_term(), option_list()) -> josser_result().
make_schema(JSON, Options) ->
    generate(JSON, [], Options).

-spec make_custom_schema(jsx:json_term(), custom_types()) -> josser_result().
make_custom_schema(JSON, CustomTypes) ->
    generate(JSON, CustomTypes, [{value_as_metadata, true}]).

-spec make_custom_schema(jsx:json_term(), custom_types(), 
                         option_list()) -> josser_result().
make_custom_schema(JSON, CustomTypes, Options) ->
    generate(JSON, CustomTypes, [{value_as_metadata, true} | Options]).


%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec generate(jsx:json_term(), custom_types(), option_list()) -> 
    josser_result().
generate(JSON, CustomTypes, Options) ->
    Opt = lists:foldl(fun set_prop/2, 
                      #options{custom_types=CustomTypes}, 
                      Options),
    try traverse(JSON, Opt) of
        JsonSchema -> {ok, JsonSchema}
    catch
        Error -> Error
    end.

-spec traverse(jsx:json_term(), #options{}) -> {ok, jsx:json_text()}
                                             | {ok, jsx:json_term()}
                                             | no_return().
traverse(JSON, Opt) ->
    Result = traverse_json(JSON, Opt),
    ResultWithSchema = [?DEFAULT_SCHEMA | Result],
    case encode_json(Opt) of
        true -> jsx:encode(ResultWithSchema);
        false -> ResultWithSchema
    end.

-spec traverse_json(jsx:json_term(), #options{}) -> jsx:json_term() 
                                                  | no_return().
traverse_json([H|_T]=JSON, Opt) when is_tuple(H) ->
    Properties = generate_properties(JSON, Opt),
    JSONObject = [{<<"type">>, <<"object">>}, 
                  {<<"properties">>, Properties}],
    apply_additional_parameters(Opt, JSONObject);
traverse_json(JSON, Opt) when is_list(JSON) ->
    ArrayItems = generate_items(JSON, Opt),
    [{<<"type">>, <<"array">>}, 
     {<<"items">>, ArrayItems}];
traverse_json(JSON, Opt) ->
    [Result] = generate_items([JSON], Opt),
    Result.

-spec apply_additional_parameters(#options{}, jsx:json_term()) -> jsx:json_term().
apply_additional_parameters(Opt, JsonObject) ->
    case additional_properties(Opt) of
        true -> [{<<"additionalProperties">>, true} | JsonObject];
        false -> [{<<"additionalProperties">>, false} | JsonObject];
        undefined -> JsonObject
    end.

% @doc
% generates items for array type
-spec generate_items(jsx:json_term(), #options{}) -> jsx:json_term() 
                                                   | no_return().
generate_items(Json, Opt) ->
    JsonTypes = lists:foldl(fun(J, Acc) -> [value_type(J, Opt) | Acc] end, 
                            [], Json),
    lists:reverse(JsonTypes).

% @doc
% generates properties for object type
-spec generate_properties(jsx:json_term(), #options{}) -> jsx:json_term()
                                                        | no_return().
generate_properties(Json, Opt) ->
    PropTypes = lists:foldl(
                  fun({K, V}, Acc) -> [{K, value_type(V, Opt)} | Acc] end,
                  [], Json),
    lists:reverse(PropTypes).

% @doc
% detects single item value type
-spec value_type(jsx:json_term(), #options{}) -> jsx:json_term() | no_return().
value_type(Value, Opt) when is_list(Value) ->
    traverse_json(Value, Opt);
value_type(Value, #options{value_as_metadata=false}) when is_integer(Value) ->
    [{<<"type">>, <<"integer">>}];
value_type(Value, #options{value_as_metadata=false}) when is_float(Value) ->
    [{<<"type">>, <<"number">>}];
value_type(Value, #options{value_as_metadata=false}) when is_boolean(Value) ->
    [{<<"type">>, <<"boolean">>}];
value_type(Value, #options{value_as_metadata=false}) when Value =:= null ->
    [{<<"type">>, <<"null">>}];
value_type(Value, #options{value_as_metadata=false}) when is_binary(Value) ->
    [{<<"type">>, <<"string">>}];
value_type(Value,  #options{value_as_metadata=true, 
                          custom_types=CustomTypes}) when is_binary(Value) ->
    TypeDesc = jsx:decode(Value),
    {ok, TypeDescValidated} = josser_custom_types:validate(TypeDesc, CustomTypes),
    TypeDescValidated.

% @doc
% setter for options record
-spec set_prop(options(), #options{}) -> #options{}.
set_prop({additional_properties, AddProp}, Opt) ->
    Opt#options{additional_properties = AddProp};
set_prop({encode_json, EncJson}, Opt) ->
    Opt#options{encode_json=EncJson};
set_prop({value_as_metadata, Val}, Opt) ->
    Opt#options{value_as_metadata=Val}.

% @doc
% getters for options record
-spec additional_properties(#options{}) -> boolean() | undefined.
additional_properties(#options{additional_properties=AddProp}) ->
    AddProp.
-spec encode_json(#options{}) -> boolean() | undefined.
encode_json(#options{encode_json=EncJson}) ->
    EncJson.

%%%-------------------------------------------------------------------
%%% @author Łukasz Biedrycki <lukasz.biedrycki@gmail.com>
%%% @doc
%%% JoSSER - Json Schema Generator
%%% @end
%%%-------------------------------------------------------------------
-module(josser_custom_types).
-author('Łukasz Biedrycki <lukasz.biedrycki@gmail.com>').

-export([validate/2]).

-include("josser.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-spec validate(jsx:json_term(), josser:custom_types()) -> 
    {ok, jsx:json_term()} | no_return().
validate(TypeDesc, CustomTypes) ->
    Type = proplists:get_value(<<"type">>, TypeDesc),
    case validate_simple_type(Type) of
        true -> {ok, TypeDesc};
        false -> validate_custom_type(Type, CustomTypes)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec validate_simple_type(binary() | null) -> boolean().
validate_simple_type(<<"object">>) -> true;
validate_simple_type(<<"array">>) -> true;
validate_simple_type(<<"any">>) -> true;
validate_simple_type(<<"string">>) -> true;
validate_simple_type(<<"integer">>) -> true;
validate_simple_type(<<"number">>) -> true;
validate_simple_type(<<"boolean">>) -> true;
validate_simple_type(null) -> true;
validate_simple_type(_Other) -> false.

-spec validate_custom_type(binary(), josser:custom_types()) ->
    {ok, jsx:json_term()} | no_return().
validate_custom_type(Type, CustomTypes) ->
    case proplists:get_value(Type, CustomTypes) of
        undefined -> erlang:throw({error, {missing_type, Type}});
        CustomType -> {ok, render_custom_type(CustomType, [])}
    end.

-spec render_custom_type(josser:custom_types(), list()) -> 
    [{binary(), binary() 
              | boolean() 
              | number() 
              | list(binary() | boolean() | number())}] 
  | no_return().
render_custom_type([], Acc) ->
    lists:reverse(Acc);
render_custom_type([{type, boolean} | Tail], Acc) ->
    render_custom_type(Tail, [{<<"type">>, <<"boolean">>} | Acc]);
render_custom_type([{type, string} | Tail], Acc) ->
    render_custom_type(Tail, [{<<"type">>, <<"string">>} | Acc]);
render_custom_type([{type, number} | Tail], Acc) ->
    render_custom_type(Tail, [{<<"type">>, <<"number">>} | Acc]);
render_custom_type([{type, integer} | Tail], Acc) ->
    render_custom_type(Tail, [{<<"type">>, <<"integer">>} | Acc]);
render_custom_type([{description, Value} | Tail], Acc) ->
    render_custom_type(Tail, [{<<"description">>, Value} | Acc]);
render_custom_type([{required, Value} | Tail], Acc) ->
    render_custom_type(Tail, [{<<"required">>, Value} | Acc]);
render_custom_type([{enum, Value} | Tail], Acc) ->
    render_custom_type(Tail, [{<<"enum">>, Value} | Acc]);
render_custom_type([{minimum, Value} | Tail], Acc) ->
    render_custom_type(Tail, [{<<"minimum">>, Value} | Acc]);
render_custom_type([{maximum, Value} | Tail], Acc) ->
    render_custom_type(Tail, [{<<"maximum">>, Value} | Acc]);
render_custom_type([{exclusive_maximum, Value} | Tail], Acc) ->
    render_custom_type(Tail, [{<<"exclusiveMaximum">>, Value} | Acc]);
render_custom_type([{exclusive_minimum, Value} | Tail], Acc) ->
    render_custom_type(Tail, [{<<"exclusiveMinimum">>, Value} | Acc]);
render_custom_type([{min_length, Value} | Tail], Acc) ->
    render_custom_type(Tail, [{<<"minLength">>, Value} | Acc]);
render_custom_type([{max_length, Value} | Tail], Acc) ->
    render_custom_type(Tail, [{<<"maxLength">>, Value} | Acc]);
render_custom_type([{pattern, Value} | Tail], Acc) ->
    render_custom_type(Tail, [{<<"pattern">>, Value} | Acc]);
render_custom_type([Other | _Tail], _Acc) ->
    erlang:throw({error, {invalid_custom_type_attr, Other}}).

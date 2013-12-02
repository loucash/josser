%%%-------------------------------------------------------------------
%%% @author Łukasz Biedrycki <lukasz.biedrycki@gmail.com>
%%% @doc
%%% JoSSER - Json Schema Generator
%%% @end
%%%-------------------------------------------------------------------
-module(josser_custom_types).
-author('Łukasz Biedrycki <lukasz.biedrycki@gmail.com>').

-export([validate/2]).

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
        CustomType -> {ok, CustomType}
    end.

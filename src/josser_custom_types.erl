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
    TypeAttrs = proplists:delete(<<"type">>, TypeDesc),
    Validated = validate_type(Type, CustomTypes),
    TypeDefinition = merge_definitions(lists:flatten(Validated), TypeAttrs),
    {ok, TypeDefinition}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
merge_definitions([], TypeDef) ->
    lists:reverse(TypeDef);
merge_definitions([{<<"type">>, Type} | Tail], TypeDef) ->
    T = proplists:get_value(<<"type">>, TypeDef),
    case T of
        Type -> 
            merge_definitions(Tail, TypeDef);
        Type2 when is_binary(Type2) ->
            TypeDef2 = lists:keyreplace(<<"type">>, 1, TypeDef, {<<"type">>, [Type, Type2]}),
            merge_definitions(Tail, TypeDef2);
        TypeList when is_list(TypeList) ->
            case lists:member(Type, TypeList) of
                true ->
                    merge_definitions(Tail, TypeDef);
                false ->
                    TypeDef2 = lists:keyreplace(<<"type">>, 1, TypeDef, {<<"type">>, [Type | TypeList]}),
                    merge_definitions(Tail, TypeDef2)
            end;
        undefined ->
            merge_definitions(Tail, [{<<"type">>, Type} | TypeDef])
    end;
merge_definitions([{<<"enum">>, Enum} | Tail], TypeDef) ->
    Enum2 = proplists:get_value(<<"enum">>, TypeDef),
    case Enum2 of
        undefined ->
            merge_definitions(Tail, [{<<"enum">>, Enum} | TypeDef]);
        _ -> 
            TypeDef2 = lists:keyreplace(<<"enum">>, 1, TypeDef, {<<"enum">>, lists:append(Enum, Enum2)}),
            merge_definitions(Tail, TypeDef2)
    end;
merge_definitions([{Key, Value} | Tail], TypeDef) ->
    Val2 = proplists:get_value(Key, TypeDef),
    case Val2 of
        undefined -> 
            merge_definitions(Tail, [{Key, Value} | TypeDef]);
        _ ->
            merge_definitions(Tail, TypeDef)
    end.

-spec validate_type(list(binary()) | binary(), josser:custom_types()) -> list().
validate_type(Type, CustomTypes) ->
    validate_type(Type, CustomTypes, []).

validate_type([], _CustomTypes, Acc) ->
    Acc;
validate_type(Type, CustomTypes, Acc) when not is_list(Type) ->
    validate_type([Type], CustomTypes, Acc);
validate_type([Type|Tail], CustomTypes, Acc) ->
    TypeDef = case validate_simple_type(Type) of
        true -> [{<<"type">>, Type}];
        false -> validate_custom_type(Type, CustomTypes)
    end,
    validate_type(Tail, CustomTypes, [TypeDef | Acc]).
    
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
        CustomType -> CustomType
    end.

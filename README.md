JoSSER - JSon Schema Generator ERlang
===========================

JoSSER is a library in Erlang to generate Json Schema out of Json. 

* It implements part of [Draft 03] (http://tools.ietf.org/html/draft-zyp-json-schema-03).
* It works with [jesse] (https://github.com/klarna/jesse).
* It supports only [jsx] (https://github.com/talentdeficit/jsx) (for now).
* It supports "value as metadata" which means that values in json can hold metadata
  about themselves.

Installation
------------

`make deps compile test`

Examples
--------

* create simple json schema

```
1> josser:make_schema([{<<"key">>, <<"value">>}]).
{ok,[{<<"$schema">>,<<"http://json-schema.org/schema#">>},
     {<<"properties">>,
      [{<<"key">>,[{<<"type">>,<<"string">>}]}]}]}
2> josser:make_schema([{<<"key">>, 123}]).
{ok,[{<<"$schema">>,<<"http://json-schema.org/schema#">>},
     {<<"properties">>,
      [{<<"key">>,[{<<"type">>,<<"integer">>}]}]}]}
```

* create json schema with values as metadata

```
1> josser:make_schema([{<<"key">>, <<"{\"type\": \"integer\", \"min\":0}">>}], [{value_as_metadata, true}]).
{ok,[{<<"$schema">>,<<"http://json-schema.org/schema#">>},
     {<<"properties">>,
      [{<<"key">>,[{<<"type">>,<<"integer">>},{<<"min">>,0}]}]}]}
```

* create json schema with custom types (`object` and `array` not yet supported)

```
1> josser:make_custom_schema([{<<"key">>, <<"{\"type\": \"custom_type\"}">>}], 
                             [{<<"custom_type">>, [{type, integer}, {maximum, 10}]}]).
{ok,[{<<"$schema">>,<<"http://json-schema.org/schema#">>},
     {<<"properties">>,
      [{<<"key">>,
        [{<<"type">>,<<"integer">>},{<<"maximum">>,10}]}]}]}
```

Contributing
------------
If you see something missing or incorrect, do not hesitate to create an issue
or pull request. Thank you!
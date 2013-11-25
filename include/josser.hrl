%%%-------------------------------------------------------------------
%%% @author Łukasz Biedrycki <lukasz.biedrycki@gmail.com>
%%% @doc
%%% JoSSER - Json Schema Generator
%%% @end
%%%-------------------------------------------------------------------

-type common_attrs(T, A)    ::  {type, T}
                              | {description, binary()} 
                              | {required, boolean()}
                              | {enum, list(A)}.
-type numeric_attrs(A)      ::  {minimum, A}
                              | {maximum, A}
                              | {exclusive_maximum, A}
                              | {exclusive_minimum, A}.
-type custom_integer()      :: [common_attrs(integer, integer()) 
                              | numeric_attrs(integer())].
-type custom_number()       :: [common_attrs(number, number()) 
                              | numeric_attrs(number())].
-type custom_string()       :: [common_attrs(string, binary())
                              | {min_length, integer()}
                              | {max_length, integer()}
                              | {pattern, binary()}].
-type custom_boolean()      :: [common_attrs(boolean, boolean())].

-type custom_type_values()  :: custom_integer() 
                             | custom_number()
                             | custom_string()
                             | custom_boolean().

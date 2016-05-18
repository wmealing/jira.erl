
-module( jira_tests ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).

-include_lib( "eunit/include/eunit.hrl" ).

init_test() ->
	State = jira:init( "user", "password", "jira.example.com" ),
	?assert( jira:url( State ) =:= <<"https://jira.example.com:443/rest/api/2">> ).
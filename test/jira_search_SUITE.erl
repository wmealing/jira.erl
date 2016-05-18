-module( jira_search_SUITE ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).

-include_lib( "common_test/include/ct.hrl" ).

-compile( export_all ).

all() ->
	[ search_test ].

init_per_suite( Config ) ->
	hackney:start(),
	State = jira:init( 	ct:get_config( jira_username ),
						ct:get_config( jira_password ),
						ct:get_config( jira_host ),
						ct:get_config( jira_port ) ),
	[ { jira_state, State } | Config ].
	
search_test( Config ) ->
	State = ?config( jira_state, Config ),
	{ ok, _Issues, _TotalResults } = jira:search( "project = WEB", 0, 25, ["summary"], State ).
	
end_per_suite( Config ) ->
	hackney:stop(),
	Config.
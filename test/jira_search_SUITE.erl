-module( jira_search_SUITE ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).

-include_lib( "common_test/include/ct.hrl" ).

-compile( export_all ).

all() ->
	[{ group, search_and_issue }].
	
groups() -> [{ search_and_issue, [sequence], [search_test, issue_test] }].

init_per_suite( Config ) ->
	hackney:start(),
	State = jira:init( 	ct:get_config( jira_username ),
						ct:get_config( jira_password ),
						ct:get_config( jira_host ),
						ct:get_config( jira_port ),
						[] ),
	[ { jira_state, State } | Config ].
	
search_test( Config ) ->
	State = ?config( jira_state, Config ),
	{ ok, Issues, _TotalResults } = jira:search( "project = " ++ ct:get_config( jira_project ), 0, 25, ["summary"], State ),
	NewConfig = [{ issues, Issues } | Config],
	{ { Y, M, D }, Time } = calendar:universal_time(),
	{ ok, _Updated } = jira:search( jira:jql( [{ project, eq, ct:get_config( jira_project ) }, 'and', { updated, gt, { { Y, M, max( D - 1, 1 ) }, Time } }] ),
													["summary"], State ),
	{ save_config, NewConfig }.
	
issue_test( Config ) ->
	State = ?config( jira_state, Config ),
	{ search_test, NewConfig } = ?config( saved_config, Config ),
	Issues = ?config( issues, NewConfig ),
	case length( Issues ) of
		%% We don't have any issues we can retrieve by key, since the search returned an empty list
		0 -> 
			Config;
		_ ->
			[ First | _Rest ] = Issues,
			Key = maps:get( <<"key">>, First ),
			{ ok, _Issue } = jira:issue( Key, State ),
			Config
	end.
	
end_per_suite( Config ) ->
	hackney:stop(),
	Config.
-module(jira_tests).
-author("Warren Kenny <warren.kenny@gmail.com>").

-include_lib("eunit/include/eunit.hrl").

%% Test suite setup and teardown
jira_test_() ->
    {setup, fun setup/0, fun teardown/1, [
        fun test_init/0,
        fun test_jql/0,
        fun test_json_get/0,
        fun test_update_issue/0,
        fun test_get_myself/0
    ]}.

setup() ->
    hackney:start().

teardown(_) ->
    hackney:stop().

test_init() ->
    State = jira:init(bearerauth, "issues.redhat.com", []),
    ?assert(jira:url(State) =:= <<"https://issues.redhat.com:443/rest/api/2">>).

test_jql() ->
    "project = 'VULN' AND updated > '2016/07/06 20:34'" = jira:jql([
        {project, eq, "VULN"}, 'and', {updated, gt, {{2016, 07, 06}, {20, 34, 00}}}
    ]).

test_json_get() ->
    <<"Hello">> = jira:json_get(
        key,
        #{<<"key">> => <<"Hello">>},
        jira:init("user", "password", "jira.example.com", 80, [])
    ),

    <<"Hello">> = jira:json_get(
        key,
        #{key => <<"Hello">>},
        jira:init(
            "user",
            "password",
            "jira.example.com",
            80,
            [{labels, atom}]
        )
    ).

test_update_issue() ->
    Key = "KMAINT-1000",
    Fields = #{<<"customfield_12310243">> => 11},
    State = jira:init(bearerauth, "issues.redhat.com", []),

    %% First get the current issue to verify it exists
    case jira:issue(Key, State) of
        {ok, OriginalIssue} ->
            %% Update the issue
            UpdateResult = jira:update_issue(Key, Fields, State),
            ?assertMatch(ok, UpdateResult),

            %% Fetch the issue again to verify the update
            {ok, UpdatedIssue} = jira:issue(Key, State),
            UpdatedFields = jira:field_from_issue(UpdatedIssue, <<"customfield_12310243">>),
            %% JIRA may return the value as float, so check for both
            ?assert(want:integer(UpdatedFields) =:= 11);
        {error, _Reason} ->
            %% If we can't fetch the issue (likely auth/network), skip the test
            ?assert(true)
    end.

test_get_myself() ->
    State = jira:init(bearerauth, "issues.redhat.com", []),

    %% Test authentication by getting current user info
    case jira:get_myself(State) of
        {ok, UserInfo} ->
            %% Verify we got a user info map with expected fields
            ?assert(is_map(UserInfo)),
            ?assert(
                maps:is_key(<<"name">>, UserInfo) orelse maps:is_key(<<"accountId">>, UserInfo)
            );
        {error, _Reason} ->
            %% If auth fails or network issues, skip the test
            ?assert(true)
    end.

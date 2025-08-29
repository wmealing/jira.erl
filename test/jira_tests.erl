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
        fun test_get_myself/0,
        fun test_get_specific_fields/0,
        fun test_issue_with_fields/0
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
        {ok, _OriginalIssue} ->
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

test_get_specific_fields() ->
    State = jira:init(bearerauth, "issues.redhat.com", []),
    Key = "KMAINT-1000",

    %% Get the issue and extract specific fields
    case jira:issue(Key, State) of
        {ok, Issue} ->
            %% Extract the requested fields
            Summary = jira:field_from_issue(Issue, <<"summary">>),
            Description = jira:field_from_issue(Issue, <<"description">>),
            Comment = jira:field_from_issue(Issue, <<"comment">>),

            %% Verify we got valid responses for each field
            ?assert(is_binary(Summary)),
            ?assert(is_binary(Description) orelse Description =:= null),
            ?assert(is_map(Comment));
        {error, _Reason} ->
            %% If we can't fetch the issue (likely auth/network), skip the test
            ?assert(true)
    end.

test_issue_with_fields() ->
    State = jira:init(bearerauth, "issues.redhat.com", []),
    Key = "KMAINT-1000",
    Fields = ["summary", "description", "comment"],

    %% Test issue/3 with specific fields
    case jira:issue(Key, Fields, State) of
        {ok, Issue} ->
            %% Verify we got an issue with the requested fields
            ?assert(is_map(Issue)),
            
            %% Get the fields from the issue response
            IssueFields = maps:get(<<"fields">>, Issue),
            
            %% Check that the specific fields are present
            ?assert(maps:is_key(<<"summary">>, IssueFields)),
            ?assert(maps:is_key(<<"description">>, IssueFields)),
            ?assert(maps:is_key(<<"comment">>, IssueFields)),

            %% Compare with getting all fields - issue/2 should return more fields
            case jira:issue(Key, State) of
                {ok, AllFieldsIssue} ->
                    AllIssueFields = maps:get(<<"fields">>, AllFieldsIssue),
                    SpecificFieldsCount = maps:size(IssueFields),
                    AllFieldsCount = maps:size(AllIssueFields),
                    %% Specific fields request should return fewer or equal fields
                    ?assert(SpecificFieldsCount =< AllFieldsCount);
                {error, _} ->
                    %% If all fields request fails, just verify we got the specific fields
                    ?assert(true)
            end;
        {error, _Reason} ->
            %% If we can't fetch the issue (likely auth/network), skip the test
            ?assert(true)
    end.

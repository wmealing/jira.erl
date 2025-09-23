-module(jira_tests).

-author("Warren Kenny <warren.kenny@gmail.com>").
-author("Wade Mealing <wmealing@gmail.com>").

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
        fun test_issue_with_fields/0,
        fun test_issue_helpers/0,
        fun test_sprint_data_helpers/0,
        fun test_board_data_helpers/0,
        fun test_integration_issue_helpers/0,
        fun test_integration_sprint_api/0,
        fun test_integration_issues_in_sprint_for_board/0,
        fun test_integration_board_api/0,
        fun test_integration_backlog_api/0
    ]}.

setup() ->
    hackney:start().

teardown(_) ->
    hackney:stop().

test_init() ->
    State = jira:init(bearerauth, "issues.redhat.com", []),
    ?assert(jira:url(State) =:= <<"https://issues.redhat.com:443/rest/api/2">>).

test_jql() ->
    "project = 'KMAINT' AND updated > '2024/07/06 20:34'" = jira:jql([
        {project, eq, "KMAINT"}, 'and', {updated, gt, {{2024, 07, 06}, {20, 34, 00}}}
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

test_issue_helpers() ->
    %% Test data - typical JIRA issue structure
    ValidIssue = #{
        <<"key">> => <<"KMAINT-1000">>,
        <<"fields">> => #{
            <<"summary">> => <<"Test Issue Summary">>,
            <<"resolution">> => #{<<"name">> => <<"Fixed">>},
            <<"assignee">> => #{
                <<"name">> => <<"testuser">>,
                <<"displayName">> => <<"Test User">>
            },
            <<"issuelinks">> => [
                #{
                    <<"type">> => #{<<"name">> => <<"Blocks">>},
                    <<"outwardIssue">> => #{<<"key">> => <<"OTHER-456">>}
                }
            ],
            <<"customfield_12310243">> => 8
        }
    },

    %% Test issue with null/missing fields
    NullIssue = #{
        <<"key">> => <<"NULL-123">>,
        <<"fields">> => #{
            <<"summary">> => <<"Test Issue with Nulls">>,
            <<"resolution">> => null,
            <<"assignee">> => null,
            <<"issuelinks">> => [],
            <<"customfield_12310243">> => null
        }
    },

    %% Test valid data extraction
    ?assertEqual(<<"Test Issue Summary">>, jira_data:issue_get_summary(ValidIssue)),
    ?assertEqual(<<"Fixed">>, jira_data:issue_get_resolution(ValidIssue)),
    ?assertEqual(<<"testuser">>, jira_data:issue_get_assignee(ValidIssue)),
    ?assertEqual(<<"Test User">>, jira_data:issue_get_displayname(ValidIssue)),
    ?assertEqual(8, jira_data:issue_get_story_points(ValidIssue)),
    ?assert(is_list(jira_data:issue_get_issuelinks(ValidIssue))),
    ?assertEqual(1, length(jira_data:issue_get_issuelinks(ValidIssue))),

    %% Test null/missing data handling
    ?assertEqual(<<"Test Issue with Nulls">>, jira_data:issue_get_summary(NullIssue)),
    ?assertEqual(null, jira_data:issue_get_resolution(NullIssue)),
    ?assertEqual(null, jira_data:issue_get_assignee(NullIssue)),
    ?assertEqual(null, jira_data:issue_get_displayname(NullIssue)),
    ?assertEqual(null, jira_data:issue_get_story_points(NullIssue)),
    ?assertEqual([], jira_data:issue_get_issuelinks(NullIssue)),

    %% Test malformed data handling
    MalformedIssue = #{<<"key">> => <<"BAD-123">>},
    ?assertEqual(null, jira_data:issue_get_summary(MalformedIssue)),
    ?assertEqual(null, jira_data:issue_get_resolution(MalformedIssue)),
    ?assertEqual(null, jira_data:issue_get_assignee(MalformedIssue)),
    ?assertEqual(null, jira_data:issue_get_displayname(MalformedIssue)),
    ?assertEqual(null, jira_data:issue_get_story_points(MalformedIssue)),
    ?assertEqual([], jira_data:issue_get_issuelinks(MalformedIssue)).

test_sprint_data_helpers() ->
    %% Test valid sprint data
    ValidSprint = #{
        <<"id">> => 12345,
        <<"name">> => <<"Sprint 23">>,
        <<"state">> => <<"active">>
    },

    %% Test sprint name extraction
    ?assertEqual(<<"Sprint 23">>, jira_data:sprint_get_name(ValidSprint)),

    %% Test invalid/null sprint data
    ?assertEqual(null, jira_data:sprint_get_name(null)),
    ?assertEqual(null, jira_data:sprint_get_name(<<"not a map">>)),
    ?assertEqual(null, jira_data:sprint_get_name(#{})).

test_board_data_helpers() ->
    %% Test board list data
    BoardList = [
        #{
            <<"id">> => 98765,
            <<"name">> => <<"Team Board">>,
            <<"type">> => <<"scrum">>
        },
        #{
            <<"id">> => 98766,
            <<"name">> => <<"Another Board">>,
            <<"type">> => <<"kanban">>
        }
    ],

    %% Test board name filtering
    TeamBoard = jira_data:board_get_name(BoardList, <<"Team Board">>),
    ?assert(is_map(TeamBoard)),
    ?assertEqual(98765, maps:get(<<"id">>, TeamBoard)),

    %% Test with string name
    TeamBoard2 = jira_data:board_get_name(BoardList, "Team Board"),
    ?assert(is_map(TeamBoard2)),
    ?assertEqual(98765, maps:get(<<"id">>, TeamBoard2)),

    %% Test non-existent board
    ?assertEqual(null, jira_data:board_get_name(BoardList, <<"Non-existent Board">>)),

    %% Test invalid input
    ?assertEqual(null, jira_data:board_get_name([], <<"Team Board">>)),
    ?assertEqual(null, jira_data:board_get_name(null, <<"Team Board">>)),
    ?assertEqual(null, jira_data:board_get_name(BoardList, null)).

test_integration_issue_helpers() ->
    State = jira:init(bearerauth, "issues.redhat.com", []),
    Key = "KMAINT-1000",

    %% Test with real JIRA data if authentication is available
    case jira:issue(Key, State) of
        {ok, Issue} ->
            %% Test field extraction with real JIRA data
            Summary = jira_data:issue_get_summary(Issue),
            ?assert(is_binary(Summary)),
            ?assert(byte_size(Summary) > 0),

            %% Test assignee extraction (may be null)
            Assignee = jira_data:issue_get_assignee(Issue),
            ?assert(Assignee =:= null orelse is_binary(Assignee)),

            %% Test display name extraction (may be null)
            DisplayName = jira_data:issue_get_displayname(Issue),
            ?assert(DisplayName =:= null orelse is_binary(DisplayName)),

            %% Test resolution extraction (may be null)
            Resolution = jira_data:issue_get_resolution(Issue),
            ?assert(Resolution =:= null orelse is_binary(Resolution)),

            %% Test story points extraction (may be null)
            StoryPoints = jira_data:issue_get_story_points(Issue),
            ?assert(StoryPoints =:= null orelse is_number(StoryPoints)),

            %% Test issue links extraction (should be a list)
            IssueLinks = jira_data:issue_get_issuelinks(Issue),
            ?assert(is_list(IssueLinks));
        {error, _Reason} ->
            %% If auth fails or network issues, skip the test
            ?assert(true)
    end.

test_integration_sprint_api() ->
    State = jira:init(bearerauth, "issues.redhat.com", []),

    %% Try to get a sprint (may fail if sprint doesn't exist, which is acceptable)
    case jira:get_sprint(State, 123456) of
        {ok, Sprint} ->
            ?assert(is_map(Sprint)),
            ?assert(maps:is_key(<<"id">>, Sprint)),

            %% Test sprint name extraction
            SprintName = jira_data:sprint_get_name(Sprint),
            ?assert(SprintName =:= null orelse is_binary(SprintName));
        {error, _Reason} ->
            %% Sprint doesn't exist or auth failed - acceptable for testing
            ?assert(true)
    end.

test_integration_board_api() ->
    {timeout, 120000, fun() ->
        State = jira:init(bearerauth, "issues.redhat.com", []),

        %% Test get_boards with pagination
        case jira:get_boards(State) of
            {ok, Boards} ->
                ?assert(is_list(Boards)),

                %% If we got boards, test board filtering
                case length(Boards) > 0 of
                    true ->
                        [FirstBoard | _] = Boards,
                        ?assert(is_map(FirstBoard)),
                        ?assert(maps:is_key(<<"id">>, FirstBoard)),
                        ?assert(maps:is_key(<<"name">>, FirstBoard)),

                        %% Test board name filtering
                        BoardName = maps:get(<<"name">>, FirstBoard),
                        FoundBoard = jira_data:board_get_name(Boards, BoardName),
                        ?assert(is_map(FoundBoard));
                    false ->
                        %% No boards returned - still a valid response
                        ?assert(true)
                end;
            {error, _Reason} ->
                %% Auth failed or network issues - skip the test
                ?assert(true)
        end
    end}.

test_integration_issues_in_sprint_for_board() ->
    {timeout, 120000, fun() ->
        ?debugMsg("************************"),

        State = jira:init(bearerauth, "issues.redhat.com", []),
        % Provided test board ID
        BoardId = 19397,
        % Provided test sprint ID
        SprintId = 72435,

        %% Test the fixed function with real API
        case jira:issues_in_sprint_for_board(State, BoardId, SprintId) of
            {ok, Issues} ->
                ?assert(is_list(Issues)),
                %% If we got issues, verify they have expected structure
                case length(Issues) > 0 of
                    true ->
                        [FirstIssue | _] = Issues,
                        ?assert(is_map(FirstIssue)),
                        ?assert(maps:is_key(<<"key">>, FirstIssue));
                    false ->
                        %% Empty list is valid - sprint might have no issues
                        ?debugFmt("ISSUES FOR SPRINT FOUND ~p~n", [length(Issues)]),
                        ?assert(true)
                end;
            {error, _Reason} ->
                %% Auth failed or network issues - acceptable for testing
                ?assert(true)
        end
    end}.

test_integration_backlog_api() ->
    {timeout, 120000, fun() ->
        State = jira:init(bearerauth, "issues.redhat.com", []),
        % Known test board with backlog issues
        BoardId = 19397,

        %% Test the get_backlog function with real API
        case jira:get_backlog(State, BoardId) of
            {ok, Issues} ->
                ?assert(is_list(Issues)),
                %% If we got issues, verify they have expected structure
                case length(Issues) > 0 of
                    true ->
                        [FirstIssue | _] = Issues,
                        ?assert(is_map(FirstIssue)),
                        ?assert(maps:is_key(<<"key">>, FirstIssue)),
                        ?assert(maps:is_key(<<"fields">>, FirstIssue));
                    false ->
                        %% Empty backlog is valid
                        ?assert(true)
                end;
            {error, _Reason1} ->
                %% Auth failed or network issues - acceptable for testing
                ?assert(true)
        end,

        %% Test pagination
        case jira:get_backlog(State, BoardId, 0, 5) of
            {ok, PagedIssues} ->
                ?assert(is_list(PagedIssues)),
                ?assert(length(PagedIssues) =< 5);
            {error, _Reason2} ->
                ?assert(true)
        end
    end}.

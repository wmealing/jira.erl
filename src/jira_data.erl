-module(jira_data).
-author("Wade Mealing <wmealing@gmail.com>").

%% Issue field helper functions
-export([
    issue_get_resolution/1,
    issue_get_issuelinks/1,
    issue_get_assignee/1,
    issue_get_displayname/1,
    issue_get_summary/1,
    issue_get_story_points/1
]).

%% Sprint and board data helper functions
-export([
    sprint_get_name/1,
    board_get_name/2
]).

%%
%% Issue Field Helper Functions
%%

%%
%% Get the resolution name from an issue
%%
issue_get_resolution(Issue) ->
    try
        Fields = jira:field_from_issue(Issue, <<"resolution">>),
        case Fields of
            null ->
                null;
            Resolution when is_map(Resolution) ->
                maps:get(<<"name">>, Resolution, null);
            _ ->
                null
        end
    catch
        error:{badkey, _} -> null;
        error:{badmatch, _} -> null
    end.

%%
%% Get the issue links from an issue
%%
issue_get_issuelinks(Issue) ->
    try
        jira:field_from_issue(Issue, <<"issuelinks">>)
    catch
        error:{badkey, _} -> [];
        error:{badmatch, _} -> []
    end.

%%
%% Get the assignee name from an issue
%%
issue_get_assignee(Issue) ->
    try
        Fields = jira:field_from_issue(Issue, <<"assignee">>),
        case Fields of
            null ->
                null;
            Assignee when is_map(Assignee) ->
                maps:get(<<"name">>, Assignee, null);
            _ ->
                null
        end
    catch
        error:{badkey, _} -> null;
        error:{badmatch, _} -> null
    end.

%%
%% Get the assignee display name from an issue
%%
issue_get_displayname(Issue) ->
    try
        Fields = jira:field_from_issue(Issue, <<"assignee">>),
        case Fields of
            null ->
                null;
            Assignee when is_map(Assignee) ->
                maps:get(<<"displayName">>, Assignee, null);
            _ ->
                null
        end
    catch
        error:{badkey, _} -> null;
        error:{badmatch, _} -> null
    end.

%%
%% Get the summary from an issue
%%
issue_get_summary(Issue) ->
    try
        jira:field_from_issue(Issue, <<"summary">>)
    catch
        error:{badkey, _} -> null;
        error:{badmatch, _} -> null
    end.

%%
%% Get the story points from an issue (customfield_12310243)
%%
issue_get_story_points(Issue) ->
    try
        Fields = jira:field_from_issue(Issue, <<"customfield_12310243">>),
        case Fields of
            null -> null;
            Points when is_number(Points) -> Points;
            _ -> null
        end
    catch
        error:{badkey, _} -> null;
        error:{badmatch, _} -> null
    end.

%%
%% Sprint Data Helper Functions
%%

%%
%% Get the name from sprint data
%%
sprint_get_name(SprintData) when is_map(SprintData) ->
    maps:get(<<"name">>, SprintData, null);
sprint_get_name(_) ->
    null.

%%
%% Board Data Helper Functions
%%

%%
%% Filter boards by name - returns the board with matching name or null
%%
board_get_name(BoardData, Name) when is_list(BoardData), is_binary(Name) ->
    case
        lists:filter(
            fun(Board) ->
                case maps:get(<<"name">>, Board, null) of
                    Name -> true;
                    _ -> false
                end
            end,
            BoardData
        )
    of
        [Board | _] -> Board;
        [] -> null
    end;
board_get_name(BoardData, Name) when is_list(BoardData), is_list(Name) ->
    board_get_name(BoardData, list_to_binary(Name));
board_get_name(_, _) ->
    null.

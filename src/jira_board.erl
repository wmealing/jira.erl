-module(jira_board).
-author("Wade Mealing <wmealing@gmail.com>").

-export([
    get_backlog/2,
    get_backlog/3,
    get_backlog/4
]).

-export([board_get_name/2]).

%%
%% Get backlog issues for a specific board
%%
-spec get_backlog(term(), integer() | string()) -> {ok, [map()]} | {error, term()}.
get_backlog(State, BoardId) ->
    get_backlog(State, BoardId, 0, 50).

%%
%% Get backlog issues for a specific board with starting offset
%%
-spec get_backlog(term(), integer() | string(), integer()) -> {ok, [map()]} | {error, term()}.
get_backlog(State, BoardId, StartAt) ->
    get_backlog(State, BoardId, StartAt, 50).

%%
%% Get backlog issues for a specific board with pagination parameters
%%
-spec get_backlog(term(), integer() | string(), integer(), integer()) ->
    {ok, [map()]} | {error, term()}.
get_backlog(State, BoardId, StartAt, MaxResults) ->
    BaseURL = jira:url(State),
    AgileURL = binary:replace(BaseURL, <<"/rest/api/2">>, <<"/rest/agile/1.0">>),
    URL = want:binary(
        url:join(AgileURL, ["board", want:string(BoardId), "backlog"]) ++
            "?startAt=" ++ want:string(StartAt) ++
            "&maxResults=" ++ want:string(MaxResults)
    ),

    {Headers, Options} = jira:get_auth_headers_and_options(State),
    case hackney:get(URL, Headers, <<>>, Options) of
        {ok, 200, _Headers, Ref} ->
            {ok, ResponseBody} = hackney:body(Ref),
            JSXOptions = jira:jsx_options(State),
            ResponseJSON = jsx:decode(ResponseBody, lists:append([return_maps], JSXOptions)),
            Issues = jira:json_get(<<"issues">>, ResponseJSON, State),
            {ok, Issues};
        {ok, _Status, _Headers, Ref} ->
            {ok, ErrorBody} = hackney:body(Ref),
            {error, ErrorBody};
        {error, Reason} ->
            {error, want:binary(Reason)}
    end.


%%
%% Filter boards by name - returns the board with matching name or null
%%
board_get_name(BoardData, Name) when is_list(BoardData), is_binary(Name) ->
    case lists:filter(fun(Board) ->
        case maps:get(<<"name">>, Board, null) of
            Name -> true;
            _ -> false
        end
    end, BoardData) of
        [Board|_] -> Board;
        [] -> null
    end;
board_get_name(BoardData, Name) when is_list(BoardData), is_list(Name) ->
    board_get_name(BoardData, list_to_binary(Name));
board_get_name(_, _) ->
    null.

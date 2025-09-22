-module(jira_board).

%%
%% Board Data Helper Functions
%%

-export([board_get_name/2]).

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

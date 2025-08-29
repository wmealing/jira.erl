-module(jira).
-author("Warren Kenny <warren.kenny@gmail.com>").

-export([
    init/5, init/4, init/3, url/1, issue/2, search/3, search/5, jql/1, update_issue/3, get_myself/1
]).

-export([key_from_issue/1, field_from_issue/2]).

-ifdef(TEST).
-export([json_get/3]).
-endif.

-record(state, {
    username :: binary() | undefined,
    password :: binary() | undefined,
    authtype :: atom() | undefined,
    api_key :: binary() | undefined,
    url :: binary(),
    jsx_options :: proplists:proplist()
}).

-type issue() :: map().
-export_type([issue/0]).

%%
%%  Initialize a JIRA handle for use in API function calls
%%
-spec init(string(), string(), string(), integer(), proplists:proplist()) -> #state{}.
init(Username, Password, Host, Port, JSXOptions) ->
    #state{
        username = want:binary(Username),
        password = want:binary(Password),
        url = want:binary("https://" ++ Host ++ ":" ++ want:string(Port) ++ "/rest/api/2"),
        jsx_options = JSXOptions
    }.

-spec init(string(), string(), proplists:proplist()) -> #state{}.
init(bearerauth, Host, JSXOptions) ->
    #state{
        authtype = bearerauth,
        url = want:binary("https://" ++ Host ++ ":" ++ "443" ++ "/rest/api/2"),
        jsx_options = JSXOptions
    };
init(Username, Password, Host) ->
    init(Username, Password, Host, 443, []).

-spec init(string(), string(), string(), proplists:proplist()) -> #state{}.
init(Username, Password, Host, JSXOptions) ->
    init(Username, Password, Host, 443, JSXOptions).

%%
%%  Retrieve the REST API URL for the given state
%%
-spec url(#state{}) -> string().
url(#state{url = URL}) -> URL.

%%
%%  Get the issue with the given key
%%
-spec issue(string(), #state{}) -> {ok, issue()} | {error, term()}.
issue(Key, State = #state{url = BaseURL, jsx_options = JSXOptions}) ->
    URL = want:binary(url:join(BaseURL, ["issue", Key])),
    {Headers, Options} = get_auth_headers_and_options(State),
    case hackney:get(URL, Headers, <<>>, Options) of
        {ok, 200, _Headers, Ref} ->
            {ok, ResponseBody} = hackney:body(Ref),
            Issue = jsx:decode(ResponseBody, lists:append([return_maps], JSXOptions)),
            {ok, Issue};
        {ok, _Status, _Headers, Ref} ->
            {ok, ErrorBody} = hackney:body(Ref),
            {error, ErrorBody};
        {error, Reason} ->
            {error, want:binary(Reason)}
    end.

%%
%%  Update an issue with the given key using the provided fields map
%%
-spec update_issue(string(), map(), #state{}) -> ok | {error, term()}.
update_issue(Key, Fields, State = #state{url = BaseURL}) ->
    URL = want:binary(url:join(BaseURL, ["issue", Key])),
    Body = #{fields => Fields},
    {Headers, Options} = get_auth_headers_and_options(State),
    AllHeaders = [{<<"Content-Type">>, <<"application/json">>} | Headers],
    case hackney:put(URL, AllHeaders, jsx:encode(Body), Options) of
        {ok, 204, _Headers, _Ref} ->
            ok;
        {ok, _Status, _Headers, Ref} ->
            {ok, ErrorBody} = hackney:body(Ref),
            {error, ErrorBody};
        {error, Reason} ->
            {error, want:binary(Reason)}
    end.

%%
%%  Get current user information to test authentication
%%
-spec get_myself(#state{}) -> {ok, map()} | {error, term()}.
get_myself(State = #state{url = BaseURL, jsx_options = JSXOptions}) ->
    URL = want:binary(url:join(BaseURL, ["myself"])),
    {Headers, Options} = get_auth_headers_and_options(State),
    case hackney:get(URL, Headers, <<>>, Options) of
        {ok, 200, _Headers, Ref} ->
            {ok, ResponseBody} = hackney:body(Ref),
            UserInfo = jsx:decode(ResponseBody, lists:append([return_maps], JSXOptions)),
            {ok, UserInfo};
        {ok, _Status, _Headers, Ref} ->
            {ok, ErrorBody} = hackney:body(Ref),
            {error, ErrorBody};
        {error, Reason} ->
            {error, want:binary(Reason)}
    end.

%%
%%  Search for any issues matching the given JQL string, collect all results by traversing all pages returned
%%
-spec search(string(), [binary()], #state{}) -> {ok, [issue()]} | {error, term()}.
search(JQL, Fields, State) ->
    search(JQL, Fields, State, 0, 50, 100, []).

-spec search(string(), [binary()], #state{}, integer(), integer(), integer(), [issue()]) ->
    {ok, [issue()]} | {error, term()}.
search(JQL, Fields, State, StartAt, Max, Total, Out) when StartAt =< Total ->
    case search(JQL, StartAt, Max, Fields, State) of
        {ok, Issues, NewTotal} ->
            search(JQL, Fields, State, StartAt + Max, Max, NewTotal, lists:append(Out, Issues));
        {error, Reason} ->
            {error, Reason}
    end;
search(_JQL, _Fields, _State, StartAt, _Max, Total, Out) when StartAt > Total ->
    {ok, Out}.

json_get(Key, JSON, #state{jsx_options = JSXOptions}) ->
    case lists:keyfind(labels, 1, JSXOptions) of
        {labels, atom} -> maps:get(want:atom(Key), JSON);
        {labels, existing_atom} -> maps:get(want:atom(Key), JSON);
        {labels, attempt_atom} -> maps:get(want:atom(Key), JSON);
        _ -> maps:get(want:binary(Key), JSON)
    end.

%%
%%  Search for any issues matching the given JQL string, starting at the given offset and returning the specified maximum number
%%  of results. On success, returns the total number of results available as well as the list of retrieved issues.
%%
-spec search(string(), integer(), integer(), [binary()], #state{}) ->
    {ok, [issue()]} | {error, term()}.
search(JQL, Start, Max, Fields, State = #state{url = BaseURL, jsx_options = JSXOptions}) ->
    Body = #{
        jql => want:binary(JQL),
        startAt => Start,
        maxResults => Max,
        fields => [want:binary(F) || F <- Fields]
    },
    URL = want:binary(url:join(BaseURL, ["search"])),
    {Headers, Options} = get_auth_headers_and_options(State),
    AllHeaders = [{<<"Content-Type">>, <<"application/json">>} | Headers],
    case hackney:post(URL, AllHeaders, jsx:encode(Body), Options) of
        {ok, 200, _Headers, Ref} ->
            {ok, ResponseBody} = hackney:body(Ref),
            ResponseJSON = jsx:decode(ResponseBody, lists:append([return_maps], JSXOptions)),
            Total = json_get(<<"total">>, ResponseJSON, State),
            Issues = json_get(<<"issues">>, ResponseJSON, State),
            {ok, Issues, Total};
        {ok, _Status, _Headers, Ref} ->
            {ok, ErrorBody} = hackney:body(Ref),
            {error, ErrorBody};
        {error, Reason} ->
            {error, want:binary(Reason)}
    end.

jql(Filters) when is_list(Filters) ->
    jql(Filters, "").

jql(['and' | T], Query) ->
    jql(T, string:join([Query, "AND"], " "));
jql(['or' | T], Query) ->
    jql(T, string:join([Query, "OR"], " "));
jql([{Key, Operator, Value} | T], Query) ->
    jql(T, string:join([Query, want:string(Key), jql_operator(Operator), jql_value(Value)], " "));
jql([], Query) ->
    string:strip(Query).

jql_operator(gt) -> ">";
jql_operator(lt) -> "<";
jql_operator(eq) -> "=";
jql_operator(contains) -> "~";
jql_operator(_) -> "".

jql_value({{Y, M, D}, {HH, MM, _SS}}) ->
    lists:flatten(io_lib:format("'~w/~2..0w/~2..0w ~2..0w:~2..0w'", [Y, M, D, HH, MM]));
jql_value({{Y, M, D}, {HH, MM, _SS, _MS}}) ->
    lists:flatten(io_lib:format("'~w/~2..0w/~2..0w ~2..0w:~2..0w'", [Y, M, D, HH, MM]));
jql_value(Value) ->
    lists:append(["'", want:string(Value), "'"]).

get_auth_headers_and_options(#state{authtype = bearerauth}) ->
    ApiKey = os:getenv("JIRA_KEY"),
    AuthKey = <<"Authorization">>,
    BearerPrefix = <<"Bearer ">>,
    ApiKeyBinary = list_to_binary(ApiKey),
    BearerValue = <<BearerPrefix/binary, ApiKeyBinary/binary>>,
    {[{AuthKey, BearerValue}], []};
get_auth_headers_and_options(#state{username = Username, password = Password}) ->
    {[], [{basic_auth, {Username, Password}}]}.

%% Helper functions.
from_issue(Issue, Key) ->
    #{Key := Value} = Issue,
    Value.

key_from_issue(Issue) ->
    from_issue(Issue, <<"key">>).

field_from_issue(Issue, Key) ->
    Fields = from_issue(Issue, <<"fields">>),
    #{Key := Value} = Fields,
    Value.

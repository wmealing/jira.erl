-module( jira ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).

-export( [init/5, init/4, init/3, url/1, issue/2, search/3, search/5, jql/1] ).

-record( state, {   username        :: binary(),
                    password        :: binary(),
                    url             :: binary(),
                    jsx_options     :: proplists:proplist()
} ).

-type issue() :: map().
-export_type( [issue/0] ).

%%
%%  Initialize a JIRA handle for use in API function calls
%%
-spec init( string(), string(), string(), integer(), proplists:proplist() ) -> #state{}.
init( Username, Password, Host, Port, JSXOptions ) ->
    #state{     username    = want:binary( Username ),
                password    = want:binary( Password ),
                url         = want:binary( "https://" ++ Host ++ ":" ++ want:string( Port ) ++ "/rest/api/2" ),
                jsx_options = JSXOptions
    }.
    
-spec init( string(), string(), string(), proplists:proplist() ) -> #state{}.
init( Username, Password, Host, JSXOptions ) ->
    init( Username, Password, Host, 443, JSXOptions ).
    
-spec init( string(), string(), string() ) -> #state{}.
init( Username, Password, Host ) ->
    init( Username, Password, Host, 443, [] ).
    
%%
%%  Retrieve the REST API URL for the given state
%%
-spec url( #state{} ) -> string().
url( #state{ url = URL } ) -> URL.

%%
%%  Get the issue with the given key
%%
-spec issue( string(), #state{} ) -> { ok, issue() } | { error, term() }.
issue( Key, #state{ url = BaseURL, username = Username, password = Password, jsx_options = JSXOptions } ) ->
    URL = want:binary( url:join( BaseURL, [ "issue", Key ] ) ),
    Options = [{ basic_auth, { Username, Password } } ],
    case hackney:get( URL, [], <<>>, Options ) of
        { ok, 200, _Headers, Ref } ->
            { ok, ResponseBody } = hackney:body( Ref ),
            Issue = jsx:decode( ResponseBody, lists:append( [ return_maps ], JSXOptions ) ),
            { ok, Issue };
        { ok, _Status, _Headers, Ref } ->
            { ok, ErrorBody } = hackney:body( Ref ),
            { error, ErrorBody };
        { error, Reason } ->
            { error, want:binary( Reason ) }
    end.

%%
%%  Search for any issues matching the given JQL string, collect all results by traversing all pages returned
%%
-spec search( string(), [binary()], #state{} ) -> { ok, [issue()] } | { error, term() }.
search( JQL, Fields, State ) ->
    search( JQL, Fields, State, 0, 50, 100, [] ).
   
-spec search( string(), [binary()], #state{}, integer(), integer(), integer(), [issue()] ) -> { ok, [issue()] } | { error, term() }.
search( JQL, Fields, State, StartAt, Max, Total, Out ) when StartAt =< Total ->
    case search( JQL, StartAt, Max, Fields, State ) of
        { ok, Issues, NewTotal }    -> search( JQL, Fields, State, StartAt + Max, Max, NewTotal, lists:append( Out, Issues ) );
        { error, Reason }           -> { error, Reason }
    end;
    
search( _JQL, _Fields, _State, StartAt, _Max, Total, Out ) when StartAt > Total ->
    { ok, Out }.
    
%%
%%  Search for any issues matching the given JQL string, starting at the given offset and returning the specified maximum number
%%  of results. On success, returns the total number of results available as well as the list of retrieved issues.
%%
-spec search( string(), integer(), integer(), [binary()], #state{} ) -> { ok, [issue()] } | { error, term() }.
search( JQL, Start, Max, Fields, #state{ username = Username, password = Password, url = BaseURL, jsx_options = JSXOptions } ) ->
    Body = #{   jql         => want:binary( JQL ),
                startAt     => Start,
                maxResults  => Max,
                fields      => [ want:binary( F ) || F <- Fields ]                
    },
    Options = [{ basic_auth, { Username, Password } } ],
    URL = want:binary( url:join( BaseURL, [ "search" ] ) ),
    case hackney:post( URL, [{ <<"Content-Type">>, <<"application/json">> }], jsx:encode( Body ), Options ) of
        { ok, 200, _Headers, Ref } ->
            { ok, ResponseBody } = hackney:body( Ref ),
            ResponseJSON = jsx:decode( ResponseBody, lists:append( [ return_maps ], JSXOptions ) ),
            Total   = maps:get( <<"total">>, ResponseJSON ),
            Issues  = maps:get( <<"issues">>, ResponseJSON ),
            { ok, Issues, Total };
        { ok, _Status, _Headers, Ref } ->
            { ok, ErrorBody } = hackney:body( Ref ),
            { error, ErrorBody };
        { error, Reason } ->
            { error, want:binary( Reason ) }
    end.

jql( Filters ) when is_list( Filters ) ->
    jql( Filters, "" ).

jql( ['and' | T], Query ) ->
    jql( T, string:join( [Query, "AND"], " " ) );

jql( ['or' | T], Query ) ->
    jql( T, string:join( [Query, "OR"], " " ) );

jql( [{ Key, Operator, Value } | T], Query ) ->
    jql( T, string:join( [Query, want:string( Key ), jql_operator( Operator ), jql_value( Value )], " " ) );

jql( [], Query ) ->
    string:strip( Query ).

jql_operator( gt )  -> ">";
jql_operator( lt )  -> "<";
jql_operator( eq )  -> "=";
jql_operator( _ )   -> "".

jql_value( { { Y, M, D }, { HH, MM, _SS } } )       -> lists:flatten( io_lib:format( "'~w/~2..0w/~2..0w ~2..0w:~2..0w'", [Y, M, D, HH, MM] ) );
jql_value( Value )                                  -> lists:append( ["'", want:string( Value ), "'" ] ).
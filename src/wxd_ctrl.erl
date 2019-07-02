-module(wxd_ctrl).

%%% @doc
-behaviour(gen_statem).

-include_lib("kernel/include/logger.hrl").
-include_lib("kernel/include/file.hrl").

%% API
-export([start_link/0]).
%% 'events' API
-export([raise/0, stop_app/0, call/1, select_demo/1, select_cat/1]).

%% gen_statem callbacks
-export([init/1, callback_mode/0,
	 handle_event/4, 
	 terminate/3, code_change/4]).

-define(SERVER, ?MODULE).

-record(data, {demos_dir :: file:filename()}).

%%%===================================================================
%%% API
%%%===================================================================

select_demo(Demo_name) ->
    gen_statem:cast(?SERVER, {select_demo, Demo_name}).

select_cat(Cat_name) ->
    gen_statem:cast(?SERVER, {select_cat, Cat_name}).

call(Args) ->
    gen_statem:call(?SERVER, {call, Args}).

stop_app() ->
    gen_statem:cast(?SERVER, stop_app).

raise() ->
    gen_statem:cast(?SERVER, raise).
    
start_link() ->
    gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @private
init([]) ->
    case code:priv_dir(wxd) of
	{error, Reason} ->
	    {stop, {demos_not_found, Reason}};
	Priv_dir ->
	    Demos_dir = filename:join(Priv_dir, "demos"),
	    case file:read_file_info(Demos_dir) of
		{ok, #file_info{type = directory}} ->
		    Data = #data{demos_dir = Demos_dir},
		    {ok, init, Data};
		{_, Reason} ->
		    {stop, {demos_not_found, Reason}}
	    end
    end.

%% @private
callback_mode() ->
    [handle_event_function, state_enter].

%% @private
handle_event(enter, _Old_state, init, _Data) ->
    Child_spec = #{id => view,
		   start => {wxd_view, start_link, []},
		   restart => temporary
		  },
    {ok, _Pid} = supervisor:start_child(wxd_sup, Child_spec),
    keep_state_and_data;
handle_event(cast, stop_app, _State, _Data) ->
    application:stop(wxd),
    keep_state_and_data;
handle_event(cast, {select_demo, Demo_name}, _State, #data{demos_dir = Dir}) ->
    Demo_name1 = string:lowercase(Demo_name),
    File_name = filename:join(Dir, Demo_name1),
    case file:read_file(File_name) of
	{error, Reason} ->
	    ?LOG_ERROR("Error ~p reading demo ~p", [Reason, File_name]);
	{ok, _Content} ->
	    ?LOG_WARNING("Selected demo ~p~n", [Demo_name1]),
	    wxd_view:load_doc("Blabla")
    end, 
    keep_state_and_data;
handle_event(cast, {select_cat, Cat_name}, _State, _Data) ->
    ?LOG_WARNING("Say someting about the category ~p~n", [Cat_name]),
    keep_state_and_data;
handle_event({call, From} = _Event_type, Event_content, _State, _Data) ->
    ?LOG_WARNING("Unhandled call ~p~n", [Event_content]),
    Reply = ok,
    {keep_state_and_data, [{reply, From, Reply}]};
handle_event(Event_type, Event_content, State, _Data) ->
    ?LOG_WARNING("Unhandled event ~p, ~p, ~p~n", [Event_type, Event_content, State]),
    keep_state_and_data.

%% @private
terminate(_Reason, _State, _Data) ->
    ok.

%% @private
code_change(_Old_vsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

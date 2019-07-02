%%%-------------------------------------------------------------------
%%% @doc
%%% The demo's main frame
%%% @end
%%%-------------------------------------------------------------------
-module(empty_wxobject).

-behaviour(wx_object).

-include_lib("wx/include/wx.hrl").

%% API
-export([start_link/0]).

%% wx_object callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 handle_event/2, terminate/2, code_change/3]).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> wxWindow:wxWindow().
start_link() ->
    wx_object:start_link(?MODULE, [], []).

%%%===================================================================
%%% wx_object callbacks
%%%===================================================================

init([]) ->
    Frame = undefined,
    {Frame, #state{}}.

handle_event(#wx{} = Evt, State) ->
    io:format("Unhandled wx event ~p~n", [Evt]),
    {noreply, State}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

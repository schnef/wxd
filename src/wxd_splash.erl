%%%-------------------------------------------------------------------
%%% @doc
%%% The demo's splash screen
%%% TODO: This still gives a segfault when the splash screen is clicked.
%%% @end
%%%-------------------------------------------------------------------
-module(wxd_splash).

-behaviour(wx_object).

-include_lib("wx/include/wx.hrl").
-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/0]).

%% wx_object callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 handle_event/2, terminate/2, code_change/3]).

-record(state, {splash}).

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
    Bmp = wxd_util:make_bmp("erlang.png"),
    Splash = wxSplashScreen:new(Bmp, ?wxSPLASH_CENTRE_ON_SCREEN bor ?wxSPLASH_TIMEOUT,
				5000, wx:null(), ?wxID_ANY),
    CB = fun(_Evt, _Obj) ->
		 wxd_ctrl:raise(),
		 wxSplashScreen:destroy(Splash)
	 end,
    wxSplashScreen:connect(Splash, close_window, [{callback, CB}]),
    {Splash, #state{splash = Splash}}.

handle_event(#wx{} = Evt, State) ->
    ?LOG_DEBUG("Unhandled wx event ~p~n", [Evt]),
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

%%%-------------------------------------------------------------------
%% @doc wxd public API
%% @end
%%%-------------------------------------------------------------------

-module(wxd_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    %% Start wx and pass the environment to the supervisor.
    %% wx:new(),
    %% Env = wx:get_env(),
    %% Ret = wxd_sup:start_link(Env),
    %% %% wxd_sup:window(wxd_splash, []), % Splash is an annonymous window
    %% %% wxd_sup:window(wxd, wxd, []), % the main window has its name registered.
    %% Ret.
    wxd_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    %% This seems the proper place to destroy wx.
    %% wx:destroy(),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

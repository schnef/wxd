-module(wxd_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { #{strategy => one_for_all},
	   [#{id => ctrl,
	      start => {wxd_ctrl, start_link, []}
	     }]
	 } }.

%%====================================================================
%% Internal functions
%%====================================================================

%% %%%-------------------------------------------------------------------
%% %% @doc
%% %% wxd top level supervisor.
%% %% @end
%% %%%-------------------------------------------------------------------

%% -module(wxd_sup).

%% -behaviour(supervisor).

%% %% API
%% -export([start_link/1, window/3, window/2]).

%% %% Supervisor callbacks
%% -export([init/1, wx_object/3]).

%% -define(SERVER, ?MODULE).

%% %%====================================================================
%% %% API functions
%% %%====================================================================

%% -spec window(Module :: module(), Args :: term()) -> supervisor:startchild_ret().
%% %%--------------------------------------------------------------------
%% %% @doc
%% %% Starts a new wx_object and adds it to the GUI supervisor.
%% %% @end
%% %%--------------------------------------------------------------------
%% window(Module, Args) ->
%%     window(undefined, Module, Args).

%% -spec window(Name :: atom(), Module :: module(), Args :: term()) -> supervisor:startchild_ret().
%% %%--------------------------------------------------------------------
%% %% @doc
%% %% Starts a new registered wx_object and adds it to the GUI
%% %% supervisor. The first argument is used as the locally registered
%% %% name.
%% %% @end
%% %%--------------------------------------------------------------------
%% window(Name, Module, Args) ->
%%     {ok, _, Window} = supervisor:start_child(?SERVER, [Name, Module, Args]),
%%     Window.

%% start_link(Env) ->
%%     supervisor:start_link({local, ?SERVER}, ?MODULE, [Env]).

%% %%====================================================================
%% %% Supervisor callbacks
%% %%====================================================================

%% init([Env]) ->
%%     wx:set_env(Env),
%%     {ok, { {simple_one_for_one, 1, 5},
%% 	   [#{id => window,
%% 	      start => {?MODULE, wx_object, []},
%% 	      restart => temporary,
%% 	      shutdown => 1000,
%% 	      type => worker
%% 	     }]
%% 	 } }.

%% %%====================================================================
%% %% Internal functions
%% %%====================================================================

%% -spec wx_object(Name :: atom(), Module :: module(), Args :: term()) -> {ok, pid()}.
%% %%--------------------------------------------------------------------
%% %% @private
%% %% @doc
%% %% Create a wx_object, register its name if Name is not undefined, and
%% %% return the wx_object's Pid. This function is called when a new
%% %% child process is added to the GUI supervisor (@see init/1.)
%% %% @end
%% %%--------------------------------------------------------------------
%% wx_object(Name, Module, Args) ->
%%     Window = case Name of
%%                  undefined ->
%%                      wx_object:start_link(Module, Args, []);
%%                  _ ->
%%                      wx_object:start_link({local, Name}, Module, Args, [])
%%              end,
%%     {ok, wx_object:get_pid(Window), Window}.

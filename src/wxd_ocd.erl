%%%-------------------------------------------------------------------
%%% @doc
%%% The demo's overview, code and demo pane
%%% @end
%%%-------------------------------------------------------------------
-module(wxd_ocd).

-behaviour(wx_object).

-include_lib("wx/include/wx.hrl").

%% API
-export([start_link/2]).

%% wx_object callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 handle_event/2, terminate/2, code_change/3]).

-record(state, {panes}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(wxWindow:wxWindow(), wxMenuBar:wxMenuBar()) -> wxWindow:wxWindow().
start_link(Parent, Menu_bar) ->
    wx_object:start_link(?MODULE, [Parent, Menu_bar], []).

%%%===================================================================
%%% wx_object callbacks
%%%===================================================================

init([Parent, Menu_bar]) ->
    %% Create element and set attributes
    NB = wxNotebook:new(Parent, ?wxID_ANY, [{style, ?wxCLIP_CHILDREN}]),
    IL = wxImageList:new(16, 16),
    [wxImageList:add(IL, wxd_util:make_bmp(X)) || X <- [overview, code, demo]],
    wxNotebook:assignImageList(NB, IL),
    wxNotebook:connect(NB, command_notebook_page_changed),

    %% Add content
    %% Ovr = wxHtmlWindow:new(NB),
    %% wxHtmlWindow:setPage(Ovr, "wxErlang"),
    Ovr = wxd_ovr:start_link(NB, Menu_bar),
    wxNotebook:addPage(NB, Ovr, "Overview", [{imageId, 0}]),

    Code = wxd_code:start_link(NB, Menu_bar),
    wxNotebook:addPage(NB, Code, "Code", [{imageId, 1}]),

    Panes = #{0 => Ovr,
	      1 => Code},

    %% Materialize
    {NB, #state{panes = Panes}}.

%% Initial, coming from nowhere
handle_event(#wx{event = #wxBookCtrl{type = command_notebook_page_changed, nOldSel = Old, nSel = New}}, % Overview page
	     #state{panes = Panes} = State) ->
    deactivate(Panes, Old),
    activate(Panes, New),
    {noreply, State};
handle_event(#wx{} = Evt, State) ->
    io:format("OCS Unhandled wx event ~p~n", [Evt]),
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

activate(Panes, Sel) ->
    #{Sel := Obj} = Panes,
    Pid = wx_object:get_pid(Obj),
    Pid ! activate.

deactivate(_Panes, -1) ->
    ok;
deactivate(Panes, Sel) ->
    #{Sel := Obj} = Panes,
    Pid = wx_object:get_pid(Obj),
    Pid ! deactivate.
    

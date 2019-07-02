%%%-------------------------------------------------------------------
%%% @doc
%%% The demo's main frame
%%% @end
%%%-------------------------------------------------------------------
-module(wxd).

-behaviour(wx_object).

-include_lib("wx/include/wx.hrl").
-include("wxd.hrl").

%% API
-export([start_link/0, raise/0]).

%% wx_object callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 handle_event/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {frame, menu_bar, status_bar, aui_mgr}).

%%%===================================================================
%%% API
%%%===================================================================

raise() ->
    wx_object:cast(?SERVER, raise).

-spec start_link() -> wxWindow:wxWindow().
start_link() ->
    wx_object:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% wx_object callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),

    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "wxErlang demo"),
    
    %% Add elements to the frame, such as menu's, a status bar and a
    %% task bar icon
    Menu_bar = wxMenuBar:new(),
    File = wxMenu:new(),
    wxMenu:append(File, ?wxID_EXIT, "&Quit"),
    wxMenuBar:append(Menu_bar, File, "&File"),
    Edit = wxMenu:new(),
    wxMenu:append(Edit, ?wxID_UNDO, "Undo"),
    wxMenu:appendSeparator(Edit),
    wxMenu:append(Edit, ?wxID_CUT, "Cut"),
    wxMenu:append(Edit, ?wxID_COPY, "Copy"),
    wxMenu:append(Edit, ?wxID_PASTE, "Paste"),
    wxMenu:appendSeparator(Edit),
    wxMenuBar:append(Menu_bar, Edit, "&Edit"),
    Help = wxMenu:new(),
    wxMenu:append(Help, ?wxID_ABOUT, "&About"),
    wxMenuBar:append(Menu_bar, Help, "&Help"),
    wxFrame:setMenuBar(Frame, Menu_bar),

    Status_bar = wxStatusBar:new(Frame),
    wxStatusBar:setStatusText(Status_bar, "Welcome to the wxErlang demo"),
    wxFrame:setStatusBar(Frame, Status_bar),

    Icon = wxd_util:make_icon("erlang-logo32.png"),
    wxFrame:setIcon(Frame, Icon),

    task_bar(Frame),

    %% Set properties of the frame, such as minimum size and where it
    %% should appear on the screen
    wxFrame:setMinSize(Frame, {640, 480}),
    wxFrame:center(Frame, [{dir, ?wxBOTH}]),

    %% Event handling: connect particular event types to be handled by
    %% this process, or when more appropriate, to a specific callback
    %% function. 
    wxFrame:connect(Frame, close_window),
    wxFrame:connect(Frame, command_menu_selected),

    %% Add content to the main frame in the form of an AUI managed
    %% navigation area, demo area and logging area.
    Mgr = content(Frame, Menu_bar, Status_bar),
    
    %% Finally, materialize the main frame.
    wxFrame:show(Frame),
    {Frame, #state{frame = Frame, menu_bar = Menu_bar, status_bar = Status_bar, aui_mgr = Mgr}}.

handle_event(#wx{id = ?wxID_ABOUT, event = #wxCommand{}}, #state{frame = Frame} = State) ->
    WxWVer = io_lib:format("~p.~p.~p.~p", [?wxMAJOR_VERSION, ?wxMINOR_VERSION,
					   ?wxRELEASE_NUMBER, ?wxSUBRELEASE_NUMBER]),
    application:load(wx),
    {ok, WxVsn} = application:get_key(wx, vsn),
    Dialog = wxMessageDialog:new(Frame, "WxErlang demo." ++
				     "\n\nFrontend: wx-" ++ WxVsn ++
				     "\nBackend: wxWidgets-" ++ lists:flatten(WxWVer),
				 [{style, ?wxOK bor ?wxICON_INFORMATION},
				  {caption, "About WxErlang demo"}]),
    wxDialog:showModal(Dialog),
    wxDialog:destroy(Dialog),
    {noreply, State};
handle_event(#wx{id = ?wxID_EXIT, event = #wxCommand{type = command_menu_selected}}, State) ->
    close(),
    {noreply, State};
handle_event(#wx{event = #wxClose{type = close_window}}, State) ->
    close(),
    {noreply, State};
handle_event(#wx{} = Evt, State) ->
    io:format("Unhandled wx event ~p~n", [Evt]),
    {noreply, State}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(raise, #state{frame = Frame} = State) ->
    wxFrame:raise(Frame),
    wxFrame:setFocus(Frame),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    io:format("Unhandled Info ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, #state{frame = Frame, aui_mgr = Mgr}) ->
    wxAuiManager:disconnect(Mgr),
    %% Without the next line, we get a Segmentation fault and error
    %% "wxWidgets Assert failure: ../src/common/wincmn.cpp(478):
    %% "GetEventHandler() == this" in ~wxWindowBase() : any pushed
    %% event handlers must have been removed"
    wxAuiManager:unInit(Mgr),
    wxFrame:destroy(Frame),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

close() ->
    %% Use separate process to call stop, otherwise this process will
    %% wait for stop to terminate which only happens after stopping
    %% the application, but the application cannot stop because this
    %% process would still be waiting for stop to return: i.e. prevent
    %% a dead-lock.
    spawn(fun() -> application:stop(wxd) end).

%% A taskbar icon is not a wxWindow subclass and therefor cannot live
%% independently as, for example, the splash screen. 
task_bar(Frame) ->
    Task_bar_icon = wxTaskBarIcon:new(),
    Icon = wxd_util:make_icon("erlang-logo64.png"),
    wxTaskBarIcon:setIcon(Task_bar_icon, Icon, [{tooltip, "wxErlang demo"}]),
    CB = fun(_Evt, _Obj) ->
		wxFrame:raise(Frame)
	end,
    wxTaskBarIcon:connect(Task_bar_icon, taskbar_left_dclick, [{callback, CB}]),
    Task_bar_icon.

%% Content of the main frame. Implements several panes managed by an
%% advanced user interface manager.
content(Frame, Menu_bar, _Status_bar) ->
    %% It might be necessary to put a panel within the frame first and
    %% attach the AUI manager to that instead of the frame. There are
    %% rumors that things don't work properly otherwise.
    Panel = wxPanel:new(Frame, [{style, ?wxTAB_TRAVERSAL bor ?wxCLIP_CHILDREN}]),
    Mgr = wxAuiManager:new([{managed_wnd, Panel}]),
    
    %% Create the panes which the manager will control and add them to
    %% the manager.
    {Nav, Nav_info} = nav(Panel, Menu_bar),
    wxAuiManager:addPane(Mgr, Nav, Nav_info),

    {OCD, OCD_info} = ocd(Panel, Menu_bar),
    wxAuiManager:addPane(Mgr, OCD, OCD_info),

    %% {SCL, SCL_info} = scl(Frame, Menu_bar, undefined),
    %% wxAuiManager:addPane(Mgr, SCK, SCK_info),

    wxAuiManager:connect(Mgr, aui_pane_button, [{skip,true}]),
    wxAuiManager:connect(Mgr, aui_pane_maximize, [{skip,true}]),  

    %% Materialize...
    wxAuiManager:update(Mgr),

    Mgr.

%% Demo navigation pane
nav(Parent, Menu_bar) ->
    %% The navigation pane is implemented as a separate wx_object to
    %% keep the code manageable.
    Nav = wxd_nav:start_link(Parent, Menu_bar),

    %% Now set properties which determine how the AUI manager will
    %% handle the pane, such as position, sizes and appearance.
    Info = wxAuiPaneInfo:new(),
    Info1 = wxAuiPaneInfo:left(Info),
    Info2 = wxAuiPaneInfo:layer(Info1, 1),
    Info3 = wxAuiPaneInfo:bestSize(Info2, {240, 700}),
    Info4 = wxAuiPaneInfo:minSize(Info3, {240, -1}),
    Info5 = wxAuiPaneInfo:closeButton(Info4, [{visible, false}]),
    Info6 = wxAuiPaneInfo:caption(Info5, "wxErlang demos"),
    Info7 = wxAuiPaneInfo:name(Info6, "nav"),
    
    {Nav, Info7}.

%% Overview, code and demo pane
ocd(Parent, Menu_bar) ->
    OCD = wxd_ocd:start_link(Parent, Menu_bar),

    Info = wxAuiPaneInfo:new(),
    Info1 = wxAuiPaneInfo:centrePane(Info),
    Info2 = wxAuiPaneInfo:name(Info1, "ocd"),
    
    {OCD, Info2}.

-module(wxd_view).

-behaviour(wx_object).

-include_lib("wx/include/wx.hrl").
-include_lib("kernel/include/logger.hrl").
-include("wxd.hrl").

%% API
-export([start_link/0, load_doc/1]).

%% wx_object callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 %% handle_sync_event/3,
	 handle_event/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(data, {frame :: wxFrame:wxFrame(), mgr :: wxAuiManager:wxAuiManager(),
	       focus_funs = [] :: [fun()], deact_fun = undefined :: undefined | fun()}).

%%%===================================================================
%%% API
%%%===================================================================

-spec load_doc(Doc :: string()) -> ok.
load_doc(Doc) ->
    wx_object:call(?SERVER, {load_doc, Doc}).

-spec start_link() -> wxWindow:wxWindow().
start_link() ->
    case wx_object:start_link({local, ?SERVER}, ?MODULE, [], []) of
	{error, _Reason} = Error ->
	    Error;
	Window ->
	    {ok, wx_object:get_pid(Window)}
    end.
    

%%%===================================================================
%%% wx_object callbacks
%%%===================================================================

init([]) ->
    wx:new(),
    process_flag(trap_exit, true),

    %% TODO: the splash screen segfaults when clicked!
    %% wxd_splash:start_link(),

    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "wxErlang demo"),

    %% Show splash screen.
    splash(Frame),
    
    %% Add elements to the frame, such as menu's, a status bar and a
    %% task bar icon
    create_menus(Frame),
    create_status_bar(Frame),
    create_task_bar(Frame),
    {Mgr, Focus_funs} = create_aui(Frame),

    %% Set properties of the frame, such as minimum size and where it
    %% should appear on the screen
    Icon = wxd_util:make_icon("erlang-logo32.png"),
    wxFrame:setIcon(Frame, Icon),
    wxFrame:setMinSize(Frame, {800, 600}),
    wxFrame:center(Frame, [{dir, ?wxBOTH}]),

    %% Event handling: connect particular event types to be
    %% handled. Initialy, the application only handles the most basic
    %% events such as closing the main window. Most event handling is
    %% done by the contained widgets and controls.
    wxFrame:connect(Frame, close_window),
    wxFrame:connect(Frame, command_menu_selected),
    %% 

    %% Finally, materialize the main frame.
    wxFrame:show(Frame),
    {Frame, #data{frame = Frame, mgr = Mgr, focus_funs = Focus_funs}}.

handle_event(#wx{id = ?wxID_ABOUT, event = #wxCommand{}}, #data{frame = Frame} = Data) ->
    WxWVer = io_lib:format("~p.~p.~p.~p", [?wxMAJOR_VERSION, ?wxMINOR_VERSION,
					   ?wxRELEASE_NUMBER, ?wxSUBRELEASE_NUMBER]),
    application:load(wx),
    {ok, WxVsn} = application:get_key(wx, vsn),
    Msg = "WxErlang demo."
	"\n\nFrontend: wx-" ++ WxVsn ++
	"\nBackend: wxWidgets-" ++ lists:flatten(WxWVer),
    Dialog = wxMessageDialog:new(Frame, Msg, [{style, ?wxOK bor ?wxICON_INFORMATION},
					      {caption, "About WxErlang demo"}]),
    wxDialog:showModal(Dialog),
    wxDialog:destroy(Dialog),
    {noreply, Data};
handle_event(#wx{id = ?wxID_EXIT, event = #wxCommand{type = command_menu_selected}}, Data) ->
    wxd_ctrl:stop_app(),
    {noreply, Data};
handle_event(#wx{event = #wxClose{type = close_window}}, Data) ->
    wxd_ctrl:stop_app(),
    {noreply, Data};
handle_event(#wx{event = #wxFocus{type = set_focus}, obj = Obj}, #data{focus_funs = Funs} = Data) ->
    case lists:keyfind(Obj, 1, Funs) of
	{_Obj, Fun} ->
	    Deact_fun = Fun(), 
	    {noreply, Data#data{deact_fun = Deact_fun}};
	false ->
	    {noreply, Data}
    end;
handle_event(#wx{event = #wxFocus{type = kill_focus}}, #data{deact_fun = Deact_fun} = Data) ->
    case is_function(Deact_fun) of
	true -> 
	    Deact_fun(),
	    {noreply, Data#data{deact_fun = undefined}};
	false ->
	    {noreply, Data}
    end;
%% Handle tree selection

%% Selecting a demo is an event we handle here and not in a callback
%% function directly added to the navigation tree itself. There is no
%% real good reason for this, but selecting a demo feels much like a
%% essential function which will also call a function from the
%% controller and putting it here feels better. Generally, functions
%% that act upon the view, such as enabling a menu item etc, probably
%% should be included in callback functions directly included in
%% creating widgets while functions that will talk to the controller
%% should be part of the handle_event function of the wx_object.
handle_event(#wx{event = #wxTree{type = command_tree_sel_changed, item = Item}, obj = Tree_ctrl}, Data) ->
    Item_text = wxTreeCtrl:getItemText(Tree_ctrl, Item),
    case wxTreeCtrl:getChildrenCount(Tree_ctrl, Item) of
	%% Naive way to determine if the selected item refers to a
	%% demo or category: if it has siblings it is a catergory and
	%% if it has none, it is a demo.
	0 ->
	    Demo = wxTreeCtrl:getItemData(Tree_ctrl, Item),
	    wxd_ctrl:select_demo(Demo);
	_ ->
	    wxd_ctrl:select_cat(Item_text)
    end,
    {noreply, Data};
handle_event(Evt, Data) ->
    ?LOG_WARNING("Unhandled wx event ~p~n", [Evt]),
    {noreply, Data}.

%% handle_sync_event(Evt, Obj, Data) ->
%%     ok.

handle_call({load_doc, Doc}, _From, Data) ->
    ?LOG_WARNING("Doc is ~p", [Doc]),
    Reply = ok,
    {reply, Reply, Data};
handle_call(_Request, _From, Data) ->
    Reply = ok,
    {reply, Reply, Data}.

handle_cast(_Msg, Data) ->
    {noreply, Data}.

handle_info(Info, Data) ->
    ?LOG_WARNING("Unhandled info ~p~n", [Info]),
    {noreply, Data}.

terminate(_Reason, #data{frame = Frame, mgr = Mgr}) ->
    wxd_ctrl:stop_app(),
    wxAuiManager:disconnect(Mgr),
    %% Without the next line, we get a Segmentation fault and error
    %% "wxWidgets Assert failure: ../src/common/wincmn.cpp(478):
    %% "GetEventHandler() == this" in ~wxWindowBase() : any pushed
    %% event handlers must have been removed"
    wxAuiManager:unInit(Mgr),
    %% TODO: does it really matter if the frame and wx are destroyed
    %% or can we just leave that out at this point because the
    %% application will be stopped anyhow?
    wxFrame:destroy(Frame),
    wx:destroy().

code_change(_OldVsn, Data, _Extra) ->
    {ok, Data}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%-------------------------------------------------------------------
%% Menus
%%-------------------------------------------------------------------

create_menus(Frame) ->
    %% Create
    Menu_bar = wxMenuBar:new(),
    File = wxMenu:new(),
    wxMenu:append(File, ?wxID_EXIT, "&Quit"),
    wxMenuBar:append(Menu_bar, File, "&File"),
    Edit = wxMenu:new(),
    Undo = wxMenu:append(Edit, ?wxID_UNDO, "Undo"),
    Redo = wxMenu:append(Edit, ?wxID_REDO, "Redo"),
    wxMenu:appendSeparator(Edit),
    Cut = wxMenu:append(Edit, ?wxID_CUT, "Cut"),
    Copy = wxMenu:append(Edit, ?wxID_COPY, "Copy"),
    Paste = wxMenu:append(Edit, ?wxID_PASTE, "Paste"),
    Delete = wxMenu:append(Edit, ?wxID_DELETE, "Delete"),
    wxMenu:appendSeparator(Edit),
    Select_all = wxMenu:append(Edit, ?wxID_SELECTALL, "Select all"),
    wxMenuBar:append(Menu_bar, Edit, "&Edit"),
    Help = wxMenu:new(),
    wxMenu:append(Help, ?wxID_ABOUT, "&About"),
    wxMenuBar:append(Menu_bar, Help, "&Help"),

    %% Set options, appearance 
    wxMenuItem:enable(Undo, [{enable, false}]),
    wxMenuItem:enable(Redo, [{enable, false}]),
    wxMenuItem:enable(Cut, [{enable, false}]),
    wxMenuItem:enable(Copy, [{enable, false}]),
    wxMenuItem:enable(Paste, [{enable, false}]),
    wxMenuItem:enable(Delete, [{enable, false}]),
    wxMenuItem:enable(Select_all, [{enable, false}]),

    %% Materialize
    wxFrame:setMenuBar(Frame, Menu_bar).

%%-------------------------------------------------------------------
%% Status bar
%%-------------------------------------------------------------------

create_status_bar(Frame) ->
    Status_bar = wxStatusBar:new(Frame),
    wxStatusBar:setStatusText(Status_bar, "Welcome to the wxErlang demo"),
    wxFrame:setStatusBar(Frame, Status_bar).

%%-------------------------------------------------------------------
%% AUI
%%-------------------------------------------------------------------

create_aui(Frame) ->
    %% It might be necessary to put a panel within the frame first and
    %% attach the AUI manager to that instead of the frame. There are
    %% rumors that things don't work properly otherwise.
    Panel = wxPanel:new(Frame, [{style, ?wxTAB_TRAVERSAL bor ?wxCLIP_CHILDREN}]),
    Mgr = wxAuiManager:new([{managed_wnd, Panel}]),
    %% wxAuiManager:setFlags(Mgr, ?wxAUI_MGR_DEFAULT bor ?wxAUI_MGR_ALLOW_ACTIVE_PANE),
    wxAuiManager:setFlags(Mgr, ?wxAUI_MGR_DEFAULT),
    
    %% Create the panes which the manager will control and add them to
    %% the manager.
    {Nav, Nav_info, Focus_funs1} = nav(Panel, Frame),
    wxAuiManager:addPane(Mgr, Nav, Nav_info),

    {OCD, OCD_info, Focus_funs2} = ocd(Panel, Frame),
    wxAuiManager:addPane(Mgr, OCD, OCD_info),

    {Log, Log_info, Focus_funs3} = log(Panel, Frame),
    wxAuiManager:addPane(Mgr, Log, Log_info),

    %% NB: for the aui_pane_activated event to work, the
    %% ?wxAUI_MGR_ALLOW_ACTIVE_PANE flag must be set for the AUI
    %% manager.
    %% wxAuiManager:connect(Mgr, aui_pane_activated),

    %% Materialize...
    wxAuiManager:update(Mgr),
    {Mgr, Focus_funs1 ++ Focus_funs2 ++ Focus_funs3}. 


%%-------------------------------------------------------------------
%% Demo navigation pane
%%-------------------------------------------------------------------

nav(Parent, _Frame) ->
    %% Create content
    Nav = create_nav_tree(Parent),

    %% Now set properties which determine how the AUI manager will
    %% handle the pane, such as position, sizes and appearance.
    Info = wxAuiPaneInfo:new(),
    Info1 = wxAuiPaneInfo:left(Info),
    Info2 = wxAuiPaneInfo:layer(Info1, 1),
    Info3 = wxAuiPaneInfo:bestSize(Info2, {240, -1}),
    Info4 = wxAuiPaneInfo:floatingSize(Info3, {240, 700}),
    Info5 = wxAuiPaneInfo:minSize(Info4, {240, -1}),
    Info6 = wxAuiPaneInfo:closeButton(Info5, [{visible, false}]),
    Info7 = wxAuiPaneInfo:caption(Info6, "wxErlang demos"),
    Info8 = wxAuiPaneInfo:name(Info7, "nav"),
    
    {Nav, Info8, []}.

create_nav_tree(Parent) ->
    Panel = wxPanel:new(Parent, [{style, ?wxTAB_TRAVERSAL bor ?wxCLIP_CHILDREN}]),
    %% Add content to the pane
    Tree = wxTreeCtrl:new(Panel, [{style, ?wxTR_DEFAULT_STYLE bor ?wxTR_HAS_VARIABLE_ROW_HEIGHT}]),
    Imgs = wxImageList:new(16, 16),
    wxTreeCtrl:assignImageList(Tree, Imgs),
    [wxImageList:add(Imgs, wxd_util:make_bmp(X)) ||
	X <- [overview, recent, frame, dialog, moredialog, core, book, customcontrol,
	      morecontrols, layout, process, clipboard, images, miscellaneous]],
    Tree1 = recreate_tree(Tree),
    wxTreeCtrl:connect(Tree1, command_tree_sel_changed),

    Box = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(Box, Tree1, [{proportion, 1}, {flag, ?wxEXPAND}]),
    wxPanel:setSizer(Panel, Box),
    Panel.

recreate_tree(Tree) ->
    wxTreeCtrl:freeze(Tree),
    wxTreeCtrl:deleteAllItems(Tree),
    Root = wxTreeCtrl:addRoot(Tree, "wxErlang Overview"),
    wxTreeCtrl:setItemImage(Tree, Root, 0),
    Tree_font = wxTreeCtrl:getFont(Tree),
    Cat_font = wxTreeCtrl:getFont(Tree),
    case os:type() of
	{win32, _} ->
	    %% The native treectrl on MSW has a bug where it doesn't
	    %% draw all of the text for an item if the font is larger
	    %% than the default.  It seems to be clipping the item's
	    %% label as if it was the size of the same label in the
	    %% default font.
	    ok;
	_ ->
	    Point_size = wxFont:getPointSize(Tree_font),
	    wxFont:setPointSize(Tree_font, Point_size + 2)
    end,
    wxFont:setWeight(Tree_font, ?wxBOLD),
    wxFont:setWeight(Cat_font, ?wxBOLD),
    wxTreeCtrl:setItemFont(Tree, Root, Tree_font),
    [begin 
	 Cat = wxTreeCtrl:appendItem(Tree, Root, Category, [{image, Index}]),
	 wxTreeCtrl:setItemFont(Tree, Cat, Cat_font),
	 [wxTreeCtrl:appendItem(Tree, Cat, Text, [{image, Index}, {data, Demo}])
	  || {Text, Demo} <- Items]
     end || {Category, Index, Items} <- ?DEMOS_DATA],
    wxTreeCtrl:expand(Tree, Root),
    wxTreeCtrl:thaw(Tree),
    Tree.

%%-------------------------------------------------------------------
%% Overview, code and demo pane
%%-------------------------------------------------------------------

ocd(Pane, Frame) ->
    %% Create element and set attributes
    NB = wxNotebook:new(Pane, ?wxID_ANY, [{style, ?wxCLIP_CHILDREN}]),
    IL = wxImageList:new(16, 16),
    [wxImageList:add(IL, wxd_util:make_bmp(X)) || X <- [overview, code, demo]],
    wxNotebook:assignImageList(NB, IL),

    %% Add content
    {Ovr, Ovr_fun} = html_window(NB, Frame),
    wxHtmlWindow:setPage(Ovr, "wxErlang"),
    wxNotebook:addPage(NB, Ovr, "Overview", [{imageId, 0}]),

    {Code, Code_fun} = stc(NB, Frame),
    %% Code = wxHtmlWindow:new(NB), %[{size, {400, 400}}]),
    %% wxHtmlWindow:setPage(Code, "some real nasty code."),
    wxNotebook:addPage(NB, Code, "Code", [{imageId, 1}]),

    % Set properties
    Info = wxAuiPaneInfo:new(),
    Info1 = wxAuiPaneInfo:centrePane(Info),
    Info2 = wxAuiPaneInfo:name(Info1, "ocd"),
    
    {NB, Info2, [{Ovr, Ovr_fun}, {Code, Code_fun}]}.

%%-------------------------------------------------------------------
%% Log
%%-------------------------------------------------------------------

log(Parent, Frame) ->
    %% Add content
    {Log, Log_fun} = text_ctrl(Parent, Frame),
    Font = wxFont:new(),
    wxFont:setFamily(Font, ?wxFONTFAMILY_MODERN),% ?wxFONTFAMILY_TELETYPE),
    Text_attr = wxTextAttr:new(),
    wxTextAttr:setFont(Text_attr, Font),
    wxTextCtrl:setDefaultStyle(Log, Text_attr),
    wxTextCtrl:setEditable(Log, false),
    wxTextCtrl:appendText(Log, "Logging output"),

    % Set properties
    Info = wxAuiPaneInfo:new(),
    Info1 = wxAuiPaneInfo:bottom(Info),
    Info2 = wxAuiPaneInfo:bestSize(Info1, {-1, 150}),
    Info3 = wxAuiPaneInfo:minSize(Info2, {-1, 140}),
    Info4 = wxAuiPaneInfo:floatingSize(Info3, {700, 240}),
    Info5 = wxAuiPaneInfo:closeButton(Info4, [{visible, false}]),
    Info6 = wxAuiPaneInfo:caption(Info5, "Log"),
    Info7 = wxAuiPaneInfo:maximizeButton(Info6, [{visible, true}]),
    Info8 = wxAuiPaneInfo:name(Info7, "log"),
    
    {Log, Info8, [{Log, Log_fun}]}.

%%-------------------------------------------------------------------
%% HTML window, used for displaying documentation.
%%-------------------------------------------------------------------

html_window(Parent, Frame) ->
    %% Ovr = wxd_ovr:start_link(Parent, Menu_bar),
    %% Pid = wx_object:get_pid(Ovr),
    Win = wxHtmlWindow:new(Parent), %[{size, {400, 400}}]),
    Menu_bar = wxFrame:getMenuBar(Frame),
    Edit_menu = wxMenuBar:getMenu(Menu_bar, wxMenuBar:findMenu(Menu_bar, "Edit")),
    Edit_menu_items = wxMenu:getMenuItems(Edit_menu),

    %% Create the Win associated function. NB: The behavior of the
    %% edit menu is directly linked with the overview widget and
    %% nothing else. If the overview is active, the behavior of the
    %% edit menu depends on text being selected or not in the
    %% overview. It is for this reason, the direct link between the
    %% behavior of the menu items and the state of the overview
    %% widget, that we use a callback here which is defined at this
    %% particular place in the function which creates the overview.

    %% Callback functions
    CB_upd = fun(_Evt, Obj) ->
		     Bool = case wxHtmlWindow:selectionToText(Win) of
				Text when Text =/= [] ->
				    true;
				_ ->
				    false
			    end,
		     wxUpdateUIEvent:enable(Obj, Bool)
	     end,
    CB_copy = fun(#wx{id = ?wxID_COPY} = _Evt, _Obj) ->
		      Text = wxHtmlWindow:selectionToText(Win),
		      io:format("copy ~p", [Text])
	      end,
    
    %% activate / deactivate function
    Fun = fun() ->
		  State = [{Item, wxMenuItem:isEnabled(Item)} || Item <- Edit_menu_items],
		  [wxMenuItem:enable(Item, [{enable, false}]) || Item <- Edit_menu_items],
		  wxFrame:connect(Frame, update_ui, [{id, ?wxID_COPY}, {callback, CB_upd}]),   
		  wxMenu:connect(Edit_menu, command_menu_selected, [{callback, CB_copy}]),
		  fun() ->
			  [wxMenuItem:enable(Item, [{enable, Bool}]) || {Item, Bool} <- State],
			  wxFrame:disconnect(Frame, update_ui, [{id, ?wxID_COPY}]),
			  wxMenu:disconnect(Edit_menu, command_menu_selected)
		  end
	  end,

    wxHtmlWindow:connect(Win, set_focus),
    wxHtmlWindow:connect(Win, kill_focus),

    {Win, Fun}.

%%-------------------------------------------------------------------
%% Text control, used for logging output and other textual output.
%%-------------------------------------------------------------------

text_ctrl(Parent, Frame) ->
    Style = ?wxTE_MULTILINE bor ?wxHSCROLL bor ?wxTE_RICH2 bor ?wxTE_NOHIDESEL,
    Text_ctrl = wxTextCtrl:new(Parent, ?wxID_ANY, [{style, Style}]),%?wxTE_MULTILINE}]),
    Menu_bar = wxFrame:getMenuBar(Frame),
    Edit_menu = wxMenuBar:getMenu(Menu_bar, wxMenuBar:findMenu(Menu_bar, "Edit")),
    Edit_menu_items = wxMenu:getMenuItems(Edit_menu),

    %% Callback functions
    
    %% NB: Should we also check for wxTextCtrl:isEditable(Text_ctrl)
    %% or is this already part of the canCut, canPaste
    %% etc. funactions?
    CB_upd = fun(#wx{id = ?wxID_UNDO}, Obj) ->
    		     Bool = wxTextCtrl:canUndo(Text_ctrl),
    		     wxUpdateUIEvent:enable(Obj, Bool);
    		(#wx{id = ?wxID_REDO}, Obj) ->
    		     Bool = wxTextCtrl:canRedo(Text_ctrl),
    		     wxUpdateUIEvent:enable(Obj, Bool);
    		(#wx{id = ?wxID_CUT}, Obj) ->
    		     Bool = wxTextCtrl:canCut(Text_ctrl),
    		     wxUpdateUIEvent:enable(Obj, Bool);
    		(#wx{id = ?wxID_COPY}, Obj) ->
    		     Bool = wxTextCtrl:canCopy(Text_ctrl),
    		     wxUpdateUIEvent:enable(Obj, Bool);
    		(#wx{id = ?wxID_PASTE}, Obj) ->
    		     %% the standard wxTextCtrl canPaste function
    		     %% needs some extra functionality to work
    		     %% properly
    		     Bool = can_paste(Text_ctrl, ?wxDF_TEXT),
    		     wxUpdateUIEvent:enable(Obj, Bool);
    		(#wx{id = ?wxID_DELETE}, Obj) ->
    		     %% Delete is not cut without the clipboard stuff
    		     Bool = case wxTextCtrl:getSelection(Text_ctrl) of
    				{X, X} -> false;
    				_ -> true
    			    end,
    		     wxUpdateUIEvent:enable(Obj, Bool);
    		(#wx{id = ?wxID_SELECTALL}, Obj) ->
    		     Bool = ([] =/= wxTextCtrl:getValue(Text_ctrl)),
    		     wxUpdateUIEvent:enable(Obj, Bool)
    	     end,
    CB_edit = fun(#wx{id = ?wxID_UNDO}, _Obj) ->
    		      wxTextCtrl:undo(Text_ctrl);
    		 (#wx{id = ?wxID_REDO}, _Obj) ->
    		      wxTextCtrl:redo(Text_ctrl);
    		 (#wx{id = ?wxID_CUT}, _Obj) ->
    		      wxTextCtrl:cut(Text_ctrl);
    		 (#wx{id = ?wxID_COPY}, _Obj) ->
    		      wxTextCtrl:copy(Text_ctrl);
    		 (#wx{id = ?wxID_PASTE}, _Obj) ->
    		      wxTextCtrl:paste(Text_ctrl);
    		 (#wx{id = ?wxID_DELETE}, _Obj) ->
    		      {From, To} = wxTextCtrl:getSelection(Text_ctrl),
    		      wxTextCtrl:remove(Text_ctrl, From, To);
    		 (#wx{id = ?wxID_SELECTALL}, _Obj) ->
    		      From = 0,
    		      To = wxTextCtrl:getLastPosition(Text_ctrl),
    		      wxTextCtrl:setSelection(Text_ctrl, From, To)
    	     end,
    
    %% activate / deactivate function
    Fun = fun() ->
    		  State = [{Item, wxMenuItem:isEnabled(Item)} || Item <- Edit_menu_items],
    		  [wxMenuItem:enable(Item, [{enable, false}]) || Item <- Edit_menu_items],
		  Ids = case wxTextCtrl:isEditable(Text_ctrl) of
			    true -> [?wxID_UNDO, ?wxID_REDO,
				     ?wxID_CUT, ?wxID_COPY, ?wxID_PASTE,
				     ?wxID_DELETE, ?wxID_SELECTALL];
			    false -> [?wxID_COPY, ?wxID_SELECTALL]
			end,
    		  [wxFrame:connect(Frame, update_ui, [{id, Id}, {callback, CB_upd}]) ||
    		      Id <- Ids],
    		  wxMenu:connect(Edit_menu, command_menu_selected, [{callback, CB_edit}]),
    		  fun() ->
    			  [wxMenuItem:enable(Item, [{enable, Bool}]) || {Item, Bool} <- State],
    			  [wxFrame:disconnect(Frame, update_ui, [{id, Id}]) || Id <- Ids],
    			  wxMenu:disconnect(Edit_menu, command_menu_selected)
    		  end
    	  end,

    wxTextCtrl:connect(Text_ctrl, set_focus),
    wxTextCtrl:connect(Text_ctrl, kill_focus),

    {Text_ctrl, Fun}.

%%-------------------------------------------------------------------
%% Styled text control, used for viewing and editing source code.
%%-------------------------------------------------------------------

stc(Parent, Frame) ->
    Stc = wxStyledTextCtrl:new(Parent, [{style, ?wxBORDER_NONE}]),

    %% Erlang Lexer
    wxStyledTextCtrl:setLexer(Stc, ?wxSTC_LEX_ERLANG),
    wxStyledTextCtrl:setKeyWords(Stc, 0, string:join(?RESERVED_WORDS, ",")),

    %% %% Enable folding
    %% wxStyledTextCtrl:setProperty(Stc, "fold", "1"), 

    %% %% Highlight tab/space mixing (shouldn't be any)
    %% wxStyledTextCtrl:setProperty(Stc, "tab.timmy.whinge.level", "1"),

    %% Font
    Font = wxFont:new(),
    wxFont:setFamily(Font, ?wxFONTFAMILY_MODERN),% ?wxFONTFAMILY_TELETYPE),
    wxStyledTextCtrl:styleSetFont(Stc, ?wxSTC_STYLE_DEFAULT, Font),

    %% Margins
    wxStyledTextCtrl:setMargins(Stc, 2, 2),
    %% Set up the numbers in the margin for margin #1
    wxStyledTextCtrl:setMarginType(Stc, 1, ?wxSTC_MARGIN_NUMBER),
    LW = wxStyledTextCtrl:textWidth(Stc, ?wxSTC_STYLE_LINENUMBER, "9999"),
    wxStyledTextCtrl:setMarginWidth(Stc, 1, LW),
    %% %% Setup a margin to hold fold markers (margin #2)
    %% wxStyledTextCtrl:setMarginType(Stc, 2, ?wxSTC_MARGIN_SYMBOL),
    %% wxStyledTextCtrl:setMarginMask(Stc, 2, ?wxSTC_MASK_FOLDERS),
    %% wxStyledTextCtrl:setMarginSensitive(Stc, 2, true),
    %% wxStyledTextCtrl:setMarginWidth(Stc, 2, 12),

    %% Indentation and tab stuff
    wxStyledTextCtrl:setIndent(Stc, 4), % Preferred indentation for Erlang
    wxStyledTextCtrl:setIndentationGuides(Stc, true), % Show indent guides
    wxStyledTextCtrl:setBackSpaceUnIndents(Stc, true), % Backspace unindents rather than delete 1 space
    wxStyledTextCtrl:setTabIndents(Stc, true), % Tab key indents
    wxStyledTextCtrl:setTabWidth(Stc, 4), % Proscribed tab size for wx
    wxStyledTextCtrl:setUseTabs(Stc, false), % Use spaces rather than tabs, or

    %% %% White space
    %% wxStyledTextCtrl:setViewWhiteSpace(Stc, ?wxSTC_WS_INVISIBLE), % Don't view white space

    %% EOL: Since we are loading/saving ourselves, and the strings
    %% will always have \n's in them, set the STC to edit them that
    %% way.
    wxStyledTextCtrl:setEOLMode(Stc, ?wxSTC_EOL_LF),
    wxStyledTextCtrl:setViewEOL(Stc, false),

    %% %% No right-edge mode indicator
    %% wxStyledTextCtrl:setEdgeMode(Stc, ?wxSTC_EDGE_NONE),

    %% %% and now set up the fold markers
    %% wxStyledTextCtrl:markerDefine(Stc, ?wxSTC_MARKNUM_FOLDEREND, ?wxSTC_MARK_BOXPLUSCONNECTED, [{foreground, ?wxWHITE}, {background, ?wxBLACK}]),
    %% wxStyledTextCtrl:markerDefine(Stc, ?wxSTC_MARKNUM_FOLDEROPENMID, ?wxSTC_MARK_BOXMINUSCONNECTED,[{foreground, ?wxWHITE}, {background, ?wxBLACK}]),
    %% wxStyledTextCtrl:markerDefine(Stc, ?wxSTC_MARKNUM_FOLDERMIDTAIL, ?wxSTC_MARK_TCORNER, [{foreground, ?wxWHITE}, {background, ?wxBLACK}]),
    %% wxStyledTextCtrl:markerDefine(Stc, ?wxSTC_MARKNUM_FOLDERTAIL, ?wxSTC_MARK_LCORNER, [{foreground, ?wxWHITE}, {background, ?wxBLACK}]),
    %% wxStyledTextCtrl:markerDefine(Stc, ?wxSTC_MARKNUM_FOLDERSUB, ?wxSTC_MARK_VLINE, [{foreground, ?wxWHITE}, {background, ?wxBLACK}]),
    %% wxStyledTextCtrl:markerDefine(Stc, ?wxSTC_MARKNUM_FOLDER, ?wxSTC_MARK_BOXPLUS, [{foreground, ?wxWHITE}, {background, ?wxBLACK}]),
    %% wxStyledTextCtrl:markerDefine(Stc, ?wxSTC_MARKNUM_FOLDEROPEN, ?wxSTC_MARK_BOXMINUS, [{foreground, ?wxWHITE}, {background, ?wxBLACK}]),
    
    wxStyledTextCtrl:setSelectionMode(Stc, ?wxSTC_SEL_LINES),

    %% Line numbers in margin
    wxStyledTextCtrl:styleSetSpec(Stc, ?wxSTC_STYLE_LINENUMBER, "fore:#000000,back:#99A9C2"),    
    %% Highlighted brace
    wxStyledTextCtrl:styleSetSpec(Stc, ?wxSTC_STYLE_BRACELIGHT, "fore:#00009D,back:#FFFF00"),
    %% Unmatched brace
    wxStyledTextCtrl:styleSetSpec(Stc, ?wxSTC_STYLE_BRACEBAD, "fore:#00009D,back:#FF0000"),
    %% Indentation guide
    wxStyledTextCtrl:styleSetSpec(Stc, ?wxSTC_STYLE_INDENTGUIDE, "fore:#CDCDCD"),
    SetStyle = fun({Style, Color}) ->
		       wxStyledTextCtrl:styleSetFont(Stc, Style, Font),
		       wxStyledTextCtrl:styleSetForeground(Stc, Style, Color)
	       end,
    [SetStyle(Style) || Style <- ?ERLANG_STYLES],

    %% Caret color
    wxStyledTextCtrl:setCaretForeground(Stc, ?wxBLUE),
    %% Selection background
    %% wxStyledTextCtrl:setSelBackground(1, '#66CCFF')

    %% Scrolling
    Policy = ?wxSTC_CARET_SLOP bor ?wxSTC_CARET_JUMPS bor ?wxSTC_CARET_EVEN, 
    wxStyledTextCtrl:setYCaretPolicy(Stc, Policy, 3),
    wxStyledTextCtrl:setVisiblePolicy(Stc, Policy, 3),

    wxStyledTextCtrl:setText(Stc, "load_code(Ed, {ok, Code}) ->\n"
			     "    ?stc:setReadOnly(Ed, false),\n"
			     "    ?stc:setTextRaw(Ed, <<Code/binary, 0:8>>),\n"
			     "    Lines = ?stc:getLineCount(Ed),\n"
			     "    Sz = trunc(math:log10(Lines))+1,\n"
			     "    LW = ?stc:textWidth(Ed, ?wxSTC_STYLE_LINENUMBER, lists:duplicate(Sz, $9)),\n"
			     "    %%io:format(\"~p ~p ~p~n\", [Lines, Sz, LW]),\n"
			     "    ?stc:setMarginWidth(Ed, 0, LW+5),\n"
			     "    ?stc:setReadOnly(Ed, true),\n"
			     "    Ed."),
    
    wxStyledTextCtrl:toggleFold(Stc, 1),

    Fun = undefined,
    
    {Stc, Fun}.

%%-------------------------------------------------------------------
%% Splash
%%-------------------------------------------------------------------

splash(Frame) ->
    Bmp = wxd_util:make_bmp("erlang.png"),
    Splash = wxSplashScreen:new(Bmp, ?wxSPLASH_CENTRE_ON_SCREEN bor ?wxSPLASH_TIMEOUT,
				2000, wx:null(), ?wxID_ANY),
    CB = fun(_Evt, _Obj) ->
    		 wxFrame:raise(Frame)
    	 end,
    wxSplashScreen:connect(Splash, destroy, [{callback, CB}]).

%%-------------------------------------------------------------------
%% Taskbar, i.e. the icon in the docking area or taskbar
%%-------------------------------------------------------------------

%% A taskbar icon is not a wxWindow subclass and therefor cannot live
%% independently as, for example, the splash screen. 
create_task_bar(Frame) ->
    Task_bar_icon = wxTaskBarIcon:new(),
    Icon = wxd_util:make_icon("erlang-logo64.png"),
    wxTaskBarIcon:setIcon(Task_bar_icon, Icon, [{tooltip, "wxErlang demo"}]),
    CB = fun(_Evt, _Obj) ->
		wxFrame:raise(Frame)
	 end,
    wxTaskBarIcon:connect(Task_bar_icon, taskbar_left_dclick, [{callback, CB}]),
    Task_bar_icon.

%%-------------------------------------------------------------------
%% Some clipboard related stuff
%% TODO: What about primary clipboard stuff etc. 
%%-------------------------------------------------------------------

%% The standard canPaste function doesn't work properly on at least
%% GNOME. It will return true on some occasions when there is
%% something fresh on the clipboard but it will return false most of
%% the times. To make it work properly, do open the clipboard and see
%% if there is something there that can be pasted.
can_paste(Object, Format) ->
    wxTextCtrl:canPaste(Object) orelse
	begin 
	    Clipboard = wxClipboard:get(),
	    case wxClipboard:open(Clipboard) of
		true ->
		    wxClipboard:usePrimarySelection(Clipboard, [{primary, false}]),
		    case wxClipboard:isSupported(Clipboard, Format) of
			true ->
			    Bool = can_paste_clipboard(Clipboard, Format);
			false ->
			    ?LOG_ERROR("Format ~p not supported for clipboard.", [Format]),
			    Bool = false
		    end,
		    wxClipboard:close(Clipboard),
		    Bool;
		false ->
		    ?LOG_ERROR("Could not open clipboard."),
		    false
	    end
	end.

%% Get content from the clipboard
can_paste_clipboard(Clipboard, ?wxDF_TEXT) ->
    Data_object = wxTextDataObject:new(),
    Bool = case wxClipboard:getData(Clipboard, Data_object) of
	       true ->
		   0 =/= wxTextDataObject:getTextLength(Data_object);
	       false ->
		   false
	   end,
    %% MUST be destroyed to free the resource
    wxTextDataObject:destroy(Data_object),
    Bool;
can_paste_clipboard(_Clipboard, Format) ->
    ?LOG_ERROR("get_clipboard for type ~p not implemented.", [Format]),
    false.

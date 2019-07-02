%%%-------------------------------------------------------------------
%%% @doc
%%% The demo's navigation pane
%%% @end
%%%-------------------------------------------------------------------
-module(wxd_nav).

-behaviour(wx_object).

-include_lib("wx/include/wx.hrl").
-include("wxd.hrl").

%% API
-export([start_link/2]).

%% wx_object callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 handle_sync_event/3, handle_event/2, terminate/2, code_change/3]).

-record(state, {pane, menu_bar}).

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
    Demos = demos(),

    P = wxPanel:new(Parent, [{style, ?wxTAB_TRAVERSAL bor ?wxCLIP_CHILDREN}]),

    %% Add content to the pane
    Tree = wxTreeCtrl:new(P, [{style, ?wxTR_DEFAULT_STYLE bor ?wxTR_HAS_VARIABLE_ROW_HEIGHT}]),
    Imgs = wxImageList:new(16, 16),
    wxTreeCtrl:assignImageList(Tree, Imgs),
    [wxImageList:add(Imgs, wxd_util:make_bmp(X)) ||
	X <- [overview, recent, frame, dialog, moredialog, core, book, customcontrol,
	      morecontrols, layout, process, clipboard, images, miscellaneous]],
    Tree1 = recreate_tree(Tree),
    wxTreeCtrl:connect(Tree1, command_tree_sel_changed, [{skip, false}]),

    Box = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(Box, Tree1, [{proportion, 1}, {flag, ?wxEXPAND}]),
    wxPanel:setSizer(P, Box),

    %% Add content to the menus: we both insert a menu item in an
    %% existing menu and add a new menu to menu bar.

    %% Add an item to an existing menu
    File_idx = wxMenuBar:findMenu(Menu_bar, "File"),
    File_menu = wxMenuBar:getMenu(Menu_bar, File_idx),
    %% Two ways to handle 'added' menu items: either use a callback
    %% function or leave it to wx_object's handle_sync_event to catch
    %% the event. Events not handled should be skipped to be processed
    %% by the next in line. Using a callback may be easier when
    %% currying the callback function with dunamicly generated ID's.

    %% Using callback function
    %% New_id = ?wxID_OPEN,
    %% wxMenu:insert(File, 0, New_id, [{text, "Open..."}]),
    %% CB_open = fun(#wx{id = Id}, _Obj) when Id =:= New_id ->
    %% 		      io:format("+++ Nav CB_open a demo");
    %% 		 (_evt, Obj) ->
    %% 		      %% Pass on this event to the next one up onn the ladder
    %% 		      wxEvent:skip(Obj)
    %% 	      end,
    %% wxMenu:connect(File, command_menu_selected, [{callback, CB_open}]),

    %% Using handle_sync_event
    wxMenu:insert(File_menu, 0, ?wxID_OPEN, [{text, "&Open..."}]),
    wxMenu:connect(File_menu, command_menu_selected, [callback]),

    %% Add a brand new menu to the menu bar
    Demo_menu = wxMenu:new(),
    wxMenu:append(Demo_menu, ?wxID_REVERT, "&Revert"),
    %% Insert menu just before the Help menu: i.e. insert it where the
    %% Help menu now is and move the Help menu one place to the right.
    Help_idx = wxMenuBar:findMenu(Menu_bar, "Help"),
    wxMenuBar:insert(Menu_bar, Help_idx, Demo_menu, "&Demos"),
    wxMenu:connect(Demo_menu, command_menu_selected),
    
    {P, #state{pane = P, menu_bar = Menu_bar}}.

handle_sync_event(#wx{id=?wxID_OPEN, event = #wxCommand{}}, _Event, _State) ->
    io:format("+++ Nav Sync_open a demo"),
    ok;
handle_sync_event(#wx{event = #wxCommand{}}, Event, _State) ->
    wxEvent:skip(Event, [{skip, true}]),
    ok.

handle_event(#wx{} = Evt, State) ->
    io:format("+++ Nav Unhandled wx event ~p~n", [Evt]),
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

demos() ->
    [#demo{label = Category, image_file = Image_file, items = Items
	   %% tree_id = Tree_id, menu_id = Menu_id
	  }
     || {Category, Image_file, Items} <- ?DEMOS_DATA].


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
	 [wxTreeCtrl:appendItem(Tree, Cat, Item, [{image, Index}]) || Item <- Items]
     end || {Category, Index, Items} <- ?DEMOS_DATA],
    wxTreeCtrl:expand(Tree, Root),
    wxTreeCtrl:thaw(Tree),
    Tree.


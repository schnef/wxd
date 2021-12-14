%%%-------------------------------------------------------------------
%%% @doc
%%% The demo's code view pane
%%% @end
%%%-------------------------------------------------------------------
-module(wxd_code).

-behaviour(wx_object).

-include_lib("wx/include/wx.hrl").

%% API
-export([start_link/2]).

%% wx_object callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 handle_event/2, terminate/2, code_change/3]).

-record(state, {update_fun, selection}).

%% For wx-2.9 usage
%% -ifndef(wxSTC_ERLANG_COMMENT_FUNCTION).
%% -define(wxSTC_ERLANG_COMMENT_FUNCTION, 14).
%% -define(wxSTC_ERLANG_COMMENT_MODULE, 15).
%% -define(wxSTC_ERLANG_COMMENT_DOC, 16).
%% -define(wxSTC_ERLANG_COMMENT_DOC_MACRO, 17).
%% -define(wxSTC_ERLANG_ATOM_QUOTED, 18).
%% -define(wxSTC_ERLANG_MACRO_QUOTED, 19).
%% -define(wxSTC_ERLANG_RECORD_QUOTED, 20).
%% -define(wxSTC_ERLANG_NODE_NAME_QUOTED, 21).
%% -define(wxSTC_ERLANG_BIFS, 22).
%% -define(wxSTC_ERLANG_MODULES, 23).
%% -define(wxSTC_ERLANG_MODULES_ATT, 24).
%% -endif.

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(wxWindow:wxWindow(), wxMenuBar:wxMenuBar()) -> wxWindow:wxWindow().
start_link(Parent, Menu_bar) ->
    wx_object:start_link(?MODULE, [Parent, Menu_bar], []).

%%%===================================================================
%%% wx_object callbacks
%%%===================================================================

-define(stc, wxStyledTextCtrl).

init([Parent, Menu_bar]) ->
    FixedFont = wxFont:new(10, ?wxFONTFAMILY_TELETYPE, ?wxNORMAL, ?wxNORMAL,[]),
    Ed = ?stc:new(Parent),

    ?stc:styleClearAll(Ed),
    ?stc:styleSetFont(Ed, ?wxSTC_STYLE_DEFAULT, FixedFont),
    ?stc:setLexer(Ed, ?wxSTC_LEX_ERLANG),
    ?stc:setMarginType(Ed, 0, ?wxSTC_MARGIN_NUMBER),
    LW = ?stc:textWidth(Ed, ?wxSTC_STYLE_LINENUMBER, "9"),
    ?stc:setMarginWidth(Ed, 0, LW),
    ?stc:setMarginWidth(Ed, 1, 0),

    ?stc:setSelectionMode(Ed, ?wxSTC_SEL_LINES),
    %%?stc:hideSelection(Ed, true),

    Styles =  [{?wxSTC_ERLANG_DEFAULT,  {0,0,0}},
	       {?wxSTC_ERLANG_COMMENT,  {160,53,35}},
	       {?wxSTC_ERLANG_VARIABLE, {150,100,40}},
	       {?wxSTC_ERLANG_NUMBER,   {5,5,100}},
	       {?wxSTC_ERLANG_KEYWORD,  {130,40,172}},
	       {?wxSTC_ERLANG_STRING,   {170,45,132}},
	       {?wxSTC_ERLANG_OPERATOR, {30,0,0}},
	       {?wxSTC_ERLANG_ATOM,     {0,0,0}},
	       {?wxSTC_ERLANG_FUNCTION_NAME, {64,102,244}},
	       {?wxSTC_ERLANG_CHARACTER,{236,155,172}},
	       {?wxSTC_ERLANG_MACRO,    {40,144,170}},
	       {?wxSTC_ERLANG_RECORD,   {40,100,20}},
	       {?wxSTC_ERLANG_NODE_NAME,{0,0,0}},
	       %% Optional 2.9 stuff
	       {?wxSTC_ERLANG_COMMENT_FUNCTION, {160,53,35}},
	       {?wxSTC_ERLANG_COMMENT_MODULE, {160,53,35}},
	       {?wxSTC_ERLANG_COMMENT_DOC, {160,53,35}},
	       {?wxSTC_ERLANG_COMMENT_DOC_MACRO, {160,53,35}},
	       {?wxSTC_ERLANG_ATOM_QUOTED, {0,0,0}},
	       {?wxSTC_ERLANG_MACRO_QUOTED, {40,144,170}},
	       {?wxSTC_ERLANG_RECORD_QUOTED, {40,100,20}},
	       {?wxSTC_ERLANG_NODE_NAME_QUOTED, {0,0,0}},
	       {?wxSTC_ERLANG_BIFS, {130,40,172}},
	       {?wxSTC_ERLANG_MODULES, {64,102,244}},
	       {?wxSTC_ERLANG_MODULES_ATT, {64,102,244}}
	      ],
    SetStyle = fun({Style, Color}) ->
		       ?stc:styleSetFont(Ed, Style, FixedFont),
		       ?stc:styleSetForeground(Ed, Style, Color)
	       end,
    [SetStyle(Style) || Style <- Styles],
    ?stc:setKeyWords(Ed, 0, keyWords()),
    
    %% Scrolling
    Policy = ?wxSTC_CARET_SLOP bor ?wxSTC_CARET_JUMPS bor ?wxSTC_CARET_EVEN, 
    ?stc:setYCaretPolicy(Ed, Policy, 3),
    ?stc:setVisiblePolicy(Ed, Policy, 3),

    %% ?stc:connect(Ed, stc_doubleclick),
    %% ?stc:connect(Ed, std_do_drop, fun(Ev, Obj) -> io:format("Ev ~p ~p~n",[Ev,Obj]) end),
    ?stc:addText(Ed, "Some demo code."),
    %% ?stc:setReadOnly(Ed, true),

    Fun = update_fun(Ed, Menu_bar),

    {Ed, #state{update_fun = Fun}}.

handle_event(#wx{id = Id, event = #wxUpdateUI{}, obj = Obj}, #state{update_fun = Fun, selection = Selection} = State) ->
    State1 = case ?stc:getSelectedText(Obj) of
		 Text when Text =/= [] ->
		     State#state{selection = true};
		 _ ->
		     State#state{selection = false}
	     end,
    case State1#state.selection =:= Selection of
	false ->
	    Fun(update, State1),
	    io:format("CODE Selection changed to ~p [Id ~p]~n", [State1#state.selection, Id]);
	true ->
	    ok
    end,
    {noreply, State1};
handle_event(#wx{} = Evt, State) ->
    io:format("Unhandled wx event ~p~n", [Evt]),
    {noreply, State}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Type, #state{update_fun = Fun} = State)
  when Type =:= activate orelse Type =:= deactivate ->
    Fun(Type, State),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

keyWords() ->
    L = ["after","begin","case","try","cond","catch","andalso","orelse",
	 "end","fun","if","let","of","receive","when","bnot","not",
	 "div","rem","band","and","bor","bxor","bsl","bsr","or","xor"],
    lists:flatten([K ++ " " || K <- L] ++ [0]).

update_fun(Ed, Menu_bar) ->
    Edit_menu = wxMenuBar:getMenu(Menu_bar, wxMenuBar:findMenu(Menu_bar, "Edit")),
    Cut = wxMenu:findItem(Edit_menu, ?wxID_CUT),
    Copy = wxMenu:findItem(Edit_menu, ?wxID_COPY),
    Paste = wxMenu:findItem(Edit_menu, ?wxID_PASTE),
    fun F(activate, State) ->
	    wxMenuItem:enable(Cut, [{enable, false}]),
	    wxMenuItem:enable(Copy, [{enable, false}]),
	    wxMenuItem:enable(Paste, [{enable, false}]),
	    wxMenu:connect(Edit_menu, command_menu_selected, [{skip, true}]),
	    ?stc:connect(Ed, update_ui),
	    F(update, State);
	F(update, #state{selection = Flag} = _State) when is_boolean(Flag) ->
	    wxMenuItem:enable(Cut, [{enable, Flag}]),
	    wxMenuItem:enable(Copy, [{enable, Flag}]);
	F(update, _) ->
	    ok;
	F(deactivate, _State) ->
	    wxMenuItem:enable(Copy, [{enable, false}]),
	    wxMenu:disconnect(Edit_menu, command_menu_selected),
	    ?stc:disconnect(Ed, update_ui)
    end.

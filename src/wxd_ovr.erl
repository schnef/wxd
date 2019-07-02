%%%-------------------------------------------------------------------
%%% @doc
%%% The demo's main frame
%%% @end
%%%-------------------------------------------------------------------
-module(wxd_ovr).

-behaviour(wx_object).

-include_lib("wx/include/wx.hrl").

%% API
-export([start_link/2]).

%% wx_object callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 handle_event/2, terminate/2, code_change/3]).

-record(state, {update_fun, selection}).

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
    Ovr = wxHtmlWindow:new(Parent), %[{size, {400, 400}}]),
    wxHtmlWindow:setPage(Ovr, "wxErlang"),
    Fun = update_fun(Ovr, Menu_bar),
    {Ovr, #state{update_fun = Fun}}.

handle_event(#wx{id = Id, event = #wxUpdateUI{}, obj = Obj}, #state{update_fun = Fun, selection = Selection} = State) ->
    State1 = case wxHtmlWindow:selectionToText(Obj) of
		 Text when Text =/= [] ->
		     State#state{selection = true};
		 _ ->
		     State#state{selection = false}
	     end,
    case State1#state.selection =:= Selection of
	false ->
	    Fun(update, State1),
	    io:format("OVR Selection changed to ~p [Id ~p]~n", [State1#state.selection, Id]);
	true ->
	    ok
    end,
    {noreply, State1};
handle_event(#wx{} = Evt, State) ->
    io:format("OVR Unhandled wx event ~p~n", [Evt]),
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

update_fun(Ovr, Menu_bar) ->
    Edit_menu = wxMenuBar:getMenu(Menu_bar, wxMenuBar:findMenu(Menu_bar, "Edit")),
    Cut = wxMenu:findItem(Edit_menu, ?wxID_CUT),
    Copy = wxMenu:findItem(Edit_menu, ?wxID_COPY),
    Paste = wxMenu:findItem(Edit_menu, ?wxID_PASTE),
    fun F(activate, State) ->
	    wxMenuItem:enable(Cut, [{enable, false}]),
	    wxMenuItem:enable(Copy, [{enable, false}]),
	    wxMenuItem:enable(Paste, [{enable, false}]),
	    wxMenu:connect(Edit_menu, command_menu_selected, [{skip, true}]),
	    wxHtmlWindow:connect(Ovr, update_ui),
	    F(update, State);
	F(update, #state{selection = Flag} = _State) when is_boolean(Flag) ->
	    wxMenuItem:enable(Copy, [{enable, Flag}]);
	F(update, _) ->
	    ok;
	F(deactivate, _State) ->
	    wxMenuItem:enable(Copy, [{enable, false}]),
	    wxMenu:disconnect(Edit_menu, command_menu_selected),
	    wxHtmlWindow:disconnect(Ovr, update_ui)
    end.

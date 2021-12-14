%%%-------------------------------------------------------------------
%%% @doc
%%% Image related utility functions.
%%% @end
%%%-------------------------------------------------------------------

-module(wxd_util).

-export([img_file/1, make_icon/1, make_bmp/1]).

img_dir() ->
    Dir = filename:join([code:priv_dir(wxd), "img"]),
    case filelib:ensure_dir(Dir) of
	ok ->
	    {ok, Dir};
	Error ->
	    Error
    end.

img_file(Name) ->
    {ok, Img_dir} = img_dir(),
    filename:join(Img_dir, Name).
    
img(Name) when is_atom(Name) -> 
    img_file(atom_to_list(Name) ++ ".png");
img(File_name) ->
    img_file(File_name).
    
-spec make_icon(Name :: atom() | file:filename_all()) -> wxIcon:wxIcon().
make_icon(Name) ->
    wxIcon:new(img(Name)).

-spec make_bmp(Name :: atom() | file:filename_all()) -> wxBitmap:wxBitmap().
make_bmp(Name) ->
    wxBitmap:new(img(Name), []).

%%% -*- mode: erlang -*-
-ifndef(wxd_hrl).
-define(wxd_hrl, true).

-record(demo, {label :: string(),
	       image_file = undefined :: file:filename_all() | undefined,
	       items = [] :: [#demo{}],
	       tree_id = undefined :: integer() | undefined,
	       menu_id = undefined :: integer() | undefined}).

-define(DEMOS_DATA,
	[
	 %% {Category, Image_index, [Item]}
	 %% new stuff
	 {"Recent Additions/Updates", 1,
	  [{"PropertyGrid", "propertygrid.erl"},
	   {"SystemSettings", "systemsettings.erl"},
	   {"GridLabelRenderer", "gridlabelrenderer.erl"},
	   {"InfoBar", "infobar.erl"},
	   {"WrapSizer", "wrapsizer.erl"},
	   {"UIActionSimulator", "uiactionsimulator.erl"},
	   {"GraphicsGradient", "graphicsgradient.erl"},
	   {"PDFViewer", "pdfviewer.erl"},
	   {"ItemsPicker", "itemspicker.erl"},
	   {"CommandLinkButton", "commandlinkbutton.erl"},
	   {"DVC_DataViewModel", "dvc_dataviewmodel.erl"},
	   {"DVC_IndexListModel", "dvc_indexlistmodel.erl"},
	   {"DVC_ListCtrl", "dvc_listctrl.erl"},
	   {"DVC_TreeCtrl", "dvc_treectrl.erl"},
	   {"DVC_CustomRenderer", "dvc_customrenderer.erl"},
	   {"PenAndBrushStyles", "penandbrushstyles.erl"},
	   {"HTML2_WebView", "html2_webview.erl"}
	  ]},

	 %% managed windows == things with a (optional) caption you can close
	 {"Frames and Dialogs", 2,
	  [{"AUI_DockingWindowMgr", "aui_dockingwindowmgr.erl"},
	   {"AUI_MDI", "aui_mdi.erl"},
	   {"Dialog", "dialog.erl"},
	   {"Frame", "frame.erl"},
	   {"MDIWindows", "mdiwindows.erl"},
	   {"MiniFrame", "miniframe.erl"},
	   {"Wizard", "wizard.erl"}
	  ]},

	 %% the common dialogs
	 {"Common Dialogs", 3, 
	  [{"AboutBox", "aboutbox.erl"},
	   {"ColourDialog", "colourdialog.erl"},
	   {"DirDialog", "dirdialog.erl"},
	   {"FileDialog", "filedialog.erl"},
	   {"FindReplaceDialog", "findreplacedialog.erl"},
	   {"FontDialog", "fontdialog.erl"},
	   {"MessageDialog", "messagedialog.erl"},
	   {"MultiChoiceDialog", "multichoicedialog.erl"},
	   {"PageSetupDialog", "pagesetupdialog.erl"},
	   {"PrintDialog", "printdialog.erl"},
	   {"ProgressDialog", "progressdialog.erl"},
	   {"SingleChoiceDialog", "singlechoicedialog.erl"},
	   {"TextEntryDialog", "textentrydialog.erl"}
	  ]},

	 %% dialogs from libraries
	 {"More Dialogs", 4,
	  [{"ImageBrowser", "imagebrowser.erl"},
	   {"ScrolledMessageDialog", "scrolledmessagedialog.erl"}
	  ]},

	 %% core controls
	 {"Core Windows/Controls", 5,
	  [{"BitmapButton", "bitmapbutton.erl"},
	   {"Button", "button.erl"},
	   {"CheckBox", "checkbox.erl"},
	   {"CheckListBox", "checklistbox.erl"},
	   {"Choice", "choice.erl"},
	   {"ComboBox", "combobox.erl"},
	   {"CommandLinkButton", "commandlinkbutton.erl"},
	   {"DVC_CustomRenderer", "dvc_customrenderer.erl"},
	   {"DVC_DataViewModel", "dvc_dataviewmodel.erl"},
	   {"DVC_IndexListModel", "dvc_indexlistmodel.erl"},
	   {"DVC_ListCtrl", "dvc_listctrl.erl"},
	   {"DVC_TreeCtrl", "dvc_treectrl.erl"},
	   {"Gauge", "gauge.erl"},
	   {"Grid", "grid.erl"},
	   {"Grid_MegaExample", "grid_megaexample.erl"},
	   {"GridLabelRenderer", "gridlabelrenderer.erl"},
	   {"ListBox", "listbox.erl"},
	   {"ListCtrl", "listctrl.erl"},
	   {"ListCtrl_virtual", "listctrl_virtual.erl"},
	   {"ListCtrl_edit", "listctrl_edit.erl"},
	   {"Menu", "menu.erl"},
	   {"PopupMenu", "popupmenu.erl"},
	   {"PopupWindow", "popupwindow.erl"},
	   {"RadioBox", "radiobox.erl"},
	   {"RadioButton", "radiobutton.erl"},
	   {"SashWindow", "sashwindow.erl"},
	   {"ScrolledWindow", "scrolledwindow.erl"},
	   {"SearchCtrl",         "searchctrl.erl"},        
	   {"Slider", "slider.erl"},
	   {"SpinButton", "spinbutton.erl"},
	   {"SpinCtrl", "spinctrl.erl"},
	   {"SpinCtrlDouble", "spinctrldouble.erl"},
	   {"SplitterWindow", "splitterwindow.erl"},
	   {"StaticBitmap", "staticbitmap.erl"},
	   {"StaticBox", "staticbox.erl"},
	   {"StaticText", "statictext.erl"},
	   {"StatusBar", "statusbar.erl"},
	   {"StockButtons", "stockbuttons.erl"},
	   {"TextCtrl", "textctrl.erl"},
	   {"ToggleButton", "togglebutton.erl"},
	   {"ToolBar", "toolbar.erl"},
	   {"TreeCtrl", "treectrl.erl"},
	   {"Validator", "validator.erl"}
	  ]},

	 {"\"Book\" Controls", 6,
	  [{"AUI_Notebook", "aui_notebook.erl"},
	   {"Choicebook", "choicebook.erl"},
	   {"FlatNotebook", "flatnotebook.erl"},
	   {"Listbook", "listbook.erl"},
	   {"Notebook", "notebook.erl"},
	   {"Toolbook", "toolbook.erl"},
	   {"Treebook", "treebook.erl"}
	  ]},

	 {"Custom Controls", 7,
	  [{"AnalogClock", "analogclock.erl"},
	   {"ColourSelect", "colourselect.erl"},
	   {"ComboTreeBox", "combotreebox.erl"},
	   {"Editor", "editor.erl"},
	   {"GenericButtons", "genericbuttons.erl"},
	   {"GenericDirCtrl", "genericdirctrl.erl"},
	   {"ItemsPicker", "itemspicker.erl"},
	   {"LEDNumberCtrl", "lednumberctrl.erl"},
	   {"MultiSash", "multisash.erl"},
	   {"PlateButton", "platebutton.erl"},
	   {"PopupControl", "popupcontrol.erl"},
	   {"PyColourChooser", "pycolourchooser.erl"},
	   {"TreeListCtrl", "treelistctrl.erl"}
	  ]},

	 %% controls coming from other libraries
	 {"More Windows/Controls", 8,
	  [{"ActiveX_FlashWindow", "activex_flashwindow.erl"},
	   {"ActiveX_IEHtmlWindow", "activex_iehtmlwindow.erl"},
	   {"ActiveX_PDFWindow", "activex_pdfwindow.erl"},
	   {"BitmapComboBox", "bitmapcombobox.erl"},
	   {"Calendar", "calendar.erl"},
	   {"CalendarCtrl", "calendarctrl.erl"},
	   {"CheckListCtrlMixin", "checklistctrlmixin.erl"},
	   {"CollapsiblePane", "collapsiblepane.erl"},
	   {"ComboCtrl", "comboctrl.erl"},
	   {"ContextHelp", "contexthelp.erl"},
	   {"DatePickerCtrl", "datepickerctrl.erl"},
	   {"DynamicSashWindow", "dynamicsashwindow.erl"},
	   {"EditableListBox", "editablelistbox.erl"},
	   {"ExpandoTextCtrl", "expandotextctrl.erl"},
	   {"FancyText", "fancytext.erl"},
	   {"FileBrowseButton", "filebrowsebutton.erl"},
	   {"FloatBar",   "floatbar.erl"},  
	   {"FloatCanvas", "floatcanvas.erl"},
	   {"HtmlWindow", "htmlwindow.erl"},
	   {"HTML2_WebView", "html2_webview.erl"},
	   {"InfoBar", "infobar.erl"},
	   {"IntCtrl", "intctrl.erl"},
	   {"MVCTree",    "mvctree.erl"},   
	   {"MaskedEditControls", "maskededitcontrols.erl"},
	   {"MaskedNumCtrl", "maskednumctrl.erl"},
	   {"MediaCtrl", "mediactrl.erl"},
	   {"MultiSplitterWindow", "multisplitterwindow.erl"},
	   {"OwnerDrawnComboBox", "ownerdrawncombobox.erl"},
	   {"Pickers", "pickers.erl"},
	   {"PropertyGrid", "propertygrid.erl"},
	   {"PyCrust", "pycrust.erl"},
	   {"PyPlot", "pyplot.erl"},
	   {"PyShell", "pyshell.erl"},
	   {"ResizeWidget", "resizewidget.erl"},
	   {"RichTextCtrl", "richtextctrl.erl"},
	   {"ScrolledPanel", "scrolledpanel.erl"},
	   {"SplitTree", "splittree.erl"},
	   {"StyledTextCtrl_1", "styledtextctrl_1.erl"},
	   {"StyledTextCtrl_2", "styledtextctrl_2.erl"},
	   {"TablePrint", "tableprint.erl"},
	   {"Throbber", "throbber.erl"},
	   {"Ticker", "ticker.erl"},
	   {"TimeCtrl", "timectrl.erl"},
	   {"TreeMixin", "treemixin.erl"},
	   {"VListBox", "vlistbox.erl"}
	  ]},

	 %% How to lay out the controls in a frame/dialog
	 {"Window Layout", 9,
	  [{"GridBagSizer", "gridbagsizer.erl"},
	   {"LayoutAnchors", "layoutanchors.erl"},
	   {"LayoutConstraints", "layoutconstraints.erl"},
	   {"Layoutf", "layoutf.erl"},
	   {"RowColSizer", "rowcolsizer.erl"},
	   {"ScrolledPanel", "scrolledpanel.erl"},
	   {"SizedControls", "sizedcontrols.erl"},
	   {"Sizers", "sizers.erl"},
	   {"WrapSizer", "wrapsizer.erl"},
	   {"XmlResource", "xmlresource.erl"},
	   {"XmlResourceHandler", "xmlresourcehandler.erl"},
	   {"XmlResourceSubclass", "xmlresourcesubclass.erl"}
	  ]},

	 %% ditto
	 {"Process and Events", 10,
	  [{"DelayedResult", "delayedresult.erl"},
	   {"EventManager", "eventmanager.erl"},
	   {"KeyEvents", "keyevents.erl"},
	   {"Process", "process.erl"},
	   {"PythonEvents", "pythonevents.erl"},
	   {"Threads", "threads.erl"},
	   {"Timer", "timer.erl"}
	   %%"infoframe",    # needs better explanation and some fixing
	  ]},

	 %% Clipboard and DnD
	 {"Clipboard and DnD", 11,
	  [{"CustomDragAndDrop", "customdraganddrop.erl"},
	   {"DragAndDrop", "draganddrop.erl"},
	   {"URLDragAndDrop" "urldraganddrop.erl"}
	  ]},

	 %% Images
	 {"Using Images", 12,
	  [{"AdjustChannels", "adjustchannels.erl"},
	   {"AlphaDrawing", "alphadrawing.erl"},
	   {"AnimateCtrl", "animatectrl.erl"},
	   {"ArtProvider", "artprovider.erl"},
	   {"BitmapFromBuffer", "bitmapfrombuffer.erl"},
	   {"Cursor", "cursor.erl"},
	   {"DragImage", "dragimage.erl"},
	   {"Image", "image.erl"},
	   {"ImageAlpha", "imagealpha.erl"},
	   {"ImageFromStream", "imagefromstream.erl"},
	   {"Img2PyArtProvider", "img2pyartprovider.erl"},
	   {"Mask", "mask.erl"},
	   {"RawBitmapAccess", "rawbitmapaccess.erl"},
	   {"Throbber" "throbber.erl"}
	  ]},

	 %% Other stuff
	 {"Miscellaneous", 13,
	  [{"AlphaDrawing", "alphadrawing.erl"},
	   {"Cairo", "cairo.erl"},
	   {"Cairo_Snippets", "cairo_snippets.erl"},
	   {"ColourDB", "colourdb.erl"},
	   %%"DialogUnits",   # needs more explanations
	   {"DragScroller", "dragscroller.erl"},
	   {"DrawXXXList", "drawxxxlist.erl"},
	   {"FileHistory", "filehistory.erl"},
	   {"FontEnumerator", "fontenumerator.erl"},
	   {"GraphicsContext", "graphicscontext.erl"},
	   {"GraphicsGradient", "graphicsgradient.erl"},
	   {"GLCanvas", "glcanvas.erl"},
	   {"I18N",         "i18n.erl"},        
	   {"Joystick", "joystick.erl"},
	   {"MimeTypesManager", "mimetypesmanager.erl"},
	   {"MouseGestures", "mousegestures.erl"},
	   {"OGL", "ogl.erl"},
	   {"PDFViewer", "pdfviewer.erl"},
	   {"PenAndBrushStyles", "penandbrushstyles.erl"},
	   {"PrintFramework", "printframework.erl"},
	   {"PseudoDC", "pseudodc.erl"},
	   {"RendererNative", "renderernative.erl"},
	   {"ShapedWindow", "shapedwindow.erl"},
	   {"Sound", "sound.erl"},
	   {"StandardPaths", "standardpaths.erl"},
	   {"SystemSettings", "systemsettings.erl"},
	   {"UIActionSimulator", "uiactionsimulator.erl"},
	   {"Unicode", "unicode.erl"}
	  ]},
	 
	 {"Check out the samples dir too", -1, []}
	]).

%%-------------------------------------------------------------------
%% Code Editor stuff
%%-------------------------------------------------------------------

-define(RESERVED_WORDS, ["after", "and", "andalso",
			 "band", "begin", "bnot", "bor", "bsl", "bsr", "bxor",
			 "case", "catch", "cond",
			 "div",
			 "end",
			 "fun",
			 "if",
			 "let",
			 "not",
			 "of", "or", "orelse",
			 "receive", "rem",
			 "try",
			 "when",
			 "xor"]).

%% For wx-2.9 usage
-ifndef(wxSTC_ERLANG_COMMENT_FUNCTION).
-define(wxSTC_ERLANG_COMMENT_FUNCTION, 14).
-define(wxSTC_ERLANG_COMMENT_MODULE, 15).
-define(wxSTC_ERLANG_COMMENT_DOC, 16).
-define(wxSTC_ERLANG_COMMENT_DOC_MACRO, 17).
-define(wxSTC_ERLANG_ATOM_QUOTED, 18).
-define(wxSTC_ERLANG_MACRO_QUOTED, 19).
-define(wxSTC_ERLANG_RECORD_QUOTED, 20).
-define(wxSTC_ERLANG_NODE_NAME_QUOTED, 21).
-define(wxSTC_ERLANG_BIFS, 22).
-define(wxSTC_ERLANG_MODULES, 23).
-define(wxSTC_ERLANG_MODULES_ATT, 24).
-endif.

-define(ERLANG_STYLES, [{?wxSTC_ERLANG_DEFAULT,  {0,0,0}},
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
			%% {?wxSTC_ERLANG_SEPARATOR,{0,0,0}},
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
		       ]).

-endif.

-module(gui).
-compile(export_all).
-include_lib("wx/include/wx.hrl").


start() ->
	State = make_window(),
	loop(State).

make_window() ->
	Server = wx:new(),
	Frame = wxFrame:new(Server ,-1, "Ecc",[{size,{800,600}}]),
	Panel = wxPanel:new(Frame),
	
	MainSizer = wxBoxSizer:new(?wxHORIZONTAL),
	LeftSide = wxStaticBoxSizer:new(?wxVERTICAL),
	RightSide = wxStaticBoxSizer:new(?wxVERTICAL),
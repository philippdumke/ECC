-module(gui).
-compile(export_all).
-include_lib("wx/include/wx.hrl").

start() ->
    State = make_window(),
    loop(State).

make_window() ->
    Server = wx:new(),
    Frame = wxFrame:new(Server, -1, "ECC", [{size,{1200, 800}}]),
    Panel  = wxPanel:new(Frame),

%% Erstellen der Sizer für das Layout
    MainSizer = wxBoxSizer:new(?wxHORIZONTAL),
    RightSide = wxStaticBoxSizer:new(?wxVERTICAL, Panel,[]),
    LeftSide = wxStaticBoxSizer:new(?wxVERTICAL,Panel,[]),

    Eingabe = wxStaticBoxSizer:new(?wxHORIZONTAL,Panel,[]),
    EingabeHD = wxStaticBoxSizer:new(?wxVERTICAL,Panel,[]),
    EingabeTL = wxStaticBoxSizer:new(?wxVERTICAL,Panel,[]),

    Hash = wxStaticBoxSizer:new(?wxHORIZONTAL,Panel,[]),

    %%  Eingabe
    STEing1 = wxStaticText:new(Panel, 2001,"", []),
    STEing2 = wxStaticText:new(Panel, 2002,"Eingabe", []),
    STEing3 = wxStaticText:new(Panel, 2003,"", []),
    STEing4 = wxStaticText:new(Panel, 2004,"", []),

    TEingabe = wxTextCtrl:new(Panel, 1001,[{value, "lorem ipsum dolor"},{style,?wxDEFAULT bor ?wxTE_MULTILINE}]),
    %% Hash
    STHash1 = wxStaticText:new(Panel,2005, "   Hash        ",[]),
    TEHash = wxTextCtrl:new(Panel,1002,[{value,""},{style,?wxDEFAULT bor ?wxTE_MULTILINE bor ?wxTE_READONLY}]),



    % Eingabe Beschriftung
    wxSizer:add(EingabeHD,STEing1,[{flag,?wxEXPAND}, {proportion,1}]),
    wxSizer:add(EingabeHD,STEing2,[{flag,?wxEXPAND}, {proportion,1}]),
    wxSizer:add(EingabeHD,STEing3,[{flag,?wxEXPAND}, {proportion,1}]),
    wxSizer:add(EingabeHD,STEing4,[{flag,?wxEXPAND}, {proportion,1}]),

    wxSizer:add(Eingabe,EingabeHD,[{flag, ?wxEXPAND}]),
    wxSizer:add(Eingabe,TEingabe,[{flag,?wxEXPAND},{proportion,1}]),

    wxSizer:add(Hash, STHash1,[]),
    wxSizer:add(Hash,TEHash,[{flag,?wxEXPAND},{proportion,1}]),




    wxSizer:add(LeftSide,Eingabe,[{flag,?wxEXPAND}]),
    wxSizer:add(LeftSide,Hash,[{flag,?wxEXPAND}]),
    wxSizer:add(MainSizer,LeftSide,[{flag,?wxEXPAND},{proportion,1}]),
    wxSizer:add(MainSizer,RightSide,[{flag,?wxEXPAND},{proportion,1}]),

    wxPanel:setSizer(Panel, MainSizer),
    %% Frame anzeigen
    wxFrame:show(Frame),
    wxFrame:connect(Frame, close_window),
    wxFrame:connect(Frame, command_button_clicked),

    {Frame,TEingabe}.


loop(State) ->
    {Frame,T1001} = State,
    receive
        #wx{event=#wxClose{}} ->
            wxWindow:destroy(Frame),
            io:format("Exit Button klicked\n",[]),
            ok
    end.

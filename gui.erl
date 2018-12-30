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
    MainSizer = wxBoxSizer:new(?wxVERTICAL),
    GuiSizer = wxBoxSizer:new(?wxHORIZONTAL),
    RightSide = wxStaticBoxSizer:new(?wxVERTICAL, Panel,[]),
    LeftSide = wxStaticBoxSizer:new(?wxVERTICAL,Panel,[]),

    Eingabe = wxStaticBoxSizer:new(?wxHORIZONTAL,Panel,[]),
    EingabeHD = wxStaticBoxSizer:new(?wxVERTICAL,Panel,[]),
    EingabeTL = wxStaticBoxSizer:new(?wxVERTICAL,Panel,[]),

    BlockL = wxStaticBoxSizer:new(?wxHORIZONTAL,Panel,[]),
    BlockLHd = wxStaticBoxSizer:new(?wxHORIZONTAL,Panel,[]),

    Hash = wxStaticBoxSizer:new(?wxHORIZONTAL,Panel,[]),
    HashHd = wxStaticBoxSizer:new(?wxVERTICAL,Panel,[]),
    Buttons = wxStaticBoxSizer:new(?wxHORIZONTAL,Panel,[]),

    PrivKeyHd = wxStaticBoxSizer:new(?wxVERTICAL,Panel,[]),
    PrivKeyTl = wxStaticBoxSizer:new(?wxHORIZONTAL,Panel,[]),

    OefKeyHd = wxStaticBoxSizer:new(?wxVERTICAL,Panel,[]),
    OefKeyTL = wxStaticBoxSizer:new(?wxHORIZONTAL,Panel,[]),

    AusgHd = wxStaticBoxSizer:new(?wxVERTICAL,Panel,[]),
    AusgTl = wxStaticBoxSizer:new(?wxHORIZONTAL,Panel,[]),


    %%  Eingabe
    STEing1 = wxStaticText:new(Panel, 2001,"", []),
    STEing2 = wxStaticText:new(Panel, 2002,"Eingabe", []),
    STEing3 = wxStaticText:new(Panel, 2003,"", []),
    STEing4 = wxStaticText:new(Panel, 2004,"", []),

    TEingabe = wxTextCtrl:new(Panel, 1001,[{value, "lorem ipsum dolor"},{style,?wxDEFAULT bor ?wxTE_MULTILINE}]),
    %% Hash
    STHash1 = wxStaticText:new(Panel,2005, "Hash     ",[]),
    STHash2 = wxStaticText:new(Panel, 2006, " ",[]),
    TEHash = wxTextCtrl:new(Panel,1002,[{value,"Hier könnte Ihre Werbung stehen"},{style,?wxDEFAULT bor ?wxTE_MULTILINE bor ?wxTE_READONLY}]),

    %% Blocklänge
    TBlockL = wxTextCtrl:new(Panel,1003,[{value,"100"},{style,?wxDEFAULT}]),
    STBlockL1 = wxStaticText:new(Panel,2007,"Blocklänge",[]),
    STBlockL2 = wxStaticText:new(Panel,2008," ",[]),

    %% Priv Key
    STPrivKey1 = wxStaticText:new(Panel,2010, "PrivKey",[]),
    STPrivKey2 = wxStaticText:new(Panel,2011," ",[]),
    PrivKey = wxTextCtrl:new(Panel,1004,[{value," "},{style,?wxDEFAULT bor ?wxTE_MULTILINE}]),

    %% Ausgabe
    TAusgabe = wxTextCtrl:new(Panel,1011,[{value, ""},{style,?wxDEFAULT bor ?wxTE_MULTILINE}]),
    STAus1 = wxStaticText:new(Panel,2012,"Ausgabe",[]),
    STAus2 = wxStaticText:new(Panel,2013,"",[]),
    STAus3 = wxStaticText:new(Panel,2014,"",[]),
    STAus4 = wxStaticText:new(Panel,2015,"",[]),



    Encrypt = wxButton:new(Panel, 101,[{label,"Verschlüsseeln"}]),
    Decrypt = wxButton:new(Panel, 102, [{label, "Entschlüsseln"}]),
    Test = wxButton:new(Panel,103,[{label,"Test"}]),
    Key = wxButton:new(Panel,104,[{label,"Gen Key"}]),


    Status = wxTextCtrl:new(Panel,1010, [{value,"ok."},{style,?wxDEFAULT bor ?wxTE_READONLY}]),
    % Eingabe Beschriftung
    wxSizer:add(EingabeHD,STEing1,[{flag,?wxEXPAND}, {proportion,1}]),
    wxSizer:add(EingabeHD,STEing2,[{flag,?wxEXPAND}, {proportion,1}]),
    wxSizer:add(EingabeHD,STEing3,[{flag,?wxEXPAND}, {proportion,1}]),
    wxSizer:add(EingabeHD,STEing4,[{flag,?wxEXPAND}, {proportion,1}]),

    wxSizer:add(Eingabe,EingabeHD,[{flag, ?wxEXPAND}]),
    wxSizer:add(Eingabe,TEingabe,[{flag,?wxEXPAND},{proportion,1}]),

    % Hash
    wxSizer:add(HashHd, STHash1,[]),
    wxSizer:add(HashHd,STHash2,[]),
    wxSizer:add(Hash, HashHd,[]),
    wxSizer:add(Hash,TEHash,[{flag,?wxEXPAND},{proportion,1}]),

    %% Blocklänge
    wxSizer:add(BlockLHd,STBlockL1,[]),
    wxSizer:add(BlockLHd,STBlockL2,[]),
    wxSizer:add(BlockL,BlockLHd,[]),
    wxSizer:add(BlockL,TBlockL,[{flag,?wxEXPAND},{proportion,1}]),


    wxSizer:add(PrivKeyHd,STPrivKey1,[]),
    wxSizer:add(PrivKeyHd,STPrivKey2,[]),
    wxSizer:add(PrivKeyTl,PrivKeyHd,[]),
    wxSizer:add(PrivKeyTl,PrivKey,[{flag,?wxEXPAND},{proportion,1}]),
    % Ausgabe
    wxSizer:add(AusgHd,STAus4,[]),
    wxSizer:add(AusgHd,STAus1,[]),
    wxSizer:add(AusgHd,STAus2,[]),
    wxSizer:add(AusgHd,STAus3,[]),
    wxSizer:add(AusgTl,AusgHd,[]),
    wxSizer:add(AusgTl,TAusgabe,[{flag,?wxEXPAND},{proportion,1}]),

    % Buttons
    wxSizer:add(Buttons,Encrypt,[{flag,?wxEXPAND}, {proportion,1}]),
    wxSizer:add(Buttons,Decrypt,[{flag,?wxEXPAND}, {proportion,1}]),
    wxSizer:add(Buttons,Test,[{flag,?wxEXPAND}, {proportion,1}]),
    wxSizer:add(Buttons,Key,[{flag,?wxEXPAND}, {proportion,1}]),

    wxSizer:add(RightSide,Buttons,[{flag,?wxEXPAND}]),
    wxSizer:add(RightSide,AusgTl,[{flag,?wxEXPAND}]),



    %% Zusammenbauen
    wxSizer:add(LeftSide,Eingabe,[{flag,?wxEXPAND}]),
    wxSizer:add(LeftSide,Hash,[{flag,?wxEXPAND}]),
    wxSizer:add(LeftSide,BlockL,[{flag,?wxEXPAND}]),
    wxSizer:add(LeftSide,PrivKeyTl,[{flag,?wxEXPAND}]),
    wxSizer:add(GuiSizer,LeftSide,[{flag,?wxEXPAND},{proportion,1}]),
    wxSizer:add(GuiSizer,RightSide,[{flag,?wxEXPAND},{proportion,1}]),
    wxSizer:add(MainSizer,GuiSizer,[{flag,?wxEXPAND}]),
    wxSizer:add(MainSizer,Status,[{flag,?wxEXPAND}]),

    wxPanel:setSizer(Panel, MainSizer),
    %% Frame anzeigen
    wxFrame:show(Frame),
    wxFrame:connect(Frame, close_window),
    wxFrame:connect(Frame, command_button_clicked),

    {Frame,TEingabe,TAusgabe,TBlockL,[]}.


loop(State) ->
    {Frame,TEingabe,TAusgabe,TBlockL,Pid} = State,
    receive
        #wx{event=#wxClose{}} ->
            wxWindow:destroy(Frame),
            io:format("Exit Button klicked\n",[]),
            ok;
    %% Verschlüsseln
    #wx{id = 101, event=#wxCommand{type = command_button_clicked}} ->
        io:format("Verschlüsseln",[]),
        Eingabe = wxTextCtrl:getValue(TEingabe),
        io:format("Eingabe ~p",[Eingabe]),
        BlockLen = list_to_integer(wxTextCtrl:getValue(TBlockL)),
        io:format("BlockLen: ~p ",[BlockLen]),
        io:format("ublock",[]),
        Block = ecc:ublock(Eingabe,BlockLen,[]),
        io:format("unblock",[]),
        Message = string:strip(ecc:unblock(Block,BlockLen,[]),right),
        wxTextCtrl:changeValue(TAusgabe,Message),
        loop(State)


    end.

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
    Buttons2 = wxStaticBoxSizer:new(?wxHORIZONTAL,Panel,[]),

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

    %% Public Key
    TPubKey = wxTextCtrl:new(Panel,1005,[{value,""},{style, ?wxDEFAULT bor ?wxTE_MULTILINE}]),
    STPubKey1 = wxStaticText:new(Panel,2016,"Öff. Key"),
    STPubKey2 = wxStaticText:new(Panel,2017,""),
    STPubKey3 = wxStaticText:new(Panel,2018,""),
    %% Ausgabe
    TAusgabe = wxTextCtrl:new(Panel,1011,[{value, ""},{style,?wxDEFAULT bor ?wxTE_MULTILINE}]),
    STAus1 = wxStaticText:new(Panel,2012,"Ausgabe",[]),
    STAus2 = wxStaticText:new(Panel,2013,"",[]),
    STAus3 = wxStaticText:new(Panel,2014,"",[]),
    STAus4 = wxStaticText:new(Panel,2015,"",[]),

    TLog = wxTextCtrl:new(Panel, 2011,[{value, ""},{style,?wxDEFAULT bor ?wxTE_MULTILINE}]),

    Encrypt = wxButton:new(Panel, 101,[{label,"Verschlüsseln"}]),
    Decrypt = wxButton:new(Panel, 102, [{label, "Entschlüsseln"}]),
    Test = wxButton:new(Panel,103,[{label,"Test"}]),
    Key = wxButton:new(Panel,104,[{label,"Gen Key"}]),
    Kill = wxButton:new(Panel, 105,[{label, "Kill"}]),
    Clear = wxButton:new(Panel, 106,[{label, "Clear"}]),
    HashB =  wxButton:new(Panel, 107,[{label, "Hash"}]),


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

    %% Priv Key
    wxSizer:add(PrivKeyHd,STPrivKey1,[]),
    wxSizer:add(PrivKeyHd,STPrivKey2,[]),
    wxSizer:add(PrivKeyTl,PrivKeyHd,[]),
    wxSizer:add(PrivKeyTl,PrivKey,[{flag,?wxEXPAND},{proportion,1}]),

    %% Öff Key
    wxSizer:add(OefKeyHd,STPubKey1,[]),
    wxSizer:add(OefKeyHd,STPubKey2,[]),
    wxSizer:add(OefKeyHd,STPubKey3,[]),
    wxSizer:add(OefKeyTL,OefKeyHd,[]),
    wxSizer:add(OefKeyTL,TPubKey,[{flag,?wxEXPAND},{proportion,1}]),

    %% Ausgabe
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
    wxSizer:add(Buttons2,Kill,[{flag, ?wxEXPAND},{proportion,1}]),
    wxSizer:add(Buttons2, Clear,[{flag, ?wxEXPAND},{proportion,1}]),
    wxSizer:add(Buttons2, HashB,[{flag,?wxEXPAND},{proportion,1}]),

    wxSizer:add(RightSide,Buttons,[{flag,?wxEXPAND}]),
    wxSizer:add(RightSide,Buttons2,[{flag,?wxEXPAND}]),
    wxSizer:add(RightSide,AusgTl,[{flag,?wxEXPAND}]),
    wxSizer:add(RightSide,TLog,[{flag,?wxEXPAND},{proportion,1}]),



    %% Zusammenbauen
    wxSizer:add(LeftSide,Eingabe,[{flag,?wxEXPAND}]),
    wxSizer:add(LeftSide,Hash,[{flag,?wxEXPAND}]),
    wxSizer:add(LeftSide,BlockL,[{flag,?wxEXPAND}]),
    wxSizer:add(LeftSide,PrivKeyTl,[{flag,?wxEXPAND}]),
    wxSizer:add(LeftSide,OefKeyTL,[{flag,?wxEXPAND}]),
    wxSizer:add(GuiSizer,LeftSide,[{flag,?wxEXPAND},{proportion,1}]),
    wxSizer:add(GuiSizer,RightSide,[{flag,?wxEXPAND},{proportion,1}]),
    wxSizer:add(MainSizer,GuiSizer,[{flag,?wxEXPAND}]),
    wxSizer:add(MainSizer,Status,[{flag,?wxEXPAND}]),

    wxPanel:setSizer(Panel, MainSizer),
    %% Frame anzeigen
    wxFrame:show(Frame),
    wxFrame:connect(Frame, close_window),
    wxFrame:connect(Frame, command_button_clicked),

    {Frame,TEingabe,TAusgabe,TBlockL,TLog,Status,TEHash,[]}.


loop(State) ->
    {Frame,TEingabe,TAusgabe,TBlockL,Tlog,Status,TEHash,Pid} = State,
    receive
        #wx{event=#wxClose{}} ->
            wxWindow:destroy(Frame),
            io:format("Exit Button klicked\n",[]),
            kill(Pid),
            ok;
    %% Verschlüsseln
    #wx{id = 101, event=#wxCommand{type = command_button_clicked}} ->
        wxTextCtrl:changeValue(Tlog,(wxTextCtrl:getValue(Tlog) ++ "\n \n"++ "========>>>>>  Verschlüsseln")),
        Eingabe = wxTextCtrl:getValue(TEingabe),
        BlockLen = list_to_integer(wxTextCtrl:getValue(TBlockL)),
        Proc = spawn(ecc,test_block,[Eingabe,BlockLen,self()]),
        to_loop(State,Proc);

    {ausgabe,verschluesseln,Result} ->
        Message = string:strip(Result,right),
        wxTextCtrl:changeValue(TAusgabe,Message),
        loop(State);

    #wx{id = 102, event=#wxCommand{type = command_button_clicked}} ->
        wxTextCtrl:changeValue(Tlog,(wxTextCtrl:getValue(Tlog) ++ "\n \n" ++ "========>>>>> Entschlüsseln")),
        loop(State);

    #wx{id = 103, event=#wxCommand{type = command_button_clicked}} ->
        wxTextCtrl:changeValue(Tlog,wxTextCtrl:getValue(Tlog) ++ "\n \n" ++ "========>>>>> Test"),
        loop(State);

    #wx{id = 104, event=#wxCommand{type = command_button_clicked}} ->
        wxTextCtrl:changeValue(Tlog,wxTextCtrl:getValue(Tlog) ++ "\n \n" ++ "========>>>>> Generiere Schlüssel"),
        loop(State);


    %% Kill Beendet alle Threads, die von der GUI gespawned wurden
    #wx{id = 105, event=#wxCommand{type = command_button_clicked}} ->
            kill(Pid),
            wxTextCtrl:changeValue(Status,"ok."),
            loop(State);

    %% Log Fenster löschen
    #wx{id = 106, event=#wxCommand{type = command_button_clicked}} ->
            wxTextCtrl:changeValue(Tlog, ""),
            loop(State);

    %% Hash berechnen
    #wx{id = 107, event=#wxCommand{type = command_button_clicked}} ->
            Message = wxTextCtrl:getValue(TEingabe),
            Proc = spawn(ecc, compute_Hash,[Message, self()]),
            to_loop(State, Proc);

    {ausgabe, hash, Result} ->
            wxTextCtrl:changeValue(TEHash, Result),
            loop(State);

    {message,A} ->
        wxTextCtrl:changeValue(Tlog,(wxTextCtrl:getValue(Tlog) ++ "\n" ++ A)),
        loop(State);

    {tik} ->
        case wxTextCtrl:getValue(Status) of
            "|" -> Next = "/";
            "/" -> Next = "--";
            "--" -> Next = "\\ ";
            "\\ " -> Next = "| ";
            "| " -> Next = "/ ";
            "/ " -> Next = "-- ";
            "-- " -> Next = "\\";
            "\\" -> Next = "|";
            _ -> Next = "/"
        end,
        wxTextCtrl:changeValue(Status, Next),
        loop(State);
    {tik,ok} ->
        wxTextCtrl:changeValue(Status,"ok"),
        loop(State)
    end.

%% Fügt eine neue Pid an und ruft den loop auf
to_loop(State,Proc) ->
  {Frame,TEingabe,TAusgabe,TBlockL,Tlog,Status,TEHash,Pid} = State,
  loop({Frame,TEingabe,TAusgabe,TBlockL,Tlog,Status,TEHash,lists:append(Pid, [Proc])}).


kill(Pid) when length(Pid) == 0 ->
    io:format("Alle Threads beendet ~n",[]);
kill(Pid) ->
    Thread = hd(Pid),
    exit(Thread, exit),
    io:format("Beende Thread: ~p~n",[hd(Pid)]),
    kill(tl(Pid)).

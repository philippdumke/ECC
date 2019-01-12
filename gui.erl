-module(gui).
-compile(export_all).
-include_lib("wx/include/wx.hrl").

start() ->
    State = make_window(),
    loop(State).

make_window() ->
    Server = wx:new(),
    Frame = wxFrame:new(Server, -1, "ECC", [{size,{1200, 1000}}]),
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
    Hash2Hd = wxStaticBoxSizer:new(?wxVERTICAL,Panel,[]),
    Hash2Tl = wxStaticBoxSizer:new(?wxHORIZONTAL,Panel,[]),
    Buttons = wxStaticBoxSizer:new(?wxHORIZONTAL,Panel,[]),
    Buttons2 = wxStaticBoxSizer:new(?wxHORIZONTAL,Panel,[]),

    PrivKeyHd = wxStaticBoxSizer:new(?wxVERTICAL,Panel,[]),
    PrivKeyTl = wxStaticBoxSizer:new(?wxHORIZONTAL,Panel,[]),

    OefKeyHd = wxStaticBoxSizer:new(?wxVERTICAL,Panel,[]),
    OefKeyTL = wxStaticBoxSizer:new(?wxHORIZONTAL,Panel,[]),
    PHd = wxStaticBoxSizer:new(?wxVERTICAL,Panel,[]),
    PTl = wxStaticBoxSizer:new(?wxHORIZONTAL,Panel,[]),
    NHd = wxStaticBoxSizer:new(?wxVERTICAL,Panel,[]),
    NTl = wxStaticBoxSizer:new(?wxHORIZONTAL,Panel,[]),
    P1Hd = wxStaticBoxSizer:new(?wxVERTICAL,Panel,[]),
    P1Tl = wxStaticBoxSizer:new(?wxHORIZONTAL,Panel,[]),
    P2Hd = wxStaticBoxSizer:new(?wxVERTICAL,Panel,[]),
    P2Tl = wxStaticBoxSizer:new(?wxHORIZONTAL,Panel,[]),
    Y1Hd = wxStaticBoxSizer:new(?wxVERTICAL,Panel,[]),
    Y1Tl = wxStaticBoxSizer:new(?wxHORIZONTAL,Panel,[]),
    Y2Hd = wxStaticBoxSizer:new(?wxVERTICAL,Panel,[]),
    Y2Tl = wxStaticBoxSizer:new(?wxHORIZONTAL,Panel,[]),

    A1Hd = wxStaticBoxSizer:new(?wxVERTICAL,Panel,[]),
    A1Tl = wxStaticBoxSizer:new(?wxHORIZONTAL,Panel,[]),
    A2Hd = wxStaticBoxSizer:new(?wxVERTICAL,Panel,[]),
    A2Tl = wxStaticBoxSizer:new(?wxHORIZONTAL,Panel,[]),


    AusgHd = wxStaticBoxSizer:new(?wxVERTICAL,Panel,[]),
    AusgTl = wxStaticBoxSizer:new(?wxHORIZONTAL,Panel,[]),

    PrimHd = wxStaticBoxSizer:new(?wxVERTICAL,Panel,[]),
    PrimTl = wxStaticBoxSizer:new(?wxHORIZONTAL,Panel,[]),


    %%  Eingabe
    STEing1 = wxStaticText:new(Panel, 2001,"", []),
    STEing2 = wxStaticText:new(Panel, 2002,"Eingabe", []),
    STEing3 = wxStaticText:new(Panel, 2003,"", []),
    STEing4 = wxStaticText:new(Panel, 2004,"", []),

    TEingabe = wxTextCtrl:new(Panel, 1001,[{value, "lorem ipsum dolor"},{style,?wxDEFAULT bor ?wxTE_MULTILINE}]),
    %% Hash
    STHash1 = wxStaticText:new(Panel,2005, "S",[]),
    STHash2 = wxStaticText:new(Panel, 2006, " ",[]),
    TEHash = wxTextCtrl:new(Panel,1002,[{value,"Hier könnte Ihre Werbung stehen"},{style,?wxDEFAULT bor ?wxTE_MULTILINE bor ?wxTE_READONLY}]),
    TEHash2 = wxTextCtrl:new(Panel,1090,[{value,""},{style,?wxDEFAULT bor ?wxTE_MULTILINE}]),
    STHash3 = wxStaticText:new(Panel,2090,"R"),


    %% Blocklänge
    TBlockL = wxTextCtrl:new(Panel,1003,[{value,"10"},{style,?wxDEFAULT}]),
    STBlockL1 = wxStaticText:new(Panel,2007,"Blocklänge",[]),
    STBlockL2 = wxStaticText:new(Panel,2008," ",[]),
    PrimLen = wxTextCtrl:new(Panel,1040,[{value,"100"},{style,?wxDEFAULT}]),
    STPrim1 = wxStaticText:new(Panel,2050,"Prim Len"),

    %% Priv Key
    STPrivKey1 = wxStaticText:new(Panel,2010, "PrivKey",[]),
    STPrivKey2 = wxStaticText:new(Panel,2011," ",[]),
    PrivKey = wxTextCtrl:new(Panel,1004,[{value,""},{style,?wxDEFAULT bor ?wxTE_MULTILINE}]),

    %% Public Key
    TPubKey = wxTextCtrl:new(Panel,1005,[{value,""},{style, ?wxDEFAULT bor ?wxTE_MULTILINE}]),
    STPubKey1 = wxStaticText:new(Panel,2016,"Kurve"),
    STPubKey2 = wxStaticText:new(Panel,2017,""),
    STPubKey3 = wxStaticText:new(Panel,2018,""),

    TPubP = wxTextCtrl:new(Panel,1020,[{value,""},{style,?wxDEFAULT bor ?wxTE_MULTILINE}]),
    TPubN = wxTextCtrl:new(Panel,1021,[{value,""},{style,?wxDEFAULT bor ?wxTE_MULTILINE}]),
    TPubP1 = wxTextCtrl:new(Panel,1022,[{value,""},{style,?wxDEFAULT bor ?wxTE_MULTILINE}]),
    TPubP2 = wxTextCtrl:new(Panel,1023,[{value,""},{style,?wxDEFAULT bor ?wxTE_MULTILINE}]),
    TPubY1 = wxTextCtrl:new(Panel,1024,[{value,""},{style,?wxDEFAULT bor ?wxTE_MULTILINE}]),
    TPubY2 = wxTextCtrl:new(Panel,1025,[{value,""},{style,?wxDEFAULT bor ?wxTE_MULTILINE}]),
    TPubA1 = wxTextCtrl:new(Panel,1026,[{value,""},{style, ?wxDEFAULT bor ?wxTE_MULTILINE}]),
    TPubA2 = wxTextCtrl:new(Panel,1027,[{value,""},{style, ?wxDEFAULT bor ?wxTE_MULTILINE}]),

    STPP1 = wxStaticText:new(Panel,2030,"P"),
    STPP2 = wxStaticText:new(Panel,2031,""),
    STPN = wxStaticText:new(Panel,2032,"N"),
    STPN1 =wxStaticText:new(Panel,2033,""),
    STPPu1 = wxStaticText:new(Panel,2034,"P1"),
    STPPu2 = wxStaticText:new(Panel,2035,""),
    STPPu3 = wxStaticText:new(Panel,2036,"P2"),
    STPPu4 = wxStaticText:new(Panel,2037,""),
    STPY11 = wxStaticText:new(Panel,2038,"Y1"),
    STPY12 = wxStaticText:new(Panel,2039,""),
    STPY21 = wxStaticText:new(Panel,2040,"Y2"),
    STPY22 = wxStaticText:new(Panel,2041,""),
    STPA11 = wxStaticText:new(Panel,2042,"A1"),
    STPA12 = wxStaticText:new(Panel,2043,""),
    STPA21 = wxStaticText:new(Panel,2044,"A2"),
    STPA22 = wxStaticText:new(Panel,2045,""),



    %% Ausgabe
    TAusgabe = wxTextCtrl:new(Panel,1011,[{value, ""},{style,?wxDEFAULT bor ?wxTE_MULTILINE}]),
    STAus1 = wxStaticText:new(Panel,2012,"Ausgabe",[]),
    STAus2 = wxStaticText:new(Panel,2013,"",[]),
    STAus3 = wxStaticText:new(Panel,2014,"",[]),
    STAus4 = wxStaticText:new(Panel,2015,"",[]),

    TLog = wxTextCtrl:new(Panel, 2011,[{value, ""},{style,?wxDEFAULT bor ?wxTE_MULTILINE}]),

    Encrypt = wxButton:new(Panel, 101,[{label,"Verschlüsseln"}]),
    Decrypt = wxButton:new(Panel, 102, [{label, "Entschlüsseln"}]),
    Test = wxButton:new(Panel,103,[{label,"Check Signatur"}]),
    Key = wxButton:new(Panel,104,[{label,"Gen. Key"}]),
    Kill = wxButton:new(Panel, 105,[{label, "Kill"}]),
    Clear = wxButton:new(Panel, 106,[{label, "Clear"}]),
    HashB =  wxButton:new(Panel, 107,[{label, "Gen. Signatur"}]),
    Export = wxButton:new(Panel,108,[{label,"Export"}]),

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
    wxSizer:add(Hash2Hd,STHash3,[]),
    wxSizer:add(Hash2Tl,Hash2Hd,[]),
    wxSizer:add(Hash2Tl,TEHash2,[{flag,?wxEXPAND},{proportion,1}]),
    wxSizer:add(Hash, Hash2Tl,[{flag,?wxEXPAND},{proportion,1}]),

    %% Blocklänge
    wxSizer:add(BlockLHd,STBlockL1,[]),
    %wxSizer:add(BlockLHd,STBlockL2,[]),
    wxSizer:add(BlockL,BlockLHd,[]),
    wxSizer:add(BlockL,TBlockL,[{flag,?wxEXPAND},{proportion,1}]),
    wxSizer:add(PrimHd, STPrim1,[{flag,?wxEXPAND}]),
    wxSizer:add(PrimTl,PrimHd,[{flag,?wxEXPAND},{proportion,1}]),
    wxSizer:add(PrimTl,PrimLen,[{flag,?wxEXPAND},{proportion,1}]),
    wxSizer:add(BlockL,PrimTl,[{flag,?wxEXPAND}]),

    %% Priv Key
    wxSizer:add(PrivKeyHd,STPrivKey1,[]),
    wxSizer:add(PrivKeyHd,STPrivKey2,[]),
    wxSizer:add(PrivKeyTl,PrivKeyHd,[]),
    wxSizer:add(PrivKeyTl,PrivKey,[{flag,?wxEXPAND},{proportion,1}]),

    %% Öff Key
    wxSizer:add(OefKeyHd,STPubKey1,[]),
    %wxSizer:add(OefKeyHd,STPubKey2,[]),
    %wxSizer:add(OefKeyHd,STPubKey3,[]),
    wxSizer:add(OefKeyTL,OefKeyHd,[]),
    wxSizer:add(OefKeyTL,TPubKey,[{flag,?wxEXPAND},{proportion,1}]),

    wxSizer:add(PHd,STPP1,[]),
    %wxSizer:add(PHd, STPP2,[]),
    wxSizer:add(PTl, PHd,[]),
    wxSizer:add(PTl, TPubP,[{flag,?wxEXPAND},{proportion,1}]),

    wxSizer:add(NHd,STPN,[]),
    %wxSizer:add(NHd, STPN1,[]),
    wxSizer:add(NTl, NHd,[]),
    wxSizer:add(NTl, TPubN,[{flag,?wxEXPAND},{proportion,1}]),

    wxSizer:add(P1Hd,STPPu1,[]),
    %wxSizer:add(P1Hd, STPPu2,[]),
    wxSizer:add(P1Tl, P1Hd,[]),
    wxSizer:add(P1Tl, TPubP1,[{flag,?wxEXPAND},{proportion,1}]),

    wxSizer:add(P2Hd,STPPu3,[]),
    %wxSizer:add(P2Hd, STPPu4,[]),
    wxSizer:add(P2Tl, P2Hd,[]),
    wxSizer:add(P2Tl, TPubP2,[{flag,?wxEXPAND},{proportion,1}]),

    wxSizer:add(Y1Hd, STPY11,[]),
    %wxSizer:add(Y1Hd, STPY12,[]),
    wxSizer:add(Y1Tl, Y1Hd,[]),
    wxSizer:add(Y1Tl, TPubY1,[{flag,?wxEXPAND},{proportion,1}]),

    wxSizer:add(Y2Hd,STPY21,[]),
    %wxSizer:add(Y2Hd, STPY22,[]),
    wxSizer:add(Y2Tl, Y2Hd,[]),
    wxSizer:add(Y2Tl, TPubY2,[{flag,?wxEXPAND},{proportion,1}]),

    wxSizer:add(A1Hd, STPA11,[]),
    %wxSizer:add(A1Hd, STPA12,[]),
    wxSizer:add(A1Tl, A1Hd,[]),
    wxSizer:add(A1Tl, TPubA1,[{flag,?wxEXPAND},{proportion,1}]),

    wxSizer:add(A2Hd,STPA21,[]),
    %wxSizer:add(A2Hd, STPA22,[]),
    wxSizer:add(A2Tl, A2Hd,[]),
    wxSizer:add(A2Tl, TPubA2,[{flag,?wxEXPAND},{proportion,1}]),



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
    wxSizer:add(Buttons2,Export,[{flag,?wxEXPAND},{proportion,1}]),

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
    wxSizer:add(LeftSide,PTl,[{flag,?wxEXPAND}]),
    wxSizer:add(LeftSide,NTl,[{flag,?wxEXPAND}]),
    wxSizer:add(LeftSide,P1Tl,[{flag,?wxEXPAND}]),
    wxSizer:add(LeftSide,P2Tl,[{flag,?wxEXPAND}]),
    wxSizer:add(LeftSide,Y1Tl,[{flag,?wxEXPAND}]),
    wxSizer:add(LeftSide,Y2Tl,[{flag,?wxEXPAND}]),
    wxSizer:add(LeftSide,A1Tl,[{flag,?wxEXPAND}]),
    wxSizer:add(LeftSide,A2Tl,[{flag,?wxEXPAND}]),

    wxSizer:add(GuiSizer,LeftSide,[{flag,?wxEXPAND},{proportion,1}]),
    wxSizer:add(GuiSizer,RightSide,[{flag,?wxEXPAND},{proportion,1}]),
    wxSizer:add(MainSizer,GuiSizer,[{flag,?wxEXPAND}]),
    wxSizer:add(MainSizer,Status,[{flag,?wxEXPAND}]),

    wxPanel:setSizer(Panel, MainSizer),
    %% Frame anzeigen
    wxFrame:show(Frame),
    wxFrame:connect(Frame, close_window),
    wxFrame:connect(Frame, command_button_clicked),

    {Frame,TEingabe,TAusgabe,TBlockL,TLog,Status,TEHash,PrivKey,TPubKey,TPubP,TPubN,TPubP1,TPubP2,TPubY1,TPubY2,PrimLen,TPubA1,TPubA2,TEHash2,[]}.


loop(State) ->
    {Frame,TEingabe,TAusgabe,TBlockL,Tlog,Status,TEHash,PrivKey,TPubKey,TPubP,TPubN,TPubP1,TPubP2,TPubY1,TPubY2,PrimLen,TPubA1,TPubA2,TEHash2,Pid} = State,
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
        {K,P,N,{P1,P2},{Y1,Y2}} = decodeOefKey(State),
        Proc = spawn(ecc,verschluesseln,[Eingabe,BlockLen,P,N,P1,P2,Y1,Y2,K,self()]),
        to_loop(State,Proc);

    {ausgabe,verschluesseln,Result} ->
        Message = string:strip(Result,right),
        wxTextCtrl:changeValue(TAusgabe,Message),
        loop(State);

    #wx{id = 102, event=#wxCommand{type = command_button_clicked}} ->
        wxTextCtrl:changeValue(Tlog,(wxTextCtrl:getValue(Tlog) ++ "\n \n" ++ "========>>>>> Entschlüsseln")),
        {K,P,M,{P1,P2},{Y1,Y2}} = decodeOefKey(State),
        BlockLen = list_to_integer(wxTextCtrl:getValue(TBlockL)),
        X = list_to_integer(wxTextCtrl:getValue(PrivKey)),
        A1 = list_to_integer(wxTextCtrl:getValue(TPubA1)),
        A2 = list_to_integer(wxTextCtrl:getValue(TPubA2)),
        Mess = wxTextCtrl:getValue(TEingabe),
        Proc = spawn(ecc,entschluesseln,[Mess, BlockLen, P, -1,{A1,A2}, X, self()]),
        loop(State);

    #wx{id = 103, event=#wxCommand{type = command_button_clicked}} ->
        wxTextCtrl:changeValue(Tlog,wxTextCtrl:getValue(Tlog) ++ "\n \n" ++ "========>>>>> Test"),

         Message = wxTextCtrl:getValue(TAusgabe),
        {K,P,N,{P1,P2},{Y1,Y2}} = decodeOefKey(State),
        R = list_to_integer(wxTextCtrl:getValue(TEHash2)),
        S = list_to_integer(wxTextCtrl:getValue(TEHash)),
        Proc = spawn(ecc, check_sig,[N,{P1,P2},P,K,Message,{Y1,Y2},R,S,self()]),
        to_loop(State, Proc);


    #wx{id = 104, event=#wxCommand{type = command_button_clicked}} ->
        wxTextCtrl:changeValue(Tlog,wxTextCtrl:getValue(Tlog) ++ "\n \n" ++ "========>>>>> Generiere Schlüssel"),
        Len = list_to_integer(wxTextCtrl:getValue(PrimLen)),
        spawn(ecc,calc_key,[Len,-1,self()]),
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
        {K,P,N,{P1,P2},{Y1,Y2}} = decodeOefKey(State),
        X = list_to_integer(wxTextCtrl:getValue(PrivKey)),
        Proc = spawn(ecc, make_sig,[N,{P1,P2},P,K,Message,X,self()]),
        to_loop(State, Proc);

    #wx{id = 108, event=#wxCommand{type = command_button_clicked}} ->
        A = wxTextCtrl:getValue(PrivKey) ++ wxTextCtrl:getValue(TPubKey),
        case length(A) == 0 of
            false ->  {K,P,N,{P1,P2},{Y1,Y2}} = decodeOefKey(State),
                      Priv = wxTextCtrl:getValue(PrivKey),
                      io:format("P2: ~p" ,[P2]),
                      String = integer_to_list(K) ++ ", " ++ integer_to_list(P) ++ ", " ++  integer_to_list(N) ++ ", " ++ integer_to_list(P1) ++ ", " ++ integer_to_list(P2) ++ ", " ++  integer_to_list(Y1) ++ ", " ++ integer_to_list(Y2) ++ ", " ++ Priv,% ++ ", " ++ integer_to_list(A1) ++ ", " ++ integer_to_list(A2),
                      file:write_file(key,String);
            true -> {ok,String} = file:read_file(key),
                    List = string:tokens(binary_to_list(String), ", "),

                    wxTextCtrl:changeValue(TPubKey,lists:nth(1,List)),
                    wxTextCtrl:changeValue(TPubP,lists:nth(2,List)),
                    wxTextCtrl:changeValue(TPubN,lists:nth(3,List)),
                    wxTextCtrl:changeValue(TPubP1,lists:nth(4,List)),
                    wxTextCtrl:changeValue(TPubP2,lists:nth(5,List)),
                    wxTextCtrl:changeValue(TPubY1,lists:nth(6,List)),
                    wxTextCtrl:changeValue(TPubY2,lists:nth(7,List)),
                    wxTextCtrl:changeValue(PrivKey,lists:nth(8,List))
                    %wxTextCtrl:changeValue(TPubA1,lists:nth(9,List)),
                    %wxTextCtrl:changeValue(TPubA2, lists:nth(10,List))
        end,
        loop(State);

    {sig,{R,S}} ->
            wxTextCtrl:changeValue(TEHash,integer_to_list(R)),
            wxTextCtrl:changeValue(TEHash2,integer_to_list(S)),
            loop(State);

    {hash,left, Result} ->
        wxTextCtrl:changeValue(TEHash, Result),
        loop(State);

    {message,A} ->
        wxTextCtrl:changeValue(Tlog,(wxTextCtrl:getValue(Tlog) ++ "\n" ++ A)),
        loop(State);
    {ausgabe, A} ->
        wxTextCtrl:changeValue(TAusgabe,A),
        loop(State);

    {ausgabe,priv,A} ->
        wxTextCtrl:changeValue(PrivKey,integer_to_list(A)),
        loop(State);
    {ausgabe,oef,{K,P,N,P1,P2,{Y1,Y2}}} ->
        wxTextCtrl:changeValue(TPubKey,integer_to_list(K)),
        wxTextCtrl:changeValue(TPubP, integer_to_list(P)),
        wxTextCtrl:changeValue(TPubN, integer_to_list(N)),
        wxTextCtrl:changeValue(TPubP1, integer_to_list(P1)),
        wxTextCtrl:changeValue(TPubP2, integer_to_list(P2)),
        wxTextCtrl:changeValue(TPubY1, integer_to_list(Y1)),
        wxTextCtrl:changeValue(TPubY2, integer_to_list(Y2)),
        loop(State);
    {ausgabe,a,{A1,A2}} ->
        wxTextCtrl:changeValue(TPubA1,integer_to_list(A1)),
        wxTextCtrl:changeValue(TPubA2,integer_to_list(A2)),
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
  {Frame,TEingabe,TAusgabe,TBlockL,Tlog,Status,TEHash,PrivKey,TPubKey,TPubP,TPubN,TPubP1,TPubP2,TPubY1,TPubY2,PrimLen,TPubA1,TPubA2,TEHash2,Pid} = State,
  loop({Frame,TEingabe,TAusgabe,TBlockL,Tlog,Status,TEHash,PrivKey,TPubKey,TPubP,TPubN,TPubP1,TPubP2,TPubY1,TPubY2,PrimLen,TPubA1,TPubA2,TEHash2,lists:append(Pid, [Proc])}).

decodeOefKey(State) ->
    {Frame,TEingabe,TAusgabe,TBlockL,Tlog,Status,TEHash,PrivKey,TPubKey,TPubP,TPubN,TPubP1,TPubP2,TPubY1,TPubY2,PrimLen,TPubA1,TPubA2,TEHash2,Pid} = State,
    Tokens = string:tokens(wxTextCtrl:getValue(TPubKey),"K: P: N: X: Y: , \n"),
    K = list_to_integer(wxTextCtrl:getValue(TPubKey)),
    P = list_to_integer(wxTextCtrl:getValue(TPubP)),
    N = list_to_integer(wxTextCtrl:getValue(TPubN)),
    P1 = list_to_integer(wxTextCtrl:getValue(TPubP1)),
    P2 = list_to_integer(wxTextCtrl:getValue(TPubP2)),
    Y1 = list_to_integer(wxTextCtrl:getValue(TPubY1)),
    Y2 = list_to_integer(wxTextCtrl:getValue(TPubY2)),
    %A1 = list_to_integer(wxTextCtrl:getValue(TPubA1)),
    %A2 = list_to_integer(wxTextCtrl:getValue(TPubA2)),
    {K,P,N,{P1,P2},{Y1,Y2}}.


kill(Pid) when length(Pid) == 0 ->
    io:format("Alle Threads beendet ~n",[]);
kill(Pid) ->
    Thread = hd(Pid),
    exit(Thread, exit),
    io:format("Beende Thread: ~p~n",[hd(Pid)]),
    kill(tl(Pid)).

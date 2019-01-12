-module(ecc).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).


%%Schnelle Exponentiation mit modulo
fpow(A,1,M) -> A rem M;
fpow(A,2,M) -> A * A rem M;
fpow(A,B,M) ->
    B1 = B div 2,
    B2 = B - B1,
    P = fpow(A,B1,M),
    case B2 of
        B1 -> (P*P) rem M;
        _  -> (P*P*A) rem M
    end.

pow(_,0) -> 1; % Wenn B == 0
pow(A,1) -> A; % Wenn B == 1
pow(A,B) -> A * (pow(A, B-1)).

fastExponentiation(_,0)  -> 1; % Wenn B == 0
fastExponentiation(A,B) when B < 0 -> pow(1/A,-B);
fastExponentiation(A,B) when B rem 2 == 1 -> A * pow(pow(A,(B-1) div 2),2);
fastExponentiation(A,B) when B rem  2 == 0 -> pow(pow(A,B div 2),2).


ublock(Input, _, Result) when length(Input) == 0 -> Result;
ublock(Input, K, Result) when length(Input) > K ->
    {NewInput,NewResult} = u_to_block(Input,K-1,0),
    ublock(NewInput, K, lists:append(Result, [NewResult]));
ublock(Input,K,Result) when length(Input) < K ->
    ublock(lists:append(Input, [32]),K,Result);
ublock(Input,K,Result) when length(Input) == K ->
    {_,NewResult} = u_to_block(Input,K-1,0),
    lists:append(Result, [NewResult]).

u_to_block(Input, 0, Result) ->
    Res = hd(Input) + Result,
    {tl(Input), Res};
u_to_block(Input, K, Result) ->
    u_to_block(tl(Input), K-1, hd(Input) * fastExponentiation(4294967296,K) + Result).


unblock(Input,_,Result) when length(Input) == 0  -> Result;
unblock(Input, K, Result) ->
    Res = u_from_block(hd(Input), K-1, Result),
    unblock(tl(Input),K,Res).


u_from_block(Input, 0, Result) ->
    Res = lists:append(Result,[Input]),
    Res;
u_from_block(Input, K, Result) ->
    Mul = fastExponentiation(4294967296,K),
    A = Input div Mul ,
    NewInput = Input - (A * Mul),
    NewResult = lists:append(Result,[A]),
	u_from_block(NewInput,K-1, NewResult).


test_block(Message, Len, Pid) ->
    Tik = spawn(ecc,tik,[Pid, self()]),
    link(Tik),
    Block = text_to_block(Message, Len, Pid),
    Result = block_to_text(Block, Len, Pid),
    Pid ! {ausgabe, verschluesseln, Result},
    exit(Tik, done),
    Pid ! {tik,ok}.

%% Füllt Blöcke mit Leerzeichen auf
add_Block(0,Result) -> Result;
add_Block(K,Result) ->
    add_Block(K-1,lists:append(Result,[32])).

%% Aufruf als Thread
%% Generiert aus einem String eine Liste aus Blöcken
%% Strings werden ergänzt, sodass sie immer eine grade Anzahl an Blöcken ergeben.
%% Sendet Log Nachrichten an die GUI
text_to_block(Message, Blocklaenge, Pid) ->
    Len = length(Message) rem (Blocklaenge * 2),
    if Len == 0 ->  Result = ublock(Message,Blocklaenge,[]);
       Len > Blocklaenge -> Result = ublock(Message,Blocklaenge,[]);
      true -> Result = ublock(lists:append(Message,add_Block(Blocklaenge,[])),Blocklaenge,[])
    end,
    Pid ! {message, "Liste aus Blöcken wurde erstellt" },
    Result.

%% Aufruf als Thread
%% Generiert aus einer Liste von Blöcken einen String
%% Sendet Log Nachrichten an die GUI
block_to_text(Input,Blocklaenge, Pid) ->
    Result = unblock(Input, Blocklaenge,[]),
    Pid ! {message, "Aus den Blöcken wurde ein String erstellt"},
    Result.

%% Berechnet den Hash  -- DEPRICATED
compute_Hash(Input, Pid) ->
    Tik = spawn(ecc,tik, [Pid, self()]),
    link(Tik),
    Pid ! {message, "Hash wird berrechnet"},
    Result = md5:md5_hex(Input),
    Pid ! {ausgabe, hash, Result},
    exit(Tik, done).

%% Generiert eine Zufallszahl der Länge N
make(N) -> new_seed(), make(N,0).
make(0,D) -> D;
make(N,D) ->
    make(N-1,D*10+(rand:uniform(10)-1)).

%% Generiert einen neuen Seed für einen Zufall. Bei Aufruf wird der neue Seed dem rand:uniform zur Vefügung gestellt.
new_seed() ->
    {_,_,X} = erlang:timestamp(),
    {H,M,S} =  time(),
    H1 = H * X rem 32767,
    M1 = M * X rem 32767,
    S1 = S * X rem 32767,
    put(random_seed,{H1,M1,S1}). %Setzte neuen Seed für rand:uniform


is_prime(D) ->
    new_seed(),
    is_prime(D,100).
is_prime(D,Tests) ->
    N = length(integer_to_list(D)) - 1,
    is_prime(Tests, D, N).


is_prime(0,_,_) -> true;
is_prime(Test,N,Len) ->
    K = rand:uniform(Len),
    A = make(K),
    if A < N ->
           B = N-1,
           case fpow(A,(N-1) div 2, N) of
               1 -> is_prime(Test - 1, N, Len);
               B -> is_prime(Test - 1, N,Len);
               _ -> false
            end;
        true -> is_prime(Test, N, Len)
    end.

%% Berechnet Legendre Symbol wenn C und P teilerfremd sind
euler_kriterium(C,P) ->
   Result = fpow(C,(P-1) div 2, P),
   N = P-1,
   case Result of
       1 -> true;
       N -> false;
       _ -> 0
    end.
% Aufruf aus der GUI, berrechnet einen Schlüssel
calc_key(Len, A,Pid) ->
    Pid ! {proc, self()},
    Tik = spawn(ecc,tik, [Pid, self()]),
    link(Tik),
    Proc = prime:spawnPrimes(self(),4,Len),
    Point = new_make_key(Len, A,1,1,Pid),
    % TODO Proc ist eine Liste !!!!
    kill(Proc),
    exit(Tik,kill),
    Pid ! {tik,ok},
    Point.

kill(Proc) when length(Proc) == 1 -> exit(hd(Proc),kill);
kill(Proc) ->
    io:format("Kill: ~p~n", [hd(Proc)]),
    exit(hd(Proc),kill),
    kill(tl(Proc)).


%% Generiert den Schlüssel
new_make_key(Len,A,Anz,AnzN,Pid) ->
    new_seed(),
    receive
        {prime, P} ->
            case P rem 8 == 5 of
                false -> new_make_key(Len,A,Anz+1,AnzN,Pid);
                true ->
                    %% Generiert eine Zahl X derenen korespondierender Wert Y ein Punkt auf der Kurve ergibt
                    X = get_valid_euklid(P),
                    %% Euklidischer Algorithmus zerlegt die Primzahl in zwei Quadrate
                    {X1,X2} = euklid({X,1},{P,0}),
                    io:format("lösung: ~p ~p ~n" ,[X1,X2]),
                    %% Für A = -1 die alte Funktion, für andere Funktionen wird die Ordnung über test_alpha bestimmt
                    case A of
                        -1 -> N = calc_n(abs(X1),abs(X2), P);
                        _ -> N = test_alpha({X1,X2},P,A)
                    end,
                    io:format("N: ~p~n",[N]),
                    %% Test ob Ordnung einen großen Primteiler enthält, damit der Diskrete Logarithmus schwer lösbar ist
                    case is_prime(N div 8) of
                        false -> new_make_key(Len,A,Anz+1,AnzN+1,Pid); %Abbruch neu anfangen
                        true ->
                            Pid ! {message, "Anzahl geprüfter Primzahlen: " ++ integer_to_list(Anz)},
                            Pid ! {message, "Anzahl geprüfter Ordnungen: " ++ integer_to_list(AnzN)},
                            %% KurvenPunkt berechnen
                            Point = calc_point(Len, P, A),
                            {P1,P2,P} = Point,
                            make_key_extension(N,Point,A,Pid),
                            io:format("Point: ~p ~p ~n ~n Prim: ~p",[P1,P2,P])
                    end
            end
    end.

%% Testet das alpha für Ordnungsberechnung a /= 1
test_alpha({X,Y},P,A) when X rem 2 == 0 -> test_alpha({Y,X},P,A);
test_alpha({X,Y},P,A) ->
    Euler = euler_kriterium(A,P),
    case Euler of
        true -> E = 1;
        false -> E = -1;
        _ -> E = 0,exit(test_alpha_euler)
    end,
    QRe = rounded_div(((X * 2) +( Y * 2 )), 8),
    QIm = rounded_div((Y * 2) -( X * 2), 8),
    QRe2 = rounded_div((-X * 2) + (Y * 2),8),
    QIm2 = rounded_div((Y * 2) - (-X * 2), 8),
    MRe = (2 * QRe - 2 * QIm) + E,
    MIm = 2 * QRe + 2 * QIm,
    MRe2 = (2 * QRe2 -2 * QIm2) + E,
    MIm2 = 2 * QRe2 + 2 * QIm2,
    io:format("Mre: ~p ~n ~p ~n",[MRe,MRe2]),
    case X of
        MRe -> P-(2* MRe)+1;
        _ ->
            case -X of
                MRe2 -> P-(2*MRe2)+1;
                _ -> exit(mre_no_match)
            end

    end.

make_key_extension(N,Punkt,A,Pid) ->
    {P1,P2,P} = Punkt,
    OS = N div 8,
    %% Generiert eine Zahl kleiner als N durch 8
    X = make_less_os(OS,length(integer_to_list(OS))),
    %% Schneller Addition
    Y = fmult({P1,P2},P,A,X),
    Pid ! {ausgabe, priv, X},
    Pid ! {ausgabe, oef, {A,P,N,P1,P2,Y}}.


%% Erzeugt eine Zahl kleiner als OS
make_less_os(OS,Len) ->
    N = make(Len),
    if N =< OS -> N;
        true -> make_less_os(OS,Len)
    end.

% Berrechnet einen Punkt auf der Kurve
calc_point(Len, P,A) ->
    %% Zufallszahl generieren
    R1 = make(Len - 2),
    R = (fastExponentiation(R1,3) - R1) rem P,
    %% Testet ob es ein korenspondierenden Y gibt
    case euler_kriterium(R,P) of
        false -> calc_point(Len, P, A);
        0 -> exit("eulerkriterium ist komisch");
        true ->
            L = P - 1,
            io:format("fpow( ~p, ~p, ~p)",[R,(P-1) div 4,P]),
            Temp =  fpow(R,(P - 1) div 4, P),
            io:format("calc_point Temp: ~p, L: ~p ~n",[Temp,L]),
            %% Berrechnen von Y je nachdem ob das Eulerkriterium 1 oder -1 liefert
            case Temp of
                1 -> R2 = fpow(R,( P + 3) div 8, P),
                     %% Testet ob die Ordnung größer 8G ist
                     Ordnung = ordT({R1 , R2},0,A,P),
                     case Ordnung of
                         false -> calc_point(Len,P,A);
                          _ -> {R1,R2,P}
                     end;

                L ->
                    R2 = ((P+1) div 2) * fpow((4 * R), ((P + 3) div 8), P) rem P,
                    Ordnung = ordT({R1 , R2},0,A,P),
                    case Ordnung of
                        false -> calc_point(Len,P,A);
                        _ -> {R1,R2,P}
                    end;
                _ -> exit("Fehler")
            end
    end.


%%Funktion zum runden
c_to_Z({A1,A2}) ->
    if A1 >= 0.5 -> B1 = trunc(A1 - 0.5) + 1;
       true -> B1 = trunc(A1 -0.5)
    end,

    if A2 >= 0.5 -> B2 = trunc(A2 - 0.5) + 1;
       true -> B2 = trunc(A2 -0.5)
    end,
    {B1,B2}.

%% Addiert zwei Punkte
tangente({X1,_},{X2,_},_) when X1 == X2 ->
    unendlichFernerPunkt;

tangente({X1,Y1},{X2,Y2},P) ->
    Xtemp = X2-X1,
    if Xtemp < 0 -> Xinf = Xtemp + P;
       true -> Xinf = Xtemp
    end,
    M = (Y2-Y1) * multiplikativInverses(Xinf,P),
    X3 = (pow(M,2) - X1 - X2) rem P,
    if X3 < 0 -> X4 = X3 +P;
       true -> X4 = X3
    end,
    Y3 = (-M * (X4 - X1) - Y1) rem P,
    if Y3 < 0 -> Y4 = Y3 + P;
       true -> Y4 = Y3
    end,
    {X4,Y4}.

%% Addiert einen Punkt mit sich selbst
sehne({_,Y},_,_) when Y == 0 ->
    unendlichFernerPunkt;
sehne({X,Y},A,P) ->
    M = (3 * pow(X, 2) + A) * multiplikativInverses(2 * Y,P),
    X3 = fpow(M,2,P) - (2 * X) rem P,
    if X3 < 0 -> X4 = X3 + P;
      true -> X4 = X3
    end,
   Y1 = (-M * (X4 - X) - Y) rem P,
   if Y1 < 0 -> Y2 = Y1 + P;
      true -> Y2 = Y1
    end,
   {X4,Y2}.


multiplikativInverses(A,M) ->
    {_,X,_} = extggT(A,M),
    Erg = X rem M,
    if Erg < 0 -> Erg + M;
          true -> Erg
    end.

%%
%% Ausgabe des Tubels wenn B = 0
extggT(A,0) -> {A,1,0};

extggT(A,B) ->
    {G,U,X} = extggT(B, A rem B),
    Q = A div B, %Floor division
    {G,X,(U - (Q * X))}.

%Testet ob die Ordnung Element 2,4,8 ist
ordT({_,_},K,_,_) when K == 4 ->
    true;
ordT({X,Y},K,A,P) ->
    Temp = sehne({X,Y},A,P),
    case Temp of
        unendlichFernerPunkt -> false;
        _ -> ordT(Temp,K+1,A,P)
    end.

%% Startwer für den Euklidischen Algorithmus (W)
get_valid_euklid(P) ->
    A = make(length(integer_to_list(P))-1),
    K = P-1,
    case fpow(A,K div 2,P) of
        K -> fpow(A,K div 4,P);
        _ -> get_valid_euklid(P)
    end.

%% Euklidischer Algorithmus für Gauss Zahlen
euklid( {0,0},B) -> B;
euklid({A1,A2},{B1,B2}) when A1 =:= B1, B2 =:= A2 -> {A1,A2};
euklid(A,B)  ->
    case is_less(A,B) of
        true ->
                {A1,A2} = A,
                {B1,B2} = B,
                %io:format("A: ~p ~p ~n",[A1,A2]),
                %E1 = ((B1 * A1) + (B2 * -A2)) div (pow(A1,2) + pow(A2,2)),
                %E2 = ((B1 * -A2) + (B2 * A1)) div (pow(A1,2) + pow(A2,2)),
                E1 = rounded_div(((B1 * A1) + (B2 * -A2)), (pow(A1,2) + pow(A2,2))),
                E2 = rounded_div(((B1 * -A2) + (B2 * A1)), (pow(A1,2) + pow(A2,2))),
                %io:format("E: ~p ~p ~n",[E1,E2]),
                {F1,F2} = c_to_Z({E1,E2}),
                %io:format("F: ~p ~p ~n",[F1,F2]),
                C1 = B1 - (A1 * F1 - A2 * F2),
                C2 = B2 - (A1 * F2 + A2 * F1),
                %io:format("C: ~p ~p ~n",[C1,C2]),
                %io:format("B: ~p ~p ~n",[B1,B2]),
                euklid({C1,C2},A);
        equals -> A;
        false -> euklid (B,A)
    end.

%% Berrechnet die Ordnung der Gruppe A = -1
calc_n(X,Y,P) when X rem 2 == 1 ->
    calc_n(Y,X,P);
calc_n(X,Y,P) ->
    case X rem 4 of
        0 ->
            case Y rem 4 of
                3 -> (P + 1) - ( -2 * Y);
                1 -> (P + 1) - ( 2 * Y)
            end;
        2 ->
            case Y rem 4 of
                3 -> (P + 1) - ( 2 * Y);
                1 -> (P + 1) - (- 2 * Y)
            end
    end.


% Liefert true wenn die Komplexe Zahl A kleiner ist als B
is_less(A,B) ->
    {A1,A2} = A,
    {B1,B2} = B,
    A_abs = pow(A1,2) + pow(A2,2),
    B_abs = pow(B1,2) + pow(B2,2),
    if A_abs < B_abs -> true;
       A_abs == B_abs -> equals;
       true -> false
    end.

tik(Pid, Parent) ->
    Proc = process_info(Parent),
    timer:sleep(200),
    Pid ! {tik},
    tik(Pid,Parent).

hash(Input) ->
    lists:flatten([integer_to_list(X,16) || <<X>> <= crypto:hash(sha,unicode:characters_to_nfc_binary(Input))]).

split(Input,Result) when length(Input) == 1 -> lists:append([hd(Input)],Result).


%% Schnelle "Multiplikation" / "Addition"
fmult(Punkt,P,A,Faktor) ->
    Bin = lists:reverse(integer_to_list(Faktor,2)),
    %% 49 = 1 ; 48 = 0
    case hd(Bin) of
        49 -> Plist = fmult(Punkt,P,A,tl(Bin),[Punkt]);
        48 -> Plist = fmult(Punkt,P,A,tl(Bin),[]);
        true -> Plist = [],
                exit(wrong_representation)
    end,
    fmult_add(hd(Plist),P,A,tl(Plist)).

fmult(Punkt,P,A,Bin,Result) when length(Bin) == 0 -> Result;


fmult(Punkt,P,A,Bin,Result) ->
    NP = sehne(Punkt,A,P),
    % 1 = 49 und 0 = 48
    case hd(Bin) of
        49 -> fmult(NP,P,A,tl(Bin),lists:append(Result,[NP]));
        48 -> fmult(NP,P,A,tl(Bin),Result);
        true -> Result
    end.
fmult_add(Punkt,P,A,Plist) when length(Plist) == 0 -> Punkt;

fmult_add(Punkt,P,A,Plist) when length(Plist) == 1 ->
    tangente(Punkt,hd(Plist),P);
fmult_add(Punkt,P,A,Plist) ->
    NP = tangente(Punkt,hd(Plist),P),
    fmult_add(NP,P,A,tl(Plist)).

%% Berrechnet die größtmögliche Blocklänge
getBlockLen(P,C,S) ->
%4294967296
    A = ublock(S,C,[]),
    case hd(A) < P of
        false -> C-1;
        true -> getBlockLen(P,C+1,lists:append([4294967295],S))
    end.

%% Wird aus der GUI Aufgerufen  verschlüsselt
verschluesseln(M,B,P,N,G1,G2,Y1,Y2,A,Pid) ->
    TextLen = length(M),
    Pid ! {message, "Länge der Nachricht: " ++ integer_to_list(TextLen)},
    Proc = spawn(ecc,tik,[Pid,self()]),
    link(Proc),
    Bllen = getBlockLen(P,1,[4294967295]),
    io:format("~p",[P]),
    Hash = hash(string:trim(M)),
    Pid ! {message, "Hash: " ++ Hash},
    case B > Bllen of
        false -> Pid ! {message, "[enc] Blocklänge ok. Starte Blockchiffre"},
                 Block = text_to_block(M,B,Pid),
                 {K,Res1,Res2} = genk(Y1,Y2,N,P,A),
                 Pid ! {message, "[enc] K: " ++ integer_to_list(K) ++ "\nPunkt: " ++ integer_to_list(Res1) ++ ", \n" ++ integer_to_list(Res2)},
                 EncList = makechifBlock(Block,[],K,{Res1,Res2},{G1,G2},P,A,Pid),
                 Pid ! {message, "[enc] Blöcke sind verschlüsselt"},
                 io:format("Enc:List ~p ~n",[EncList]),
                 Cipher = list_to_cipher(EncList,[]),
                 Pid ! {ausgabe,Cipher};

        true ->  Pid ! {message, ("[enc] Blocklänge " ++ integer_to_list(B) ++ " ist zu groß")},
                 exit(error)
    end,
    exit(Proc,done),
    Pid ! {message, "Verschlüsseln abgeschlossen"},
    Pid ! {tik,ok}.

list_to_cipher(Message,Result) when length(Message) == 1 ->
    Res = new_integer_to_cipher(hd(Message)),
    lists:append(Result,lists:append("|",Res));
list_to_cipher(Message, Result) ->
    Res = new_integer_to_cipher(hd(Message)),
    list_to_cipher(tl(Message), lists:append(Result, lists:append("|",Res))).

new_integer_to_cipher(Message) ->
    base64:encode_to_string(integer_to_list(Message)).

integer_to_cipher(Message,Result) ->
    Z = Message div 100,
    if Z == 0 ->
                 Last = Message div 100,
                 Encode = base64:encode_to_string([Last]),
                 lists:append(Encode,Result);
       true ->
                Last = Message rem 100,
                Encode = base64:encode_to_string([Last]),
                integer_to_cipher(Message div 100, lists:append(Encode,Result))
    end.

%% Erzeugt aus einer Liste von Blöcken eine Liste mit verschlüsselten Tupeln
makechifBlock(M,Result,K,{C1,C2},{G1,G2},P,A, Pid) when length(M) == 2 ->
    M1 = hd(M),
    M2 = hd(tl(M)),
    {A1,B1,B2} = genchif(K,{C1,C2},{G1,G2},P,{M1,M2},A,Pid),
    Pid ! {ausgabe, a, A1},
    lists:append(Result,lists:append([B1],[B2]));
makechifBlock(M,Result,K,{C1,C2},{G1,G2},P,A,Pid) ->
    M1 = hd(M),
    M2 = hd(tl(M)),
    {A1,B1,B2} = genchif(K,{C1,C2},{G1,G2},P,{M1,M2},A,Pid),
    Pid ! {ausgabe, a, A1},
    makechifBlock(tl(tl(M)),lists:append(Result,lists:append([B1],[B2])),K,{C1,C2},{G1,G2},P,A,Pid).

%% Algorithmus 3.3 Punkt 1) 4 aka Punkt verschlüsselung
genchif(K,{C1,C2},{G1,G2},P,{M1,M2},A,Pid) ->
    {A1,A2} = fmult({G1,G2},P,A,K),
    B1 = C1 * M1 rem P,
    %Pid ! {message, "----------"},
    %Pid ! {message, "Verschlüssele: " ++ integer_to_list(C1) ++ " --> " ++ integer_to_list(B1)},
    %Pid ! {message, " +++++++++ "},
    B2 = C2 * M2 rem P,
    %Pid ! {message, "Verschlüssele: " ++ integer_to_list(C2) ++ " --> " ++ integer_to_list(B2)},
    %Pid ! {message, "----------"},
    {{A1,A2},B1,B2}.

%Algorithmus §.3 Punkt 1) 3 Zufallszahl für Verschlüsselung
genk(Y1,Y2,N,P,A) ->
    K = make_less_than(N),
    {Res1,Res2} = fmult({Y1,Y2},P,A,K),
    if Res1 == 0 -> genk(Y1,Y2,N,P,A);
       Res2 == 0 -> genk(Y1,Y2,N,P,A);
       true -> {K,Res1,Res2}
    end.

%% Generiert eine Zahl kleiner A
make_less_than(A) ->
    make_less_than(A,length(integer_to_list(A))).
make_less_than(A,Len) ->
    B = make(Len),
    if B < A -> B;
       true -> make_less_than(A,Len)
    end.

%% Wird aus der GUI aufgerufen  entschlüsselt
entschluesseln(Chif, BlockLen, P, A, {A1,A2},X,Pid) ->
    Proc = spawn(ecc,tik,[Pid, self()]),
    link(Proc),
    List = cipher_to_list(Chif),
    DechifList = list_dechif(List, {A1,A2},P,X,A,[],Pid),
    Result = block_to_text(DechifList,BlockLen,Pid),
    Pid ! {ausgabe, Result},
    Hash = hash(string:trim(Result)),
    Pid ! {message, "Hash: " ++ Hash},
    exit(Proc,done),
    Pid ! {tik,ok}.



list_dechif(List,{A1,A2},P,X,A,Result,Pid) when length(List) == 2 ->
    {R1,R2} = makedechif({A1,A2},P,hd(List),hd(tl(List)),X,A,Pid),
    lists:append(Result,lists:append([R1],[R2]));

list_dechif(List,{A1,A2},P,X,A,Result,Pid)  ->
    {R1,R2} = makedechif({A1,A2},P,hd(List),hd(tl(List)),X,A,Pid),
    list_dechif(tl(tl(List)),{A1,A2},P,X,A,lists:append(Result,lists:append([R1],[R2])),Pid).



cipher_to_integer(List,Result) when length(List) == 1 ->
    %io:format("List: ~p~n",[hd(List)]),
    Res = list_to_integer(base64:decode_to_string(hd(List))),
    %io:format("Res: ~p~n",[Res]),
    lists:append(Result, [Res]);

cipher_to_integer(List, Result) ->
    Res = list_to_integer(base64:decode_to_string(hd(List))),
    %io:format("res: ~p~n",([Res])),
    %io:format("Result : ~p~n",[Result]),
    A = lists:append(Result,[Res]),
    %io:format("Liste: ~p~n",[A]),
    cipher_to_integer(tl(List),lists:append(Result,[Res])).

cipher_to_string(List,Result) when length(List) == 4 ->
    A = Result + hd(base64:decode_to_string(string:slice(List,0,4))),
    %io:format("A: ~p~n",[A]),
    A;

cipher_to_string(List, Result) ->
    A = string:slice(List,0,4),
    B = base64:decode_to_string(A),
    C = hd(B) *100,
    Chiph = Result + hd(base64:decode_to_string(string:slice(List, 0,4))) *100 ,
    cipher_to_string(tl(tl(tl(tl(List)))),Chiph).

cipher_to_list(Input) ->
    Tokens = string:tokens(Input,"|"),
    cipher_to_integer(Tokens,[]).

makedechif({A1,A2},P,B1,B2,X,A,Pid) ->
    {C1,C2} = fmult({A1,A2},P,A,X),
    M1 = (B1 * multiplikativInverses(C1,P))rem P,
    %Pid ! {message, "------------"},
    %Pid ! {message, "decrypt: " ++ integer_to_list(B1) ++ " --> " ++ integer_to_list(M1)},
    M2 = (B2 * multiplikativInverses(C2,P)) rem P,
    %Pid ! {message, " +++++++++++ "},
    %Pid ! {message, "decrypt: " ++ integer_to_list(B2) ++ " --> " ++ integer_to_list(M2)},
    %Pid ! {message, "------------"},
    {M1,M2}.

%% Generiert eine Signatur
make_sig(Ordnung,{G1,G2},P,A, Nachricht,Priv,Pid) ->
    K = make_less_than(Ordnung-1),
    {U,_} = fmult({G1,G2},P,A,K),
    R = U rem Ordnung,
    %% Testet ob U = Ordnung
    case R of
        0 -> make_sig(Ordnung,{G1,G2},P,A, Nachricht,Priv,Pid);
        _ ->
            %% Generiert einen Hash
            Hash = hash(string:trim(Nachricht)),
            %% Blockchiffre mit der Blocklänge 50 um eine Zahl aus dem Hash zu generieren
            ListofHash = ublock(Hash,50,[]),
            S = ((hd(ListofHash) + Priv * R) * multiplikativInverses(K,Ordnung)) rem Ordnung,
           case S of
              0 -> make_sig(Ordnung,{G1,G2},P,A,Nachricht,Priv,Pid);
              _ -> Pid ! {sig,{R,S}}
            end
    end.

%Prüft die Signatur ---> Funktioniert manchmal
check_sig(Ordnung,{G1,G2},P,A,Nachricht,{Y1,Y2},S,R, Pid) ->
    ListofHash = ublock(hash(string:trim(Nachricht)),50,[]),
    W = multiplikativInverses(S,Ordnung),
    U1 = (W * hd(ListofHash)) rem Ordnung,
    U2 = (R * W) rem Ordnung,
    {U3,_} = tangente(fmult({G1,G2}, P,A,U1),fmult({Y1,Y2},P,A,U2),P),
    io:format("U3: ~p~n ",[U3]),
    U4 = U3 rem Ordnung,
   case R rem Ordnung of
      U4 -> Pid ! {message, "Signatur ok."};
      _ -> Pid ! {message, "Signatur nicht ok."}
    end.

%% Ganzzahlige Division mit korrekter Rundung
rounded_div(A,B) ->
    C = A div B,
    D = C * B,
    E = A - D,
    F = B div 2,
    if E < F -> C;
       true -> C+1
    end.

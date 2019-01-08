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
%ublock(Input,K,Result) when length(Input) rem  (2 * K) /= 0 ->
%    ublock(lists:append(Input,[32]),K,Result);
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

add_Block(0,Result) -> Result;
add_Block(K,Result) ->
    add_Block(K-1,lists:append(Result,[32])).

%% Aufruf als Thread
%% Generiert aus einem String eine Liste aus Blöcken
%% Strings werden ergänzt, sodass sie immer eine grade Anzahl an Blöcken ergeben.
%% Sendet Log Nachrichten an die GUI
text_to_block(Message, Blocklaenge, Pid) ->
    %Len = length(Message),
    %Pid !{message, string:concat("Länge der Eingabe: ", Len)},

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

compute_Hash(Input, Pid) ->
    Tik = spawn(ecc,tik, [Pid, self()]),
    link(Tik),
    Pid ! {message, "Hash wird berrechnet"},
    Result = md5:md5_hex(Input),
    Pid ! {ausgabe, hash, Result},
    exit(Tik, done).


%% Addiert zwei Komplexe Zahlen
add(A,B) ->
    {A1,A2} = A,
    {B1,B2} = B,
    Result = {A1 + B1, A2 + B2},
    Result.
%% Subtrahiert zwei Komplexe Zahlen
sub(A,B) ->
    {A1,A2} = A,
    {B1,B2} = B,
    Result = {A1 - B1, A2 - B2},
    Result.
%% Multipliziert zwei Komplexe Zahlen
mul(A,B) ->
    {A1,A2} = A,
    {B1,B2} = B,
    Real = (A1 * B1) - (A2 * B2),
    Img = (A1 * B2) + (B1 * A2),
    {Real , Img}.


%% Generiert eine sichere Primzahl
make_prime(K) when K > 0 ->
    new_seed(),
    N = make(K),
    if N > 3 ->
           io:format("Generiere eine ~w stellige Primzahl ", [K]),
           MaxTries = N - 3,
           P1 = make_prime(MaxTries, N + rand:uniform(9), K),
           io:format("~n", []),
           P1;
       true -> make_prime(K)
    end.

make_prime(0,_,Len) -> exit(max_tries_exceeded);
make_prime(K,P,Len) ->
    io:format(".",[]),
    %case miller_rabin:is_prime(P) of
    case ez_crypt_miller_rabin:is_probably_prime(P) of
        true -> P;
        false -> make_prime(K-1,P+1,Len)
    end.

is_mod8(P) ->
    if (P rem 8) == 5 ->
           true;
    true  ->
           false
    end.

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

euler_kriterium(C,P) ->
   Result = fpow(C,(P-1) div 2, P),
   N = P-1,
   case Result of
       1 -> true;
       N -> false;
       _ -> 0
    end.

calc_key(Len, A,Pid) ->
    Tik = spawn(ecc,tik, [Pid, self()]),
    link(Tik),
    Proc = prime:spawnPrimes(self(),4,Len),
    Point = new_make_key(Len, A,Pid),
    kill(Proc),
    exit(Tik,done),
    Pid ! {tik,ok},
    Point.

kill(Proc) when length(Proc) == 1 -> exit(hd(Proc),done);
kill(Proc) ->
    io:format("Kill: ~p~n", [hd(Proc)]),
    exit(hd(Proc),done),
    kill(tl(Proc)).

new_make_key(Len,A,Pid) ->
    new_seed(),
    receive
        {prime, P} ->
            case P rem 8 == 5 of
                false -> new_make_key(Len,A,Pid);
                true ->
                    X = get_valid_euklid(P),
                    {X1,X2} = euklid({X,1},{P,0}),
                    io:format("lösung: ~p ~p ~n" ,[X1,X2]),
                    N = calc_n(abs(X1),abs(X2), P),
                    io:format("N: ~p~n",[N]),
                    case is_prime(N div 8) of
                        false -> new_make_key(Len,A,Pid); %Abbruch neu anfangen
                        true ->
                            Point = calc_point(Len, P, A),
                            {P1,P2,P} = Point,
                            make_key_extension(N,Point,A,Pid),
                            io:format("Point: ~p ~p ~n ~n Prim: ~p",[P1,P2,P])
                    end
            end
    end.

make_key_extension(N,Punkt,A,Pid) ->
    {P1,P2,P} = Punkt,
    OS = N div 8,
    X = make_less_os(OS,length(integer_to_list(OS))),
    Y = fmult({P1,P2},P,A,X),
    Pid ! {ausgabe, priv, X},
    Pid ! {ausgabe, oef, {1,P,N,P1,P2,Y}}.


make_less_os(OS,Len) ->
    N = make(Len),
    if N =< OS -> N;
        true -> make_less_os(OS,Len)
    end.

make_key(Len,A) ->
    new_seed(),
    % Erzeugen einer zufälligen Zahl kleiner als P
    W2 = make(Len -1),
    % Erzeugen einer Primzahl
    P = make_prime(Len),
    case euler_kriterium(W2,P) of
        false ->
            W = fpow(W2, (P - 1) div 4,P),
            {X,Y} = euklid({W,1},{P,0}),
            N = calc_n(abs(X),abs(Y),P), %Betrag
            case is_prime(N div 8) of
                false -> make_key(Len,A); %Abbruch neu anfangen
                true ->
                    Point = calc_point(Len, P, A)

            end;

            %% Hier gehts dann weiter
        true -> make_key(Len,A)
    end.

calc_point(Len, P,A) ->
    R1 = make(Len - 2),
    R = (fastExponentiation(R1,3) - R1) rem P,
    case euler_kriterium(R,P) of
        false -> calc_point(Len, P, A);
        0 -> exit("eulerkriterium ist komisch");
        true ->
            L = P - 1,
            io:format("fpow( ~p, ~p, ~p)",[R,(P-1) div 4,P]),
            Temp =  fpow(R,(P - 1) div 4, P),
            io:format("calc_point Temp: ~p, L: ~p ~n",[Temp,L]),
            case Temp of
                1 -> {R1 , fpow(R,( P + 3) div 8, P),P};
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


% Funktion mappen
c_to_Z({A1,A2}) ->
    if A1 >= 0.5 -> B1 = trunc(A1 - 0.5) + 1;
       true -> B1 = trunc(A1 -0.5)
    end,

    if A2 >= 0.5 -> B2 = trunc(A2 - 0.5) + 1;
       true -> B2 = trunc(A2 -0.5)
    end,
    {B1,B2}.


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

get_valid_euklid(P) ->
    A = make(length(integer_to_list(P))-1),
    K = P-1,
    case fpow(A,K div 2,P) of
        K -> fpow(A,K div 4,P);
        _ -> get_valid_euklid(P)
    end.


euklid( {0,0},B) -> B;
euklid({A1,A2},{B1,B2}) when A1 =:= B1, B2 =:= A2 -> {A1,A2};
euklid(A,B)  ->
    case is_less(A,B) of
        true ->
                {A1,A2} = A,
                {B1,B2} = B,
                %io:format("A: ~p ~p ~n",[A1,A2]),
                E1 = ((B1 * A1) + (B2 * -A2)) div (pow(A1,2) + pow(A2,2)),
                E2 = ((B1 * -A2) + (B2 * A1)) div (pow(A1,2) + pow(A2,2)),
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

test(Pid) ->
    Message = "Hallo",
    Tik = spawn(ecc, tik, [Pid, self()]),
    Pid ! {message, Message},
    io:format("~p",[Message]),
    timer:sleep(10000),
    exit(Tik,done),
    Pid ! {tik,ok}.

tik(Pid, Parent) ->
    Proc = process_info(Parent),
    case lists:nth(3,Proc) of
        {status,_} ->
                timer:sleep(200),
                Pid ! {tik},
                tik(Pid,Parent);
        _ -> io:format("exit",[]), exit(self(),exited)
    end.

hash(Input, Pid, Token) ->
    Pid ! {hash, Token, lists:flatten([integer_to_list(X,16) || <<X>> <= crypto:hash(md5,unicode:characters_to_nfc_binary(Input))])}.

split(Input,Result) when length(Input) == 1 -> lists:append([hd(Input)],Result).



fmult(Punkt,P,A,Faktor) ->
    Bin = lists:reverse(integer_to_list(Faktor,2)),
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


getBlockLen(P,C,S) ->
%4294967296
    A = ublock(S,C,[]),
    case hd(A) < P of
        false -> C-1;
        true -> getBlockLen(P,C+1,lists:append([4294967295],S))
    end.

verschluesseln(M,B,P,N,G1,G2,Y1,Y2,A,Pid) ->
    Proc = spawn(ecc,tik,[Pid,self()]),
    link(Proc),
    Bllen = getBlockLen(P,1,[4294967295]),
    io:format("~p",[P]),
    case B > Bllen of
        false -> Pid ! {message, "[enc] Blocklänge ok. Starte Blockchiffre"},
                 Block = text_to_block(M,B,Pid),
                 {K,Res1,Res2} = genk(Y1,Y2,N,P,A),
                 Pid ! {message, "[enc] K: " ++ integer_to_list(K) ++ "Punkt: " ++ integer_to_list(Res1) ++ ", " ++ integer_to_list(Res2)},
                 EncList = makechifBlock(Block,[],K,{Res1,Res2},{G1,G2},P,A),
                 Pid ! {message, "[enc] Blöcke sind verschlüsselt"},
                 Cipher = list_to_cipher(EncList,[]),
                 Pid ! {message,Cipher};

        true ->  Pid ! {message, ("[enc] Blocklänge " ++ integer_to_list(B) ++ " ist zu groß")},
                 exit(error)
    end.

list_to_cipher(Message,Result) when length(Message) == 1 ->
    Res = integer_to_cipher(hd(Message),[]),
    lists:append(Result,Res);
list_to_cipher(Message, Result) ->
    Res = integer_to_cipher(hd(Message),[]),
    list_to_cipher(tl(Message), lists:append(Result, lists:append("|",Res))).

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
makechifBlock(M,Result,K,{C1,C2},{G1,G2},P,A) when length(M) == 2 ->
    M1 = hd(M),
    M2 = hd(tl(M)),
    {A1,B1,B2} = genchif(K,{C1,C2},{G1,G2},P,{M1,M2},A),
    lists:append(Result,lists:append([B1],[B2]));
makechifBlock(M,Result,K,{C1,C2},{G1,G2},P,A) ->
    M1 = hd(M),
    M2 = hd(tl(M)),
    {A1,B1,B2} = genchif(K,{C1,C2},{G1,G2},P,{M1,M2},A),
    Pid ! {a,A}, %  Muss in die GUI %TODO
    makechifBlock(tl(tl(M)),lists:append(Result,lists:append([B1],[B2])),K,{C1,C2},{G1,G2},P,A).

%% Algorithmus 3.3 Punkt 1) 4
genchif(K,{C1,C2},{G1,G2},P,{M1,M2},A) ->
    {A1,A2} = fmult({G1,G2},P,A,K),
    B1 = C1 * M1 rem P,
    B2 = C2 * M2 rem P,
    {{A1,A2},B1,B2}.%% Fertiges Chiffrat ?

%Algorithmus §.3 Punkt 1) 3
genk(Y1,Y2,N,P,A) ->
    K = make_less_than(N),
    {Res1,Res2} = fmult({Y1,Y2},P,A,K),
    if Res1 == 0 -> genk(Y1,Y2,N,P,A);
       Res2 == 0 -> genk(Y1,Y2,N,P,A);
       true -> {K,Res1,Res2}
    end.

make_less_than(A) ->
    make_less_than(A,length(integer_to_list(A))).
make_less_than(A,Len) ->
    B = make(Len),
    if B < A -> B;
       true -> make_less_than(A,Len)
    end.

entschluesseln(Chif, BlockLen, P, A, {A1,A2},X,Pid) ->
    List = cipher_to_list(Chif),
    DechifList = list_dechif(List, {A1,A2},P,X,A,[]),
    Result = block_to_text(DechifList,BlockLen,Pid),
    Pid ! {message, Result}.


list_dechif(List,{A1,A2},P,X,A,Result) when length(List) == 2 ->
    {R1,R2} = makedechif({A1,A2},P,hd(List),hd(tl(List)),X,A),
    lists:append(Result,lists:append(R1,R2));

list_dechif(List,{A1,A2},P,X,A,Result)  ->
    {R1,R2} = makedechif({A1,A2},P,hd(List),hd(tl(List)),X,A),
    list_dechif(tl(tl(List)),{A1,A2},P,X,A,lists:append(Result,lists:append(R1,R2))).



cipher_to_integer(List,Result) when length(List) == 1 ->
    Res = cipher_to_string(hd(List),[]),
    lists:append(Result, Res);

cipher_to_integer(List, Result) ->
    Res = cipher_to_string(hd(List), []),
    cipher_to_integer(tl(List),lists:append(Result,Res)).

cipher_to_string(List,Result) when length(List) == 4 ->
    Result + list_to_integer(base64:decode_to_string(string:slice(List,0,4)));

cipher_to_string(List, Result) ->
    Chiph = Result + list_to_integer(base64:decode_to_string(string:slice(List, 0,4))) *100 ,
    cipher_to_string(tl(tl(tl(tl(List)))),Chiph).

cipher_to_list(Input) ->
    Tokens = string:tokens(Input,"|"),
    cipher_to_integer(Tokens,[]).

makedechif({A1,A2},P,B1,B2,X,A) ->
    {C1,C2} = fmult({A1,A2},P,A,X),
    M1 = (B1 * multiplikativInverses(C1,P))rem P,
    M2 = (B2 * multiplikativInverses(C2,P)) rem P,
    {M1,M2}.

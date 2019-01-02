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





add(A,B) ->
    {A1,A2} = A,
    {B1,B2} = B,
    Result = {A1 + B1, A2 + B2},
    Result.

sub(A,B) ->
    {A1,A2} = A,
    {B1,B2} = B,
    Result = {A1 - B1, A2 - B2},
    Result.

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
           P1 = make_prime(MaxTries, N +1),
           io:format("~n", []),
           P1;
       true -> make_prime(K)
    end.

make_prime(0,_) -> exit(something_happend);
make_prime(K,P) ->
    io:format(".",[]),
    case is_mod8(P) of
        true ->
            case is_prime(P) of
                true -> P;
                false -> make_prime(K-1, P + 8)
            end;
        false ->
            case is_prime(P) of
                true -> P;
                false -> make_prime(K-1,P+1)
            end
    end.

is_mod8(P) ->
    if (P rem 8) == 5 ->
           true;
    true  ->
           false
    end.

make(N) -> new_seed(), make(N,0).
make(0,D) -> D;
make(N,D) -> make(N-1,D*10+(rand:uniform(10)-1)).

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
            Temp =  fpow(R,(P - 1) / 4, P),
            io:format("calc_point Temp: ~p, L: ~p ~n",[Temp,L]),
            case Temp of
                1 -> {R1 , fpow(R,( P + 3) / 8, P)};
                L ->
                    R2 = ((P+1) div 2) * fpow((4 * R), ((p + 3) div 8), P) rem P,
                    Ordnung = ordT({R1 , R2},0,A),
                    case Ordnung of
                        false -> calc_point(Len,P,A);
                        _ -> {R1,R2}
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


tangente({X1,_},{X2,_}) when X1 == X2 ->
    unendlichFernerPunkt;

tangente({X1,Y1},{X2,Y2}) ->
    M = (Y2-Y1) / (X2-X1),
    X3 = pow(M,2) - X1 - X2,
    {X3, -(M * (X3 - X1) + Y1)}.

sehne({_,Y},_) when Y == 0 ->
    unendlichFernerPunkt;
sehne({X,Y},A) ->
    M = (3 * pow(X, 2) + A) / (2 * Y),
    X3 = pow(M,2) - (2 * X),
    {X3, -(M * (X3 - X) + Y)}.


%Testet ob die Ordnung Element 2,4,8 ist
ordT({_,_},K,_) when K == 4 ->
    true;
ordT({X,Y},K,A) ->
    Temp = sehne({X,Y},A),
    case Temp of
        unendlichFernerPunkt -> false;
        _ -> ordT(Temp,K+1,A)
    end.


euklid( {0,0},B) -> B;

euklid(A,B)  ->
    case is_less(A,B) of
        true ->
                {A1,A2} = A,
                {B1,B2} = B,
                io:format("A: ~p ~p ~n",[A1,A2]),
                E1 = ((B1 * A1) + (B2 * A2)) / (pow(A1,2) + pow(A2,2)),
                E2 = ((B1 * A2) + (B2 * A1)) / (pow(A1,2) + pow(A2,2)),
                io:format("E: ~p ~p ~n",[E1,E2]),
                {F1,F2} = c_to_Z({E1,E2}),
                C1 = B1 - (A1 * F1 - A2 * F2),
                C2 = B2 - (A1 * F2 + A2 * F1),
                euklid({C1,C2},A);
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
       A_abs == B_abs -> false;
       true -> false
    end.

test(Pid) ->
    Message = "Hallo",
    Pid ! {message, Message}.

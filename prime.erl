-module(prime).
-compile(export_all).
-import(crypto, [strong_rand_bytes/1, rand_uniform/2]).

is_probably_prime(2) -> true;
is_probably_prime(N) when N < 2 -> false;
is_probably_prime(3) -> true;
is_probably_prime(5) -> true;
is_probably_prime(N) ->
    case is_even(N) of
	true -> false;
	false ->
	    {S, D} = sd(N-1, 0),
	    loop(40, N, S, D)
    end.

loop(0,_,_,_) ->
    true;
loop(K, N, S, D) ->
    A = 2 + rand_uniform(1,N-4),
    X = ecc:fpow(A,D,N),
    N1 = N - 1,
    case X of
	1  -> loop(K-1, N, S, D);
	N1 -> loop(K-1, N,S,D);
	_  -> inner(S-1, K, N, X, S, D)
    end.

inner(0, K, N, X, S, D) ->
    case N-1 of
	X -> loop(K-1, N, S, D);
	_ -> false
    end;
inner(R, K, N, X, S, D) ->
    X1 = ecc:fpow(X,2,N),
    N1 = N - 1,
    case X1 of
	1  -> false;
	N1 -> inner(0, K, N, X1, S, D);
	_  -> inner(R-1,K,N,X1,S,D)
    end.

sd(K, N) ->
    case is_even(K) of
	true  -> sd(K div 2, N+1);
	false -> {N, K}
    end.

is_even(K) ->
    K band 1 == 0.


spawnPrimes(Pid, Anz, Len) ->
	Proc = spawn_loop(Pid,Anz, Len, []),
	Proc.

spawn_loop(_,Anz, _,Proc) when Anz == 0 -> Proc;
spawn_loop(Pid,Anz,Len, Proc) ->
	X = spawn(prime, make_prime,[Len,Pid]),
    link(X),
	lists:append(Proc,[X]),
	Pid ! {proc, X},
    spawn_loop(Pid,Anz - 1, Len, lists:append(Proc, [X])).



%% Generiert eine sichere Primzahl
make_prime(K,Pid) when K > 0 ->
    new_seed(),
    N = make(K),
    if N > 3 ->
           %io:format("Generiere eine ~w stellige Primzahl ", [K]),
           MaxTries = N - 3,
           P1 = make_prime(MaxTries, N + rand:uniform(9), K),
           %io:format("~n", []),
           Pid ! {prime,P1},
           make_prime(K,Pid);
       true -> make_prime(K,Pid)
    end.

make_prime(0,_,Len) -> exit(max_tries_exceeded);
make_prime(K,P,Len) ->
    %io:format(".",[]),
    %case miller_rabin:is_prime(P) of
    case is_probably_prime(P) of
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
           case ecc:fpow(A,(N-1) div 2, N) of
               1 -> is_prime(Test - 1, N, Len);
               B -> is_prime(Test - 1, N,Len);
               _ -> false
            end;
        true -> is_prime(Test, N, Len)
    end.













-module(ecc).
-compile(export_all).

%%
euler_kriterium(C,P) ->
   Result = krypto:fpow(C,(P-1) div 2, P),
   N = P-1,
   case Result of
       1 -> true;
       N -> false;
       _ -> 0
    end.

mult_legendre(A,P) ->
   PFZ = krypto:primfz(A),
   mult_legendre(A,P,PFZ).

mult_legendre(A,P,PFZ) when length(PFZ) > 2 ->
    euler_kriterium(hd(PFZ),P) * mult_legendre(A,P,tl(PFZ));
mult_legendre(_,P,PFZ) ->
    euler_kriterium(hd(PFZ),P).


h1d(P) ->
    X = [X || X <- lists:seq(1,P), euler_kriterium(X,P) == true],
    comp_x(X,P,[]).

comp_x(X,P,R) when length(tl(X)) == 0 ->
    R ++ [ A || A <- lists:seq(1,P),trunc(math:pow(A,2)) rem P == hd(X)];

comp_x(X,P,R) ->
    comp_x(tl(X),P,R ++ [ A || A <- lists:seq(1,P),(trunc(math:pow(A,2)) rem P) == hd(X)]).



calc_legendre(A,M) ->
    case euler_kriterium(A,M) of
        1 -> h1d(M);
        -1 -> io:format("a ist ein quadratischer Nichtrest",[]);
        0 -> io:format("a|b \n",[])
    end.

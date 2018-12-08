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
    {NewInput,NewResult} = u_to_block(Input,K,0),
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


unblock(Input,_,Result) when length(Input) == 0  -> tl(Result);
unblock(Input, K, Result) ->
    Res = u_from_block(hd(Input), K, Result),
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


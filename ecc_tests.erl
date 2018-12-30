-module(ecc_tests).
-compile(export_all).

ublock_1_test() -> A = ecc:ublock("hallo",5,[]),
                   [104,97,108,108,111] = ecc:unblock(A,5,[]).
ublock_2_test() -> A = ecc:ublock("hallo",5,[]),
                   "hallo" = ecc:unblock(A,5,[]).
ublock_3_test() -> A = ecc:ublock("hallo",10,[]),
                   "hallo" = string:strip(ecc:unblock(A,10,[]),right).
ublock_4_test() -> A = ecc:ublock("hallo 123",10,[]),
                   "hallo 123" = string:strip(ecc:unblock(A,10,[]),right).

ublock_5_test() -> A = ecc:ublock("😂",10,[]),
                   "😂" = string:strip(ecc:unblock(A,10,[]), right).

ublock_6_test() -> String = "Agner Krarup Erlang (* 1. Januar 1878 in Lønborg, Dänemark; † 3. Februar 1929 in Kopenhagen) war ein dänischer Mathematiker und Ingenieur.",
                  A = ecc:ublock(String,100,[]),
                 String = string:strip(ecc:unblock(A,100,[]),right).


euler_kriterium_test() ->  false = ecc:euler_kriterium(7,13).
euler_kriterium_2_test() -> true = ecc:euler_kriterium(3,13).


euklid_test() -> {-8,-3} = ecc:euklid({73,0},{27,1}).

is_less_1_test() -> false = ecc:is_less({73,0},{42,1}).
is_less_2_test() -> true = ecc:is_less({41,0},{42,1}).
is_less_3_test() -> false = ecc:is_less({1,3},{1,2}).
is_less_4_test() -> true = ecc:is_less({1,0},{1,1}).


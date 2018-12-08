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



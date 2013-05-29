-module(prime_tester_tests).
-include_lib("eunit/include/eunit.hrl").

nr_1_is_not_prime_test() ->
	?assertEqual(not_prime, prime_tester:is_prime(1)).

nr_2_is_prime_test() ->
	?assertEqual(prime, prime_tester:is_prime(2)).

nr_198313_is_prime_test()->
	?assertEqual(prime, prime_tester:is_prime(198313)).

nr_10000000_is_not_prime_test() ->
	?assertEqual(not_prime, prime_tester:is_prime(10000000)).
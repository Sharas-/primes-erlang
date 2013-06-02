-module(number_utils_tests).
-include_lib("eunit/include/eunit.hrl").

nr_1_is_not_prime_test() ->
	?assertEqual(not_prime, number_utils:is_prime(1)).

nr_2_is_prime_test() ->
	?assertEqual(prime, number_utils:is_prime(2)).

nr_198313_is_prime_test()->
	?assertEqual(prime, number_utils:is_prime(198313)).

nr_10000000_is_not_prime_test() ->
	?assertEqual(not_prime, number_utils:is_prime(10000000)).

nr_999_has_odd_divisor_test() ->
	?assertEqual(true, number_utils:has_odd_divisor(999)).

nr_8_doesnt_have_odd_divisor_test() ->
	?assertEqual(false, number_utils:has_odd_divisor(8)).

make_2_odd_test() ->
	OddNr = number_utils:make_odd(2),
	?assertEqual(true, OddNr band 1 == 1).	
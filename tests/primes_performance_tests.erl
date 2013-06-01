-module(primes_performance_tests).
-include_lib("eunit/include/eunit.hrl").

nr_9876432000000000000000234_test() ->
	invoke_and_print_time(number_utils, is_prime, [9876432000000000000000234]).

nr_9100000000000876432000000000000000234_test() ->
	invoke_and_print_time(number_utils, is_prime, [9100000000000876432000000000000000234]).

find_primes_1_to_1000000_test() ->
	invoke_and_print_time(primes, find, [1, 1000000, fun(_) -> ok end]).

invoke_and_print_time(M, F, Args) ->
	{Elapsed, _} = timer:tc(M, F, Args),
	io:format(user, "~s:~s(~p) took ~f s~n", [M, F, Args, Elapsed/1000000]).


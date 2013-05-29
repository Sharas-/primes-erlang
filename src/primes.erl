%%%--------------------------------------------------------------------- 
%%% Facade for prime enumaration algorithm. 
%%%--------------------------------------------------------------------- 
-module(primes).
-export([find/3]).

-compile({nowarn_unused_function, [{find_primes_sequential, 3}, {find_primes_parallel, 3}, {start_prime_testers, 2}, {collect_results, 2}]}).

-spec find(pos_integer(), pos_integer(), fun((pos_integer()) -> any())) -> ok.
%%----------------------------------------------------------------------
%% Purpose:	Finds all primes in specified range.
%% Args:	From - range start (inclusive) 
%%			To - range end (inclusive)
%%			Callback - function to call when prime is found
%% Returns:	ok.
%%----------------------------------------------------------------------
find(From, To, Callback) ->
		OddFrom = case From < 3 of % Handling case when need to skip 1 as non prime and anounce 2 as prime
				 true ->
				 	Callback(2),
				 	3;
				 false ->
				 	make_odd(From) % Need to make "From" an odd number so that we can skip all even numbers when searching for primes
				end,
	find_primes_sequential(OddFrom, To, Callback).
	%find_primes_parallel(OddFrom, OddTo, Callback).

%-------------- private functions --------------------------------

%-------------- sequential algorithm --------------------------------

-spec find_primes_sequential(pos_integer(), pos_integer(), fun((pos_integer())-> any())) -> ok.
find_primes_sequential(From, To, _) when From > To ->
	ok;
find_primes_sequential(From, To, Callback) ->
	case prime_tester:is_prime(From) of
		prime -> Callback(From);
		_ -> nothing
	end,
	find_primes_sequential(From + 2, To, Callback). % incrementing by 2 to skip even numbers

%-------------- parallel algorithm --------------------------------

-spec find_primes_parallel(pos_integer(), pos_integer(), fun((pos_integer())-> any())) -> ok.
find_primes_parallel(From, To, Callback) ->
	ResultsCnt = trunc((To - From) / 2) + 1,
	start_prime_testers(From, To),
	collect_results(Callback, ResultsCnt).

-spec start_prime_testers(pos_integer(), pos_integer()) -> ok.
start_prime_testers(From, To) when From > To ->
	ok;
start_prime_testers(From, To) ->
	prime_tester:start_link(self(), From),
	start_prime_testers(From + 2, To). % incrementing by 2 to skip even numbers. 

-spec collect_results(fun((pos_integer()) -> any()), pos_integer()) -> ok.
collect_results(_, 0) ->
	ok;
collect_results(Callback, ResultsCount) ->
	receive 
		{prime, N} -> 
			Callback(N),
			collect_results(Callback, ResultsCount - 1);
		{not_prime, _} ->
			collect_results(Callback, ResultsCount - 1)
	end.

-spec make_odd(integer()) -> integer().
make_odd(Number) ->
	Number bor 1.
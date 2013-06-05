%%%--------------------------------------------------------------------- 
%%% @doc Facade for prime enumaration algorithm. 
%%%--------------------------------------------------------------------- 
-module(primes).
-export([find_primes/3, generate_primes/3]).

-compile({nowarn_unused_function, [{collect_results, 2}]}).

%%----------------------------------------------------------------------
%% Purpose:	Finds all primes in specified range.
%% Args:	From - range start (inclusive) 
%%			To - range end (inclusive)
%%			Callback - function to call when prime is found
%% Returns:	ok.
%%----------------------------------------------------------------------
-spec find_primes(pos_integer(), pos_integer(), fun((pos_integer()) -> any())) -> any().
find_primes(From, To, _) when From > To ->
	ok;
find_primes(From, To, Callback) when From < 3, To > 1 -> % case when range includes 2
	Callback(2), % anonce 2 as prime
	find_primes(3, To, Callback); % skip 1 by starting from 3
find_primes(From, To, Callback) ->
 	OddFrom = number_utils:make_odd(From),
	% --- sequential ---
	lazy:map(lazy:filter(lazy:range(OddFrom, To, 2), fun number_utils:is_prime/1), Callback).
	% --- parallel (naive) spanw prime tester for each odd number ---
	% lazy:map(lazy:range(OddFrom, To, 2), fun(Number) -> prime_tester:start_link(self(), Number) end),
	% WorkersCnt = trunc((To - OddFrom) / 2) + 1,
	% collect_results(Callback, WorkersCnt).

%%----------------------------------------------------------------------
%% Purpose:	Searches for specified number of primes.
%% Args:	From - number to start search from.
%% 			HowMany - how many primes to find.
%% 			Callback - function to call when prime is found.
%% Returns:	ok.
%----------------------------------------------------------------------
-spec generate_primes(pos_integer(), pos_integer(), fun((pos_integer()) -> any())) -> any().
generate_primes(_From, _HowMany, _Callback) when _HowMany > 0 ->
	ok.

%-------------- private functions --------------------------------

-spec collect_results(fun((pos_integer()) -> any()), pos_integer()) -> done.
collect_results(_, 0) ->
	done;
collect_results(Callback, ResultsCnt) ->
	receive 
		{prime, N} -> 
			Callback(N),
			collect_results(Callback, ResultsCnt - 1);
		{not_prime, _} ->
			collect_results(Callback, ResultsCnt - 1)
	end.
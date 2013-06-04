%%%--------------------------------------------------------------------- 
%%% @doc Facade for prime enumaration algorithm. 
%%%--------------------------------------------------------------------- 
-module(primes).
-export([find_primes/3, generate_primes/3]).

-compile({nowarn_unused_function, [{collect_results, 2}, {anounce_if_prime, 2}]}).

%%----------------------------------------------------------------------
%% Purpose:	Finds all primes in specified range.
%% Args:	From - range start (inclusive) 
%%			To - range end (inclusive)
%%			Callback - function to call when prime is found
%% Returns:	ok.
%%----------------------------------------------------------------------
-spec find_primes(pos_integer(), pos_integer(), fun((pos_integer()) -> any())) -> any().
find_primes(From, To, Callback) ->
		OddFrom = case From < 3 of % Handling case when need to skip 1 as non prime and anounce 2 as prime
				 true ->
				 	Callback(2),
				 	3;
				 false ->
				 	number_utils:make_odd(From)
				end,
	% --- sequential ---
	number_utils:iterate_odd_numbers(OddFrom, To, fun(Number) -> anounce_if_prime(Number, Callback) end),
	ok.
	% --- parallel ---
	% NrCnt = number_utils:iterate_odd_numbers(OddFrom, To, fun(Number) -> prime_tester:start_link(self(), Number) end),
	% collect_results(Callback, NrCnt),
	% ok.

%%----------------------------------------------------------------------
%% Purpose:	Searches for specified number of primes.
%% Args:	From - number to start search from.
%% 			HowMany - how many primes to find.
%% 			Callback - function to call when prime is found.
%% Returns:	ok.
%%----------------------------------------------------------------------
-spec generate_primes(pos_integer(), pos_integer(), fun((pos_integer()) -> any())) -> any().
generate_primes(From, HowMany, Callback) when HowMany > 0 ->
	generate_primes_impl(number_utils:prime_iterator(From), HowMany, Callback).

%-------------- private functions --------------------------------

-spec generate_primes_impl(fun(()-> {pos_integer, fun()}), pos_integer(), fun((pos_integer()) -> any())) -> any().
generate_primes_impl(_, 0, _) ->
	ok;
generate_primes_impl(PrimeIterator, HowMany, Callback) ->
	{NextPrime, PrimeIterator_} = PrimeIterator(),
	Callback(NextPrime),
	generate_primes_impl(PrimeIterator_, HowMany - 1, Callback).

-spec anounce_if_prime(pos_integer(), fun((pos_integer()) -> any())) -> {true, any()} | false.
anounce_if_prime(Number, Callback) ->
	case number_utils:is_prime(Number) of
		prime -> {true, Callback(Number)};
		not_prime -> false
	end.

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
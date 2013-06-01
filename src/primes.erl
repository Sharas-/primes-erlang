%%%--------------------------------------------------------------------- 
%%% Facade for prime enumaration algorithm. 
%%%--------------------------------------------------------------------- 
-module(primes).
-export([find/3]).

-compile({nowarn_unused_function, [{collect_results, 2}, {anounce_if_prime, 2}]}).

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
				 	number_utils:make_odd(From)
				end,
	% sequential
	% iterate_odd_numbers(OddFrom, To, fun(Number) -> anounce_if_prime(Number, Callback) end).
	% parallel
	iterate_odd_numbers(OddFrom, To, fun(Number) -> prime_tester:start_link(self(), Number) end),
	ResultsCnt = trunc((To - OddFrom) / 2),
	collect_results(Callback, ResultsCnt).
%-------------- private functions --------------------------------


iterate_odd_numbers(From, To, Callback) ->
	iterate_odd_numbers_impl(number_utils:make_odd(From), To , Callback).

iterate_odd_numbers_impl(From, To, _) when From > To ->
	ok;
iterate_odd_numbers_impl(From, To, Callback) ->
	Callback(From),
	iterate_odd_numbers_impl(From + 2, To, Callback).


anounce_if_prime(Number, Callback) ->
	case number_utils:is_prime(Number) of
		prime -> {true, Callback(Number)};
		not_prime -> false
	end.

%-------------- sequential algorithm --------------------------------

% -spec find_primes_sequential(pos_integer(), pos_integer(), fun((pos_integer())-> any())) -> ok.
% find_primes_sequential(From, To, _) when From > To ->
% 	ok;
% find_primes_sequential(From, To, Callback) ->
% 	case number_utils:is_prime(From) of
% 		prime -> Callback(From);
% 		_ -> nothing
% 	end,
% 	find_primes_sequential(From + 2, To, Callback). % incrementing by 2 to skip even numbers

%-------------- parallel algorithm --------------------------------


% -spec find_primes_parallel(pos_integer(), pos_integer(), fun((pos_integer())-> any())) -> ok.
% find_primes_parallel(From, To, Callback) ->
% 	ResultsCnt = trunc((To - From) / 2) + 1,
% 	start_prime_testers(From, To),
% 	collect_results(Callback, ResultsCnt).

% -spec start_prime_testers(pos_integer(), pos_integer()) -> ok.
% start_prime_testers(From, To) when From > To ->
% 	ok;
% start_prime_testers(From, To) ->
% 	prime_tester:start_link(self(), From),
% 	start_prime_testers(From + 2, To). % incrementing by 2 to skip even numbers. 

% -spec collect_results(fun((pos_integer()) -> any()), pos_integer()) -> ok.
% collect_results(_, 0) ->
% 	ok;
% collect_results(Callback, ResultsCount) ->
% 	receive 
% 		{prime, N} -> 
% 			Callback(N),
% 			collect_results(Callback, ResultsCount - 1);
% 		{not_prime, _} ->
% 			collect_results(Callback, ResultsCount - 1)
% 	end.

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

%%%--------------------------------------------------------------------- 
%%% @doc Convenience methods for prime number search
%%%--------------------------------------------------------------------- 

-module(number_utils).
-export([is_prime/1, make_odd/1, iterate_odd_numbers/3, odd_number_iterator/1, prime_iterator/1]).

%%----------------------------------------------------------------------
%% Purpose:	Tests a number for prime.
%% Args:	NumberToTest - number to be tested
%% Returns:	prime | not_prime
%%----------------------------------------------------------------------
-spec is_prime(pos_integer()) -> prime | not_prime.
is_prime(1) ->
	not_prime;
is_prime(2) -> 
	prime;
is_prime(NumberToTest) when NumberToTest band 1 == 0 ->
	not_prime;
is_prime(NumberToTest) ->
	case has_odd_divisor(NumberToTest) of
	 	true -> not_prime;
		false -> prime
	end.

%%----------------------------------------------------------------------
%% Purpose:	Makes number odd by adding one to it. If it is already odd just returns it.
%% Args:	Number - Number to be made odd
%% Returns:	Input number incremented by 1 if it was even. Otherwise returns imput number unchanged.
%%----------------------------------------------------------------------
-spec make_odd(integer()) -> integer().
make_odd(Number) ->
	Number bor 1.

%%----------------------------------------------------------------------
%% Purpose:	Calls back on odd numbers found in specified range. 
%% Args:	From - Range start (inclusive)
%% 			To - Range end (inclusive)
%% 			Callback - Function to call when odd number found
%% Returns:	Count of odd numbers in the range
%%----------------------------------------------------------------------
-spec iterate_odd_numbers(pos_integer(), pos_integer(), fun((pos_integer()) -> any())) -> pos_integer().
iterate_odd_numbers(From, To, Callback) ->
	iterate_odd_numbers_impl(make_odd(From), To, Callback, 0).

-spec iterate_odd_numbers_impl(pos_integer(), pos_integer(), fun((pos_integer())-> any()), pos_integer()) -> pos_integer().
iterate_odd_numbers_impl(From, To, _, Cnt) when From > To ->
	Cnt;
iterate_odd_numbers_impl(From, To, Callback, Cnt) ->
	Callback(From),
	iterate_odd_numbers_impl(From + 2, To, Callback, Cnt + 1).


-type iterator_step_return() :: {pos_integer, fun()}. 

%%----------------------------------------------------------------------
%% Purpose:	Iterates odd numbers in Erlngs' lazy evaluation style
%% Args:	StartNum - Number to start iterating from
%% Returns:	function that returns next odd number with next iteration lambda
%%----------------------------------------------------------------------
-spec odd_number_iterator(pos_integer) -> fun(() -> iterator_step_return()).
odd_number_iterator(StartNum) ->
	fun() -> next_odd_number(make_odd(StartNum)) end.

-spec next_odd_number(pos_integer) -> iterator_step_return().
next_odd_number(Number) ->
	{Number, fun() -> next_odd_number(Number + 2) end}.

%%----------------------------------------------------------------------
%% Purpose:	Iterates primes in Erlngs' lazy evaluation style
%% Args:	StartNum - Number to start iterating from
%% Returns:	function that returns next prime with next iteration lambda
-spec prime_iterator(pos_integer) -> fun(() -> iterator_step_return()).
prime_iterator(StartNum) ->
	fun() -> next_prime(odd_number_iterator(StartNum)) end.

-spec next_prime(pos_integer) -> iterator_step_return().
next_prime(NumIterator) ->
	{NextNum, Iterator} = NumIterator(),
	case is_prime(NextNum) of
		prime -> {NextNum, fun() -> next_prime(Iterator) end};
		not_prime -> next_prime(Iterator)
	end.


%--------------- private functions ---------------------------------

-spec has_odd_divisor(pos_integer()) -> boolean().
has_odd_divisor(Number) ->
	MaxDivisor = trunc(math:sqrt(Number)),
	has_odd_divisor_impl(Number, 3, MaxDivisor).	

-spec has_odd_divisor_impl(pos_integer(), pos_integer(), pos_integer()) -> boolean().
has_odd_divisor_impl(_, From, To) when From > To ->
	false;
has_odd_divisor_impl(Digit, From, _) when Digit rem From == 0 ->
	true;
has_odd_divisor_impl(Digit, From, To) ->
	has_odd_divisor_impl(Digit, From + 2, To). % increment by 2 to skip even divisors.
%%%--------------------------------------------------------------------- 
%%% @doc Convenience methods for prime number search
%%%--------------------------------------------------------------------- 

-module(number_utils).
-export([is_prime/1, make_odd/1]).

-spec is_prime(pos_integer()) -> prime | not_prime.
%%----------------------------------------------------------------------
%% Purpose:	Tests a number for prime.
%% Args:	NumberToTest - number to be tested
%% Returns:	prime | not_prime
%%----------------------------------------------------------------------
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
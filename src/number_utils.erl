%%%--------------------------------------------------------------------- 
%%% @doc Convenience methods for prime number search
%%%--------------------------------------------------------------------- 

-module(number_utils).
-export([is_prime/1, make_odd/1]).

%%----------------------------------------------------------------------
%% Purpose:	Tests a number for prime.
%% Args:	NumberToTest - number to be tested
%% Returns:	prime | not_prime
%%----------------------------------------------------------------------
-spec is_prime(pos_integer()) -> boolean().
is_prime(1) ->
	false;
is_prime(2) -> 
	true;
is_prime(NumberToTest) ->
	not is_composite(NumberToTest).

%%----------------------------------------------------------------------
%% Purpose:	Makes number odd by adding one to it. If it is already odd just returns it.
%% Args:	Number - Number to be made odd
%% Returns:	Input number incremented by 1 if it was even. Otherwise returns imput number unchanged.
%%----------------------------------------------------------------------
-spec make_odd(integer()) -> integer().
make_odd(Number) ->
	Number bor 1.

%--------------- private functions ---------------------------------

-spec is_composite(integer()) -> boolean().
is_composite(Number) when Number band 1 == 0 ->
	true;
is_composite(Number) ->
	has_odd_divisor(Number).


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
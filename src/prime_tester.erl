%%%--------------------------------------------------------------------- 
%%% Implementation of a prime tester worker process
%%%--------------------------------------------------------------------- 
-module(prime_tester).
-export([start_link/2, is_prime/1]).

% because of "unused" compiler error 
-export([is_prime/2]).

-spec start_link(pid(), pos_integer()) -> pid().
%%----------------------------------------------------------------------
%% Purpose:	Starts a worker process.
%% Args:	Pid - test result receivers' pid
%% 			NumberToTest - number to test for prime 
%% Returns:	ok.
%%----------------------------------------------------------------------
start_link(Pid, NumberToTest) ->
	erlang:spawn_link(?MODULE, is_prime, [Pid, NumberToTest]).

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
is_prime(NumberToTest) when NumberToTest rem 2 == 0 ->
	not_prime;
is_prime(NumberToTest) ->
	MaxDivisor = trunc(math:sqrt(NumberToTest)),
	case has_odd_divisor(NumberToTest, 3, MaxDivisor) of % first divisor is 3 because we already know it is not an even number.
	 	true -> not_prime;
		false -> prime
	end.

%-------------- private functions---------------------------

-spec is_prime(pid(), pos_integer()) -> {prime, pos_integer()} | {not_prime, pos_integer()}.
is_prime(Pid, NumberToTest) ->
	TestResult = is_prime(NumberToTest),
	Pid ! {TestResult, NumberToTest}.

-spec has_odd_divisor(pos_integer(), pos_integer(), pos_integer()) -> boolean().
has_odd_divisor(_, From, To) when From > To ->
	false;
has_odd_divisor(Digit, From, _) when Digit rem From == 0 ->
	true;
has_odd_divisor(Digit, From, To) ->
	has_odd_divisor(Digit, From + 2, To). % increment by 2 to skip even divisors.
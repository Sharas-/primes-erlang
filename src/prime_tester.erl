%%%--------------------------------------------------------------------- 
%%% @doc Implementation of a prime tester worker process
%%%--------------------------------------------------------------------- 
-module(prime_tester).
-export([start_link/2]).
-export([test_and_respond/2]).

%%----------------------------------------------------------------------
%% Purpose:	Starts a worker process.
%% Args:	Pid - test result receivers' pid
%% 			NumberToTest - number to test for prime 
%% Returns:	ok.
%%----------------------------------------------------------------------
-spec start_link(pid(), pos_integer()) -> pid().
start_link(Pid, NumberToTest) ->
	erlang:spawn_link(?MODULE, test_and_respond, [Pid, NumberToTest]).


%-------------- private functions---------------------------

-spec test_and_respond(pid(), pos_integer()) -> {prime, pos_integer()} | {not_prime, pos_integer()}.
test_and_respond(Pid, NumberToTest) ->
	TestResult = case number_utils:is_prime(NumberToTest) of
					true -> prime;
					false -> not_prime
				end,
	Pid ! {TestResult, NumberToTest}.
%%%--------------------------------------------------------------------- 
%%% @doc Lazy evaluation convenience functions.
%%% Due to Erlangs' lack of lazy evaluation. 
%%%--------------------------------------------------------------------- 

-module(lazy).
-export([range/2, range/3, infinite_range/2, filter/2, map/2, list/1]).

-type iterator(TValue):: {TValue, fun(() -> iterator(TValue))} | done.
%%----------------------------------------------------------------------
%% Purpose:	Creates lazy stream of numbers
%% Args:	From - stream start
%% 			To - stream end
%% 			Step - stream increment
%% Returns:	Stream iterator 
%%----------------------------------------------------------------------
-spec range(number(), number()) -> iterator(number()).
range(From, To) ->
	range(From, To, 1).

-spec range(number(), number(), number()) -> iterator(number()).
range(From, To, _) when From > To ->
	done;
range(From, To, Step) ->
	{From, fun() -> range(From + Step, To, Step) end}.

%%----------------------------------------------------------------------
%% Purpose:	Creates "infinite" lazy stream of numbers
%% Args:	From - stream start
%% 			Step - stream increment
%% Returns:	Stream iterator
%%----------------------------------------------------------------------
-spec infinite_range(number(), number()) -> iterator(number()).
infinite_range(From, Step) ->
 	{From, fun() -> infinite_range(From + Step, Step) end}.
%%----------------------------------------------------------------------
%% Purpose:	Creates lazy stream of items satisfying predicate
%% Args:	Stream iterator
%% 			Predicate - Filter function
%% Returns:	Stream iterator
%%----------------------------------------------------------------------
-spec filter(iterator(any()), fun((any()) -> boolean())) -> iterator(any()).
filter(done, _) ->
	done;
filter({Value, Iterator}, Predicate) ->
	case Predicate(Value) of
		true -> {Value, fun()-> filter(Iterator(), Predicate) end};
		false -> filter(Iterator(), Predicate)
	end.
%%----------------------------------------------------------------------
%% Purpose:	Iterates lazy stream and applies supplied function to its values
%% Args:	Stream iterator
%% 			Fun - Function to apply to streams' items
%% Returns:	done.
%%----------------------------------------------------------------------
-spec map(iterator(any()), fun((any()) -> any())) -> done.
map(done, _) ->
	done;
map({Value, Iterator}, Fun) ->
	Fun(Value),
	map(Iterator(), Fun).

%%----------------------------------------------------------------------
%% Purpose: Iterates lazy stream and returns its items in a list
%% Args:	Stream iterator
%% Returns:	List of streams values
%%----------------------------------------------------------------------
-spec list(iterator(any())) -> [any()].
list(done) ->
	[];
list({Value, Iterator}) ->
	[Value | list(Iterator())].
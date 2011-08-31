-module(trunkBin).
-include_lib("eunit/include/eunit.hrl").
-export([parse/1]).

%%% This module makes it possible to read trunketed external terms with no compression. 
%%%       


%%% parse() 
%%% 
%%% Parses both trunkated binaries and lists with binary values (i.e. binary_to_list).
%%%

parse(Trunk) when is_list(Trunk) ->
    parse(list_to_binary(Trunk));
parse(<<131, Rest/binary>>) ->
    case getTerm(Rest) of
	{error, _Msg, Term} ->
	    Term;
	{Term,<<>>} ->
	    Term
    end.

%%% getTerm()
%%%
%%% Parses ext terms.
%%%

%% smallInt
getTerm(<<97, Value, Rest/binary>>) ->
    {Value, Rest};

%% Int
getTerm(<<98, Value:32, Rest/binary>>) ->
    {Value, Rest};

%% Float
getTerm(<<99, _Float:31/binary, Rest/binary>>) ->
    {'_parser: FLOAT', Rest};

%% Atom
getTerm(<<100, Len:16, Atom:Len/binary, Rest/binary>>) ->
    {list_to_atom(binary_to_list(Atom)), Rest};

%% Pid
getTerm(<<103, 100, NodeLen:16, _Node:NodeLen/binary, _Serial:32, _ID:32,
	  _Creation, Rest/binary>>) ->
    {'_parser: PID', Rest};

%% smallTuple
getTerm(<<104, Arity, Rest/binary>>) ->
    trav(Rest, Arity, tuple);

%% largeTuple
getTerm(<<105, Arity:32, Rest/binary>>) ->
    trav(Rest, Arity, tuple);

%% Nil
getTerm(<<106, Rest/binary>>) ->
    {[], Rest};

%% String 
getTerm(<<107, Len:16, String:Len/binary, Rest/binary>>) ->
    {binary_to_list(String), Rest};

%% List 
getTerm(<<108,Arity:32, Rest/binary>>) ->
    trav(Rest, Arity+1, list);

%% Binary - not yet sure if this patternmatch is relevant
getTerm(<<109,0,0,0,0, Rest/binary>>) ->
   {<<>>, Rest};

getTerm(<<109, Len:32, Data:Len/binary, Rest/binary>>) ->
    {Data, Rest};

%% smallBig
getTerm(<<110,Len, _Sign, _BigNum:Len/binary, Rest/binary>>) ->
    {'_parser: BIGNUM', Rest};

%% newRef
getTerm(<<114, Len:16, 100, NodeLen:16, _Node:NodeLen/binary, 
	  _Creation, Data/binary>>) ->
    NewLen = Len * 4,
    <<_ID:NewLen/binary, Rest/binary>> = Data,
    {'_parser: NEWREF', Rest};

%% terms that can not be parsed
getTerm(Unknown) ->
    <<Tag, _Rest/binary>> = Unknown,
    TrunkTag = 
	case Tag of
	    97 -> "SMALLINT";
	    98 -> "INT";
	    99 -> "FLOAT";
	    100 -> "ATOM";
	    103 -> "PID";
	    104 -> "SMALLTUPLE";
	    105 -> "LARGETUPLE";
	    106 -> "NIL";
	    107 -> "STRING";
	    108 -> "LIST";
	    109 -> "BIN";
	    110 -> "SMALLBIG";
	    114 -> "NEWREF";
	    _Other -> "TRUNK"
	end,
    {list_to_atom("_TRUNKATED_" ++ TrunkTag), <<>>}.

%%% trav() 
%%%
%%% Goes through structures (i.e. tuples and lists).
%%%

trav(Binary, Arity, Type) ->
    trav(Binary, Arity, [], Type).

trav(<<>>, Arity, Acc, Type) when (Arity /= 0) ->
    case Type of
	tuple ->
	    {error, {arity_error, Arity}, list_to_tuple(Acc ++ ['_PARSE_ERROR'])};
	list ->
	    {error, {arity_error, Arity}, Acc ++ ['_PARSE_ERROR']}
    end;

trav(Binary, 0, Acc, Type) -> 
    case Type of
	tuple -> 
	    {list_to_tuple(Acc), Binary};
	list ->
	    {Acc, Binary}
    end;

trav(Binary, Arity, GlobAcc, Type) ->
    case getTerm(Binary) of
	{error, {arity_error, _GlobArity}, Acc} ->
	    case Type of
		tuple ->
		    {error, {arity_error, Arity}, 
		     list_to_tuple(GlobAcc ++ [Acc, '_PARSE_ERROR'])};
		list ->
		    {error, {arity_error, Arity}, GlobAcc ++ [Acc, '_PARSE_ERROR']}
	    end;
	{Term, Rest} -> 
	    trav(Rest, Arity - 1, GlobAcc ++ [Term], Type)
    end.

%%% Simple eunit tests
%%%
%%%

getTerm_test_() -> 
    [?_assert(getTerm(<<97,1>>) =:=
		  {1, <<>>}),
     ?_assert(getTerm(<<104,2,97,1,97,2>>) =:= 
		  {{1,2}, <<>>}),
     ?_assert(getTerm(<<104,3,97,1,97,2>>) =:=
		  {error, {arity_error, 1}, {1,2, parse_error}}),
     ?_assert(getTerm(<<104, 1, 104,2,97,1>>) =:=
		  {error, {arity_error,1},{{1, parse_error}, parse_error}}),
     ?_assert(getTerm(<<104,2,104,1,97,1,104,3,97,1,97,2>>) =:=
		  {error, {arity_error, 1}, 
		   {{1},{1,2,parse_error},parse_error}}),
     ?_assert(getTerm(<<105,0,0,0,2, 97,1>>) =:= 
		  {error, {arity_error, 1}, {1, parse_error}})].

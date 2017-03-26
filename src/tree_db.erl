%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2015, Tony Rogvall
%%% @doc
%%%    Tree data base
%%% @end
%%% Created : 19 Oct 2015 by Tony Rogvall <tony@rogvall.se>

-module(tree_db).

-export([new/1, lookup/2, insert/2, delete/1, delete/2, clear/1,
	 first/1, last/1, next/2, prev/2,
	 first_child/2, last_child/2, next_sibling/2, prev_sibling/2,
	 foldl/3, foldr/3, foldbl/3, foldbr/3,
	 depth_first/3, breadth_first/3]).
-export([put/3, put/4, get/2, get_ts/2]).
-export([update_counter/3, update_timestamp/2, update_timestamp/3]).
-export([fold_matching/4, foldl_matching/4, foldr_matching/4]).
-export([from_list/2, to_list/1, to_list/2]).
-export([subscribe/3, publish/3]).

-export([enql/3, enqr/3]).
-export([match_keys/2]).
-export([internal_key/1, external_key/1, pattern_key/1]).
-export([is_pattern_key/1, is_prefix_key/1]).

-define(QUEUE_T(), term()).  %% R15 !

-define(eot, '$end_of_table').
-define(top, <<>>).
-define(is_external_key(Key), is_binary((Key))).
-define(is_internal_key(Key), (((Key) =:= []) 
			       orelse 
				 (is_list((Key)) andalso is_atom(hd((Key)))))).

-type eot() :: '$end_of_table'.
-type internal_key() :: [atom()|integer()].
-type external_key() :: binary() | string().
-type pattern_key()  :: [atom()|integer()|'*'|'?'].
-type key() :: internal_key() | external_key().
-type table() :: term().
-type timestamp() :: integer().
%% fold functions get internal key format as input, convert with
%% external_key(Key) if needed.
-type foldfunc() :: fun(({Key::internal_key(),Value::term()},Acc::term) ->
			       term()).

-spec new(Name::atom()) -> table().
new(Name) ->
    case tree_db_srv:start(Name) of
	{ok,_Pid} ->
	    Name
    end.

from_list(Name, List) ->
    T = new(Name),
    lists:foreach(
      fun({K,V}) ->
	      put(T, K, V)
      end, List),
    T.

to_list(T) ->
    to_list(T, "*").

to_list(T, Match) ->
    foldr_matching(T, Match,
		   fun({K,V},Acc) ->
			   [{binary_to_list(external_key(K)), V}|Acc]
		   end, []).

-spec delete(Name::atom()) -> true.
delete(Name) when is_atom(Name) ->
    tree_db_srv:stop(Name).

-spec clear(Name::atom()) -> true.
clear(Name) when is_atom(Name) ->
    ets:delete_all_objects(Name).

-spec insert(Table::table(), {Key::key(),Value::term()}) -> true.
insert(Table, {Key,Value}) ->
    insert_(Table,internal_key(Key),Value).

insert_(Table, Key, Value) ->
    ets:insert(Table, {Key,Value,timestamp()}).

-spec lookup(Table::table(), Key::key()) ->
		    [{Key::key(),Value::term()}].

lookup(Table, K) when ?is_internal_key(K) ->
    case ets:lookup(Table,K) of
	[{K,Value,_Ts}] ->
	    [{K,Value}];
	[] ->
	    []
    end;
lookup(Table, Key) ->  %% external key, return external key
    case ets:lookup(Table, internal_key(Key)) of
	[] -> [];
	[{_,Value,_Ts}] -> [{Key,Value}]
    end.

-spec put(Table::table(), Key::key(),Value::term()) -> true.
put(T, Key, Val) ->
    put(T, Key, Val, timestamp()).

-spec put(Table::table(), Key::key(),Value::term(),Ts::timestamp()) -> true.
put(T, Key, Val, Ts) ->
    ets:insert(T, {internal_key(Key),Val,Ts}).

-spec get(Table::table(), Key::key()) -> Value::term().
get(T, Key) ->
    case ets:lookup(T,internal_key(Key)) of
	[] -> error({badkey,Key});
	[{_,Val,_Ts}] -> Val
    end.

-spec get_ts(Table::table(), Key::key()) -> {Value::term(),Ts::timestamp()}.
get_ts(T, Key) ->
    case ets:lookup(T,internal_key(Key)) of
	[] -> error({badkey,Key});
	[{_,Val,Ts}] -> {Val,Ts}
    end.

-spec update_counter(Table::table(), Key::key(), Inc::term()) ->
			    Value::term().
update_counter(T, Key, I) ->
    K = internal_key(Key),
    hd(ets:update_counter(T, K, [{2,I},{3,1,0,timestamp()}])).

-spec update_timestamp(Table::table(), Key::key()) -> true.
update_timestamp(T, Key) ->
    update_timestamp(T, Key, timestamp()).

-spec update_timestamp(Table::table(), Key::key(), Ts::timestamp()) -> true.
update_timestamp(T, Key, Ts) ->
    K = internal_key(Key),
    ets:update_element(T, K, {3, Ts}).

-spec delete(Table::table(), Key::key()) -> true.
delete(T, K) -> ets:delete(T, internal_key(K)).

-spec first(table()) -> internal_key() | eot().
first(T) -> ets:first(T).

-spec last(table()) -> internal_key() | eot().
last(T) -> ets:last(T).

-spec next(table(), key()) -> internal_key() | eot().
next(T,K) -> ets:next(T, internal_key(K)).

-spec prev(table(), key()) -> internal_key() | eot().
prev(T,K) -> ets:prev(T, internal_key(K)).

-spec first_child(table(), key()) -> internal_key() | eot().
first_child(Table,Parent) ->
    first_child_(Table,internal_key(Parent)).

first_child_(Table,Parent) ->
    case ets:next(Table, Parent) of
	?eot -> ?eot;
	Child ->
	    case is_parent(Parent, Child) of
		{true, FirstChild} -> FirstChild;
		false -> ?eot
	    end
    end.

-spec last_child(table(), key()) -> internal_key() | eot().
last_child(Table,Parent0) ->
    Parent = internal_key(Parent0),
    case ets:prev(Table, Parent++[?top]) of
	?eot -> ?eot;
	Child ->
	    case is_parent(Parent, Child) of
		{true,LastChild} -> LastChild;
		false -> ?eot
	    end
    end.

-spec next_sibling(table(), key()) -> internal_key() | eot().
next_sibling(Table,Child) ->
    next_sibling(Table,internal_key(Child)).

next_sibling_(Table,Child) ->
    case ets:next(Table,Child++[?top]) of
	?eot -> ?eot;
	Child2 ->
	    case is_sibling(Child,Child2) of
		{true,Sibling} -> Sibling;
		false -> ?eot
	    end
    end.

-spec prev_sibling(table(), key()) -> internal_key() | eot().
prev_sibling(Table,Child0) ->
    Child = internal_key(Child0),
    case ets:prev(Table,Child) of
	?eot -> ?eot;
	Child2 ->
	    case is_sibling(Child,Child2) of
		{true,Sibling} -> Sibling;
		false -> ?eot
	    end
    end.

%% is_parent is true if Parent is a Prefix of Child
%% [a,b] is a parent of [a,b,c,d]  and {true,[a,b,c]} is returned
is_parent(Parent, Child) ->
    is_parent_(Parent, Child, []).

is_parent_([H|Parent], [H|Child], NewChild) ->
    is_parent_(Parent, Child, [H|NewChild]);
is_parent_([], [H|_Child], NewChild) ->
    {true, lists:reverse(NewChild,[H])};
is_parent_(_, _, _) ->
    false.

%% is_sibling is true if Child1 is a sibling to Child2
%% example: [a,b,c] is a sibling of [a,b,d,e]  and {true,[a,b,d]} is returned
is_sibling(Child1, Child2) ->
    is_sibling_(Child1, Child2, []).

is_sibling_([H|Child1], [H|Child2], Parent) ->
    is_sibling_(Child1, Child2, [H|Parent]);
is_sibling_([H1], [H2|_], Parent) when H1 =/= H2 ->
    {true, lists:reverse(Parent,[H2])};
is_sibling_(_, _, _) ->
    false.

-spec fold_matching(Table::table(), Pattern::external_key(),
		    Fun::foldfunc(), Acc::term()) -> term().

fold_matching(Table, Pattern, Func, Acc) ->
    foldl_matching(Table, Pattern, Func, Acc).

-spec foldl_matching(Table::table(), Pattern::external_key(),
		     Fun::foldfunc(), Acc::term()) -> term().

foldl_matching(Table, Pattern, Func, Acc) ->
    PatternKey = pattern_key(Pattern),
    Parent = fixed_prefix(PatternKey),
    Q = enql(Table,Parent,queue:new()),
    foldbl_(Table,
	    fun(Elem={Key,_Value}, Acci) ->
		    case match_ikeys(PatternKey, Key) of
			true -> Func(Elem,Acci);
			false -> Acci
		    end
	    end, Acc, Q).

-spec foldr_matching(Table::table(), Pattern::external_key(),
		     Fun::foldfunc(), Acc::term()) -> term().

foldr_matching(Table, Pattern, Func, Acc) ->
    PatternKey = pattern_key(Pattern),
    Parent = fixed_prefix(PatternKey),
    Q = enqr(Table,Parent,queue:new()),
    foldbr_(Table,
	    fun(Elem={Key,_Value}, Acci) ->
		    case match_ikeys(PatternKey, Key) of
			true -> Func(Elem,Acci);
			false -> Acci
		    end
	    end, Acc, Q).

%% depth first traversal
-spec depth_first(Table::table(), Fun::foldfunc(), Acc::term()) -> term().
depth_first(Table,Func,Acc) ->
    foldl(Table,Func,Acc).

-spec breadth_first(Table::table(), Fun::foldfunc(), Acc::term()) -> term().
breadth_first(Table,Func,Acc) ->
    foldbl(Table,Func,Acc).
    
%% Fold over the tree depth first left to right
-spec foldl(Table::table(), Fun::foldfunc(), Acc::term()) -> term().

foldl(Table,Func,Acc) ->
    foldl_(Table,Func,Acc,first(Table)).

foldl_(_Table,_Func,Acc,?eot) ->
    Acc;
foldl_(Table,Func,Acc,K) ->
    [Elem] = lookup(Table,K),
    Acc1 = Func(Elem,Acc),
    foldl_(Table,Func,Acc1,ets:next(Table,K)).

%% Fold over the tree depth first right to left
-spec foldr(Table::table(), Fun::foldfunc(), Acc::term()) -> term().

foldr(Table,Func,Acc) ->
    foldr_(Table,Func,Acc,last(Table)).

foldr_(_Table,_Func,Acc,?eot) ->
    Acc;
foldr_(Table,Func,Acc,K) ->
    [Elem] = lookup(Table,K),
    Acc1 = Func(Elem,Acc),
    foldr_(Table,Func,Acc1,ets:prev(Table,K)).

%% Fold over the tree breadth first left to right
-spec foldbl(Table::table(), Fun::foldfunc(), Acc::term()) -> term().

foldbl(Table,Func,Acc) ->
    Q = enql(Table,[],queue:new()),
    foldbl_(Table,Func,Acc,Q).

foldbl_(Table,Func,Acc,Q) ->
    case queue:out(Q) of
	{{value,K},Q1} ->
	    Q2 = enql(Table,K,Q1),
	    case lookup(Table,K) of
		[Elem] ->
		    Acc1 = Func(Elem,Acc),
		    foldbl_(Table,Func,Acc1,Q2);
		[] ->
		    foldbl_(Table,Func,Acc,Q2)
	    end;
	{empty,_Q1} ->
	    Acc
    end.

%% Fold over the tree breadth first right to left
-spec foldbr(Table::table(), Fun::foldfunc(), Acc::term()) -> term().

foldbr(Table,Func,Acc) ->
    Q = enqr(Table,[],queue:new()),
    foldbr_(Table,Func,Acc,Q).

foldbr_(Table,Func,Acc,Q) ->
    case queue:out(Q) of
	{{value,K},Q1} ->
	    Q2 = enqr(Table,K,Q1),	    
	    case lookup(Table,K) of
		[Elem] ->
		    Acc1 = Func(Elem,Acc),
		    foldbr_(Table,Func,Acc1,Q2);
		[] ->
		    foldbr_(Table,Func,Acc,Q2)
	    end;
	{empty,_Q1} ->
	    Acc
    end.

%% enqueue children left to right
-spec enql(Table::table(),Key::key(),Queue::?QUEUE_T()) ->
		  NewQueue::?QUEUE_T().
enql(Table,K,Q) ->
    enql_(Table,first_child(Table,K), Q).

enql_(_Table,?eot,Q) ->
    Q;
enql_(Table,K,Q) ->
    enql_(Table,next_sibling(Table,K), queue:in(K, Q)).

%% enqueue children right to left
-spec enqr(Table::table(),Key::key(),Queue::?QUEUE_T()) ->
		  NewQueue::?QUEUE_T().
enqr(Table,K,Q) ->
    enqr_(Table,last_child(Table,K), Q).

enqr_(_Table,?eot,Q) ->
    Q;
enqr_(Table,K,Q) ->
    enqr_(Table,prev_sibling(Table,K), queue:in(K, Q)).

-spec subscribe(Table::table(),Pattern::key(),
		CallbackOrPid::pid()|{atom(),atom()}|function()) ->
		       true.

subscribe(Table, Topic,CallbackOrPid) ->
    Key = internal_key(Topic),
    Key1 = Key++['$',erlang:unique_integer()],
    insert_(Table,Key1,CallbackOrPid).

-spec publish(Table::table(), Topic::key(), Value::term()) -> ok.

publish(Table, Topic, Value) ->
    Key = internal_key(Topic),
    case Key of
	[] ->
	    publish_leaf_(Table,[],Key,Value);
	_ ->
	    Q = push_star_node(Table,[],[[]]),
	    publish_pat_(Table,Key,Q,[],Key,Value)
    end.

%% Do breadth first search pattern scan
publish_pat_(Table,[_],[],Q,Key,Value) ->
    IN = lists:usort(Q),
    lists:foreach(fun(Node) -> publish_leaf_(Table,Node,Key,Value) end, IN);
publish_pat_(Table,[_|Ks],[],Q,Key,Value) ->
    IN = lists:usort(Q),
    publish_pat_(Table,Ks,IN,[],Key,Value);
publish_pat_(Table,Ks0=[K|_],[N|IN],Q0,Key,Value) ->
    Q1 = keep_star_node(N,Q0),
    Q2 = push_key_node(Table,N,K,Q1),
    Q3 = push_one_node(Table,N,Q2),
    publish_pat_(Table,Ks0,IN,Q3,Key,Value).

is_star_node(N) ->
    case lists:reverse(N) of
	['*'|_] -> true;
	_ -> false
    end.

keep_star_node(N,Q) ->
    case is_star_node(N) of
	true -> [N|Q];
	false -> Q
    end.
    
%% walk possible zero transitions
%% pattern: '*' ['*']*
push_star_node(Table,N,Q) ->
    N1 = N++['*'],
    case first_child_(Table,N1) of
	'$end_of_table' -> Q;
	_ -> push_star_node(Table,N1,[N1|Q])
    end.

%% walk a key K and follow up with zero transitions
%% pattern: K ['*']*
push_key_node(Table,N,K,Q) ->
    N1 = N++[K],
    case first_child_(Table,N1) of
	'$end_of_table' -> Q;
	_ -> push_star_node(Table,N1,[N1|Q])
    end.

%% walk possible one nodes, and add possible zero transitions
%% pattern: '?' ['*']*
push_one_node(Table,N,Q) ->
    N1 = N++['?'],
    case first_child_(Table,N1) of
	'$end_of_table' -> Q;
	_ -> push_star_node(Table,N1,[N1|Q])
    end.

%% publish to subscribers that subscribe on the direct key
publish_leaf_(Table,Node,Key,Value) ->
    case first_child_(Table,Node++['$']) of
	'$end_of_table' -> ok;
	Child -> publish_leaf_list_(Table,Child,Key,Value)
    end.

%% traverse all subscribers 
publish_leaf_list_(Table,Child,Key,Value) ->
    case lists:reverse(Child) of
	[N,'$'|Cs] when is_integer(N) ->
	    Topic = lists:reverse(Cs),
	    case lookup(Table,Child) of
		[{_, Sub}] when is_pid(Sub) ->
		    Sub ! {Topic,Key,Value};
		[{_, Fun}] when is_function(Fun);
				is_atom(element(1,Fun)),
				is_atom(element(2,Fun)) ->
		    apply(Fun,[Topic,Key,Value]);
		[] -> ok
	    end;
	_ ->
	    ok
    end,
    case next_sibling_(Table, Child) of
	'$end_of_table' -> ok;
	Next -> publish_leaf_list_(Table,Next,Key,Value)
    end.


%% Set key to it's internal form
%% to_internal(Pos,Elem) ->
%%    Key = internal_key(element(Pos, Elem)),
%%    setelement(Pos,Elem,Key).

%% Set key to it's external form
%% to_external(Pos,Elem) ->
%%    Key = external_key(element(Pos, Elem)),
%%    setelement(Pos,Elem,Key).

-spec match_keys(Pattern::external_key(), Key::key()) ->
			boolean().
match_keys(Pattern, Key) ->
    M = pattern_key(Pattern),
    K = internal_key(Key),
    match_ikeys(M, K).

match_ikeys(M, K) ->
    case match(M,K) of
	fail -> false;
	R -> R
    end.

match(['*'], _) -> true;
match(['*'|Ms],Ks) -> match__(Ms,Ks);
match(['?'|Ms],[_|Ks]) -> match(Ms,Ks);
match([K|Ms],[K|Ks]) -> match(Ms,Ks);
match([_],[_]) -> false;
match([],[]) -> true;
match(_, _) -> fail.

match_(['*'|Ms],Ks) -> match__(Ms,Ks);
match_(['?'|Ms], Ks) -> match_(Ms, Ks);
match_([K|Ms], Ks) ->
    case match_drop(K, Ks) of
	false -> false;
	Ks1 -> match(Ms,Ks1)
    end;
match_([], []) -> true;
match_(_, _) -> fail.

match__(Ms,Ks) ->
    %% io:format("match__: ms=~w, ks=~w\n", [Ms,Ks]),
    case lists:member('*', Ms) orelse lists:member('?', Ms) of
	false ->
	    lists:suffix(Ms, Ks);
	true ->
	    case match_(Ms, Ks) of
		fail when Ks =/= [] ->
		    match__(Ms, tl(Ks));
		R -> R
	    end
    end.

match_drop(K, [K|Ks]) -> Ks;
match_drop(K, [_|Ks]) -> match_drop(K,Ks);
match_drop(_K, []) -> false.

%%
%% @doc 
%%    Check if key contains * or ? in the pattern
%%
-spec is_pattern_key(Key::internal_key() | external_key()) ->
			    boolean().

is_pattern_key(Key) ->
    is_pattern_path(pattern_key(Key)).

is_pattern_path(['*'|_]) -> true;
is_pattern_path(['?'|_]) -> true;
is_pattern_path([_|As]) -> is_pattern_path(As);
is_pattern_path([]) -> false.

%%
%% @doc 
%%    Check if key ends in * or ?
%%
-spec is_prefix_key(Key::internal_key() | external_key()) ->
			   boolean().

is_prefix_key(Key) ->
    is_prefix_path(pattern_key(Key)).

is_prefix_path([]) -> false;
is_prefix_path(Key) -> is_prefix_path_(Key).
    
is_prefix_path_(['*']) -> true;
is_prefix_path_(['?']) -> true;
is_prefix_path_(['*'|_]) -> false;
is_prefix_path_(['?'|_]) -> false;
is_prefix_path_([_|As]) -> is_prefix_path_(As);
is_prefix_path_([]) -> false.
    
%%
%% Match keys are in the form of normal keys or:
%% allow * in place of components
%% "a.*.c"
%% "a.?"
%% "a.b.2.*"
%% convert a string or a binary into a internal key

-spec pattern_key(Key::internal_key() | external_key()) ->
			 pattern_key().
pattern_key(Key) when ?is_internal_key(Key) ->
    Key;
pattern_key(Name) when is_binary(Name) ->
    ipath(binary:split(Name, <<".">>, [global]),true);
pattern_key(Name) when is_list(Name) ->
    ipath(binary:split(iolist_to_binary(Name),<<".">>,[global]),true).

%%
%% External keys are in the form 
%% "a.b.c"
%% "a.2"
%% "a.b.2.c"
%% convert a string or a binary into a internal key
internal_key(Key) when ?is_internal_key(Key) ->
    Key;
internal_key(Name) when is_binary(Name) ->
    ipath(binary:split(Name, <<".">>, [global]),false);
internal_key(Name) when is_list(Name) ->
    ipath(binary:split(iolist_to_binary(Name),<<".">>,[global]),false).

ipath([P|Ps],Match) ->
    case P of
	<<"*">> when Match -> 
	    [ '*' | ipath(Ps,Match)];
	<<"?">> when Match ->
	    [ '?' | ipath(Ps,Match)];
	P = <<C,_/binary>> when C >= $0, C =< $9 ->
	    try bin_to_integer(P) of
		I -> [I | ipath(Ps,Match)]
	    catch
		error:_ -> error(bad_key)
	    end;
	P -> %% existing atom?
	    [ binary_to_atom(P, latin1) | ipath(Ps,Match) ]
    end;
ipath([],_Match) ->
    [].

bin_to_integer(Bin) -> %% R15 support
    list_to_integer(binary_to_list(Bin)).

external_key(Key) when ?is_internal_key(Key) ->
    iolist_to_binary(join(xpath(Key), $.)).

%% convert to an extern id
xpath([K|Ks]) when is_atom(K) ->
    [atom_to_list(K) | xpath(Ks)];
xpath([I|Ks]) when is_integer(I) ->
    [integer_to_list(I)|xpath(Ks)];
xpath([]) ->
    [].

join([],_S) -> [];
join([A],_S) -> [A];
join([A|As],S) -> [A,S|join(As,S)].

%% get the fixed key pattern prefix if any
fixed_prefix(['*'|_]) -> [];
fixed_prefix(['?'|_]) -> [];
fixed_prefix([K|Ks]) -> [K|fixed_prefix(Ks)];
fixed_prefix([]) -> [].

timestamp() ->
    try erlang:system_time(micro_seconds)
    catch
	error:undef ->
	    {MS,S,US} = os:timestamp(),
	    (MS*1000000+S)*1000000+US
    end.

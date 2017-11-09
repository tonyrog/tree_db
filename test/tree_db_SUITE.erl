%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2016, Tony Rogvall
%%% @doc
%%%    Test suite for tree_db
%%% @end
%%% Created : 10 Mar 2016 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(tree_db_SUITE).


%-include_lib("test_server/include/test_server.hrl").


-ifdef(line_trace).
-line_trace(true).
-define(line,
	put(test_server_loc,{?MODULE,?LINE}),
	io:format(lists:concat([?MODULE,",",integer_to_list(?LINE),": ~p"]),
		  [erlang:now()]),).
-else.
-define(line,put(test_server_loc,{?MODULE,?LINE}),).
-endif.

-export([init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         all/0, all/1,
         suite/0,
         a_test_case/1]).


-export([tree/1, match_success/1, match_failure/1, pubsub/1]).

%%--------------------------------------------------------------------
%% TEST SERVER CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

suite() -> [].

all() ->
    [tree, match_success, match_failure, pubsub].

%%--------------------------------------------------------------------
%%
%% @doc
%% Initialization before the suite.
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the suite.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%
%% @spec init_per_suite(Config) -> Config
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Config.

%%--------------------------------------------------------------------
%% @doc
%% Cleanup after the suite.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% @spec end_per_suite(Config) -> _
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Initialization before each test case
%%
%% TestCase - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the test case.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%
%% @spec init_per_testcase(TestCase, Config) -> Config
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @doc
%% Cleanup after each test case
%%
%% TestCase = atom()
%%   Name of the test case that is finished.
%% Config = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% @spec end_per_testcase(TestCase, Config) -> _
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Returns a description of the test suite when
%% Clause == doc, and a test specification (list
%% of the conf and test cases in the suite) when
%% Clause == suite.
%% Returns a list of all test cases in this test suite
%%
%% Clause = doc | suite
%%   Indicates expected return value.
%% Descr = [string()] | []
%%   String that describes the test suite.
%% Spec = [TestCase]
%%   A test specification.
%% TestCase = ConfCase | atom()
%%   Configuration case, or the name of a test case function.
%% ConfCase = {conf,Init,Spec,End} |
%%            {conf,Properties,Init,Spec,End}
%% Init = End = {Mod,Func} | Func
%%   Initialization and cleanup function.
%% Mod = Func = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%%   Execution properties of the test cases (may be combined).
%% Shuffle = shuffle | {shuffle,Seed}
%%   To get cases executed in random order.
%% Seed = {integer(),integer(),integer()}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%%   To get execution of cases repeated.
%% N = integer() | forever
%% Reason = term()
%%   The reason for skipping the test suite.
%%
%% @spec all(Clause) -> TestCases
%% @end
%%--------------------------------------------------------------------
all(doc) ->
    ["Describe the main purpose of this suite"];

all(suite) ->
    [tree, match_success, match_failure, pubsub].


%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

tree(doc) ->
    ["Test basic tree properties"];
tree(suite) -> [];
tree(Config) when is_list(Config) ->
    ?line _T = tree_db:from_list(tree, [{"a.b.x", 1}, {"a.b.y", 2}, {"a.b.z", 3}, {"b.c.x", 4}]),
    ?line [{[b,c,x],4}] = tree_db:foldl_matching(tree, "b.c.*", fun(E,A) -> [E|A] end, []),
    ?line [{[a,b,x],1},{[a,b,y],2},{[a,b,z],3}] = lists:sort(tree_db:foldl_matching(tree, "a.b.?", fun(E,A) -> [E|A] end, [])),
    ok.



match_success(doc) ->
    ["A number of sucessful matches"];
match_success(suite) -> [];
match_success(Config) when is_list(Config) ->
    ?line true = tree_db:match_keys("*", "a.b.c.a.b.c.a.b.c"),
    ?line true = tree_db:match_keys("a.*", "a.b.c.a.b.c.a.b.c"),
    ?line true = tree_db:match_keys("*.c", "a.b.c.a.b.c.a.b.c"),
    ?line true = tree_db:match_keys("a.*.c", "a.b.c.a.b.c.a.b.c"),
    ?line true = tree_db:match_keys("a.*.a.b.*.c", "a.b.c.a.b.c.a.b.c"),
    ?line true = tree_db:match_keys("a.b.*.c.d.*", "a.b.2.c.d.3"),
    ?line true = tree_db:match_keys("a.b.2.c.d.*", "a.b.2.c.d.3"),
    ok.

match_failure(doc) ->
    ["A number of failed matches"];
match_failure(suite) -> [];
match_failure(Config) when is_list(Config) ->
    ?line false = tree_db:match_keys("*.b", "a.b.c.a.b.c.a.b.c"),
    ?line false = tree_db:match_keys("b.*", "a.b.c.a.b.c.a.b.c"),
    ?line false = tree_db:match_keys("*.a.c.*", "a.b.c.a.b.c.a.b.c"),
    ?line false = tree_db:match_keys("*.a.b.a.*.c", "a.b.c.a.b.c.a.b.c"),
    ok.

pubsub(doc) ->
    ["Test pubsub patterns"];
pubsub(suite) -> [];
pubsub(Config) when is_list(Config) ->
    T = tree_db:new(test_a),
    ?line true = tree_db:subscribe(T, "*.b", fun callback/3),
    ?line true = tree_db:subscribe(T, "*.a", fun callback/3),
    ?line true = tree_db:subscribe(T, "a.*", fun callback/3),
    ?line true = tree_db:subscribe(T, "a.b", fun callback/3),
    ?line true = tree_db:subscribe(T, "b.a", fun callback/3),
    ?line true = tree_db:subscribe(T, "a.b.c", fun callback/3),
    ?line true = tree_db:subscribe(T, "a.?.c", fun callback/3),
    %%
    put(tree_db:internal_key("*.b"), 0),
    put(tree_db:internal_key("a.b"), 0),
    put(tree_db:internal_key("a.*"), 0),
    tree_db:publish(T, "a.b", 123),
    ?line 1 = get(tree_db:internal_key("*.b")),
    ?line 1 = get(tree_db:internal_key("a.*")),
    ?line 1 = get(tree_db:internal_key("a.b")),
    ok.

callback(Topic,_Key,_Value) ->
    put(Topic, get(Topic)+1).

%%--------------------------------------------------------------------
%% @doc
%%  Test case function. Returns a description of the test
%%  case (doc), then returns a test specification (suite),
%%  or performs the actual test (Config).
%%
%% Arg = doc | suite | Config
%%   Indicates expected behaviour and return value.
%% Config = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Descr = [string()] | []
%%   String that describes the test case.
%% Spec = [tuple()] | []
%%   A test specification, see all/1.
%% Reason = term()
%%   The reason for skipping the test case.
%%
%% @spec TestCase(Arg) -> Descr | Spec | ok | exit() | {skip,Reason}
%% @end
%%--------------------------------------------------------------------
a_test_case(doc) ->
    ["Describe the main purpose of this test case"];

a_test_case(suite) ->
    [];

a_test_case(Config) when is_list(Config) ->
    ok.

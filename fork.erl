-module(fork).
-behaviour(gen_fsm).

%% API
-export([start_link/0]).

%% gen_fsm callbacks
-export([
         init/1, terminate/3, code_change/4,
         handle_event/3, handle_sync_event/4, handle_info/3
        ]).
-export([on_table/2, on_table/3,
         in_use/2, in_use/3]).

-define(SERVER, ?MODULE).

-type gen_state() :: on_table | in_use.
-type state() :: map().


%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, pid()}.


%% @doc Starts, initializes and links gen_fsm.
start_link() ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, {}, []).


%%%===================================================================
%%% init, terminate and code_change callbacks
%%%===================================================================

-spec init({}) -> {ok, gen_state(), state()}.
-spec terminate(atom(), gen_state(), state()) -> ok.
-spec code_change(string(), gen_state(), state(), any())
                 -> {ok, gen_state(), state()}.

%% @private
%% @doc Initialize internal gen_fsm state.
init(Name) ->
    {ok, on_table, #{name => Name}}.

%% @private
%% @doc Free any initialized resources before clearing gen_fsm state.
terminate(_Reason, _StateName, _State) ->
    ok.

%% @private
%% @doc Change running code.
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.


%%%===================================================================
%%% State change callbacks
%%%===================================================================

-spec on_table(any(), state()) -> {next_state, gen_state(), state()}.
%% @private
%% @doc Handle asynch computation for state_name.
on_table({get_fork, From}, #{name := Name} = State) ->
    college:log(Name, in_use),
    gen_fsm:send_event(From, {success, self()}),
    {next_state, in_use, State};
on_table(_Event, State) ->
    {next_state, on_table, State}.

-spec on_table(any(), {pid(), reference()}, state())
                -> {reply, any(), gen_state(), state()}.
%% @private
%% @doc Handle synch computation for state_name.
on_table(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, on_table, State}.



-spec in_use(any(), state()) -> {next_state, gen_state(), state()}.
-spec in_use(any(), {pid(), reference()}, state())
                -> {reply, any(), gen_state(), state()}.

%% @private
%% @doc Handle asynch computation for state_name.
in_use(leave_fork, #{name := Name} = State) ->
    college:log(Name, on_table),
    {next_state, on_table, State};
in_use(_Event, State) ->
    {next_state, in_use, State}.

%% @private
%% @doc Handle synch computation for state_name.
in_use(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, in_use, State}.


%%%===================================================================
%%% Generic event callbacks
%%%===================================================================

-spec handle_event(any(), gen_state(), state())
                  -> {next_state, gen_state(), state()}.
-spec handle_sync_event(any(), {pid(), reference()}, gen_state(), state())
                       -> {reply, any(), gen_state(), state()}.
-spec handle_info(any(), gen_state(), state())
                 -> {next_state, gen_state(), state()}.
%% @private
%% @doc Handle send_all_state_event/2.
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%% @private
%% @doc Handle sync_send_all_state_event/[2,3]
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%% @private
%% @doc Handle TCP messages not sent through eventing.
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

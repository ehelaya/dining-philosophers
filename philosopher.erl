-module(philosopher).

-behaviour(gen_fsm).


%% call backs

-export([init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]
       ).

%% states
-export([thinking/2, thinking/3,
         hungry/2, hungry/3,
         has_left_fork/2, has_left_fork/3,
         has_right_fork/2, has_right_fork/3,
         eating/2, eating/3]).


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

-spec init({string(), pid(), pid()}) -> {ok, gen_state(), state()}.
-spec terminate(atom(), gen_state(), state()) -> ok.
-spec code_change(string(), gen_state(), state(), any())
                 -> {ok, gen_state(), state()}.

%% @private
%% @doc Initialize internal gen_fsm state.
init({Name, LeftFork, RightFork}) ->
    process_flag(trap_exit, true),
    Timeout = rand:uniform(100),
    State = #{name => Name, left_fork => LeftFork, right_fork => RightFork},
    college:log(Name, thinking),
    gen_fsm:send_event_after(Timeout, timeout),
    {ok, thinking, State}.

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

thinking(timeout, State) ->
    gen_fsm:send_event(self(), grab_forks),
    {next_state, hungry, State};
thinking(start_thinking, #{name := Name} = State) ->
    college:log(Name, thinking),
    Timeout = rand:uniform(100),
    gen_fsm:send_event_after(Timeout, timeout),
    {next_state, thinking, State};
thinking(_, State) ->
    {next_state, thinking, State}.

thinking(_Other, _From,  State) ->
    {next_state, thinking, State}.

hungry(grab_forks, #{right_fork := RFork, left_fork := LFork, name := Name} = State) ->
    college:log(Name, hungry),
    gen_fsm:send_event(RFork, {get_fork, self()}),
    gen_fsm:send_event(LFork, {get_fork, self()}),
    {next_state, hungry, State};
hungry({success, LFork}, #{left_fork := LFork, name := Name} = State) ->
    college:log(Name, left_fork),
    {next_state, has_left_fork, State};
hungry({success, RFork}, #{right_fork := RFork, name := Name} = State) ->
    college:log(Name, right_fork),
    {next_state, has_right_fork, State};
hungry(_, State) ->
    {next_state, hungry, State}.

hungry(_Other, _From,  State) ->
	{next_state, hungry, State}.

has_left_fork({success, RFork}, #{right_fork := RFork, name := Name} = State) ->
    college:log(Name, right_fork),
    gen_fsm:send_event(self(), start_eating),
    {next_state, eating, State};
has_left_fork(_, State) ->
    {next_state, hungry, State}.

has_left_fork(_, _, State) ->
    {next_state, has_left_fork, State}.


has_right_fork({success, LFork}, #{left_fork := LFork, name := Name} = State) ->
    college:log(Name, left_fork),
    gen_fsm:send_event(self(), start_eating),
    {next_state, eating, State};
has_right_fork(_, State) ->
    {next_state, has_right_fork, State}.

has_right_fork(_, _, State) ->
    {next_state, has_right_fork, State}.

eating(timeout, #{right_fork := RFork, left_fork := LFork} = State) ->
    gen_fsm:send_event(RFork, leave_fork),
    gen_fsm:send_event(LFork, leave_fork),
    gen_fsm:send_event(self(), start_thinking),
    {next_state, thinking, State};
eating(start_eating, #{name := Name} = State) ->
    college:log(Name, eating),
    Timeout = rand:uniform(100),
    gen_fsm:send_event_after(Timeout, timeout),
    {next_state, eating, State};
eating(_, State) ->
    {next_state, eating, State}.

eating(_, _, State) ->
    {next_state, eating, State}.


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

-module(college).

-export([start/0]).
-export([log/2]).

-define(FORK_COUNT, 5).
-define(PHIL_NAMES, ["Socrates", "Confucius", "Aristole", "Homer", "Plato"]).

start() ->
    {ok, ForkPids} = start_forks(?FORK_COUNT),
    ok = start_philosophers(ForkPids).

log(Name, Status) ->
    io:format("~s : ~s ~n",[Name, status(Status)]).

%internal
start_forks(Count) ->
  init_fork(Count, []).

init_fork(0, Pids) ->
    {ok, Pids};
init_fork(Index, Pids) ->
    ForkName = lists:concat(["Fork ", Index]),
    {ok, Pid} = gen_fsm:start(fork, [ForkName], []),
    init_fork(Index - 1, [Pid] ++ Pids).

start_philosophers(ForkPids) ->
    [Fork1, Fork2, Fork3, Fork4, Fork5] = ForkPids,
    [Phil1Name, Phil2Name, Phil3Name, Phil4Name, Phil5Name] = ?PHIL_NAMES,
    {ok, _} = start_philosopher({Phil1Name, Fork1, Fork2}),
    {ok, _} = start_philosopher({Phil2Name, Fork2, Fork3}),
    {ok, _} = start_philosopher({Phil3Name, Fork3, Fork4}),
    {ok, _} = start_philosopher({Phil4Name, Fork4, Fork5}),
    {ok, _} = start_philosopher({Phil5Name, Fork5, Fork1}),
    ok.


start_philosopher(Data) ->
    gen_fsm:start(philosopher, Data, []).

status(Status) ->
    case Status of
        thinking    ->  "is thinking";
        hungry      -> "is hungry";
        eating      -> "is eating";
        right_fork  -> "got right fork";
        left_fork   -> "got left fork";
        on_table    -> "on table";
        in_use      ->"in use";
        Status      -> Status
    end.

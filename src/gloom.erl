% Copyright 2009 Paul J. Davis <paul.joseph.davis@gmail.com>
%
% This file is part of Gloom, which is released under the MIT license.
-module(gloom).
-behaviour(gen_server).

-export([start/0, start_link/0]).
-export([add_slave/1, respond/1, add_job/4, rem_jobs/0]).
-export([info/1, error/1]).

-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

start() ->
    application:start(crypto),
    application:start(gloom).

info(Arg) when is_list(Arg) ->
    error_logger:info_report(Arg);
info(Arg) ->
    error_logger:info_report([Arg]).

error(Arg) when is_list(Arg) ->
    error_logger:error_report(Arg);
error(Arg) ->
    error_logger:error_report([Arg]).

start_link() ->
    gen_server:start_link({local, gloom}, ?MODULE, [], []).

add_slave(Type) ->
    gen_server:cast(gloom, {slave, join, self(), Type}).

respond(Body) ->
    gen_server:cast(gloom, {slave, response, self(), Body}).

add_job(Id, Type, Priority, Body) ->
    gen_server:cast(gloom, {master, add, self(), Id, Type, Priority, Body}).

rem_jobs() ->
    gen_server:cast(gloom, {master, rem_jobs, self()}).

init(_) ->
    ets:new(slaves, [set, protected, named_table]),
    ets:new(jobs, [set, protected, named_table]),
    {ok, nil}.

terminate(_Reason, _State) ->
    gen_server:cast(gloom_master, reset),
    gen_server:cast(gloom_slave, reset),
    ok.

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast({slave, join, Pid, Type}, State) ->
    info({adding_slave, {Pid, Type}}),
    {monitors, Monitors} = process_info(self(), monitors),
    case lists:member({process, Pid}, Monitors) of
        false -> erlang:monitor(process, Pid);
        _ -> ok
    end,
    ok = gloom_util:add_slave(Pid, Type),
    {noreply, State};
handle_cast({slave, response, Pid, Body}, State) ->
    info({slave_responded, Pid}),
    ok = gloom_util:respond(Pid, Body),
    {noreply, State};
handle_cast({master, add, Pid, Id, Type, Priority, Body}, State) ->
    info({adding_job, {Pid, Id, Type}}),
    case (catch gloom_util:add_job(Pid, Id, Type, Priority, Body)) of
        ok -> ok;
        Error -> gen_server:cast(Pid, {error, Error})
    end,
    {noreply, State};
handle_cast({master, rem_jobs, Pid}, State) ->
    ok = gloom_util:rem_jobs(Pid),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    info({removing_slave, Pid}),
    ok = gloom_util:rem_slave(Pid),
    {noreply, State};
handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

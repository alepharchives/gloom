% Copyright 2009 Paul J. Davis <paul.joseph.davis@gmail.com>
%
% This file is part of Gloom, which is released under the MIT license.
-module(gloom).
-behaviour(gen_server).

-export([start/0, start_link/0]).
-export([add_slave/1, respond/1, add_job/4]).

-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

start() ->
    application:start(crypto),
    application:start(gloom).

start_link() ->
    gen_server:start_link({local, gloom}, ?MODULE, [], []).

add_slave(Type) ->
    gen_server:cast(gloom, {slave, join, self(), Type}).

respond(Body) ->
    gen_server:cast(gloom, {slave, response, self(), Body}).

add_job(Id, Type, Priority, Body) ->
    gen_server:cast(gloom, {master, add, self(), Id, Type, Priority, Body}).

init(_) ->
    ets:new(slaves, [set, protected, named_table]),
    ets:new(jobs, [set, protected, named_table]),
    {ok, nil}.

terminate(_Reason, _State) ->
    gen_server:cast(gloom_master, reset),
    gen_server:cast(gloom_slave, reset),
    io:format("GlOOM TERMINATE!~n", []),
    ok.

handle_call(Msg, _From, State) ->
    io:format("Gloom called: ~p~n", [Msg]),
    {reply, ok, State}.

handle_cast({slave, join, Pid, Type}, State) ->
    io:format("Slave joining: ~p => ~p~n", [Pid, Type]),
    erlang:monitor(process, Pid),
    ok = gloom_util:add_slave(Pid, Type),
    {noreply, State};
handle_cast({slave, response, Pid, Body}, State) ->
    io:format("Job response: ~p => ~p~n", [Pid, Body]),
    ok = gloom_util:respond(Pid, Body),
    {noreply, State};
handle_cast({master, add, Pid, Id, Type, Priority, Body}, State) ->
    io:format("Adding job: {~p, ~p} => ~p~n", [Pid, Id, Body]),
    case (catch gloom_util:add_job(Pid, Id, Type, Priority, Body)) of
        ok -> ok;
        Error -> gen_server:cast(Pid, {error, Error})
    end,
    {noreply, State};
handle_cast(Msg, State) ->
    io:format("Gloom cast: ~p~n", [Msg]),
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    io:format("Slave leaving: ~p~n", [Pid]),
    ok = gloom_util:rem_slave(Pid),
    {noreply, State};
handle_info(Msg, State) ->
    io:format("Gloom info: ~p~n", [Msg]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
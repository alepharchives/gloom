% Copyright 2009 Paul J. Davis <paul.joseph.davis@gmail.com>
%
% This file is part of Gloom, which is released under the MIT license.
-module(gloom_server).
-behaviour(gen_server).

-export([start_link/2]).
-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_info/2, handle_cast/2]).
-export([listen/1, stream/2]).

-export([send/2, send_error/2]).

-record(state, {
    mod=nil,
    socket=nil,
    clients=[]
}).

-define(POOL, 2).
-define(PORT_OPTS, [
    binary,
    {active, false},
    {reuseaddr, true},
    {nodelay, true},
    {packet, raw}
]).

start_link(Module, Port) ->
    gen_server:start_link({local, Module}, ?MODULE, {Module, Port}, []).

send(Socket, Data) ->
    Json = list_to_binary(lists:flatten(mochijson2:encode(Data))),
    Length = size(Json),
    Packet = <<Length:32/integer, Json/binary>>,
    gen_tcp:send(Socket, Packet).

send_error(Socket, {Error, Reason}) ->
    send(Socket, {[{error, Error}, {reason, Reason}]}).

init({Module, Port}) ->
    process_flag(trap_exit, true),
    case (catch gen_tcp:listen(Port, ?PORT_OPTS)) of
        {ok, Socket} ->
            {ok, spawn_clients(#state{mod=Module, socket=Socket})};
        {error, Reason} ->
            {stop, Reason};
        Other ->
            {stop, Other}
    end.

terminate(_Reason, State) ->
    lists:map(fun(X) -> exit(X) end, State#state.clients),
    gen_tcp:close(State#state.socket).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast(reset, State) ->
    [exit(Pid, normal) || Pid <- State#state.clients],
    {noreply, spawn_clients(State#state{clients=[]})};
handle_cast({unreg, Pid}, State) ->
    Curr = [Cli || Cli <- State#state.clients, Cli /= Pid],
    {noreply, spawn_clients(State#state{clients=Curr})}.

handle_info({'EXIT', Pid, _}, State) ->
    Curr = [Cli || Cli <- State#state.clients, Cli /= Pid],
    {noreply, spawn_clients(State#state{clients=Curr})}.

spawn_clients(#state{clients=Clients}=State) ->
    New = lists:map(fun(_) ->
        proc_lib:spawn_link(?MODULE, listen, [State#state{clients=[]}])
    end, lists:seq(1, ?POOL-length(Clients))),
    State#state{clients=Clients ++ New}.

listen(#state{mod=Module, socket=Socket}) ->
    State = case (catch gen_tcp:accept(Socket)) of
        {ok, Client} ->
            ok = gen_server:cast(Module, {unreg, self()}),
            Pid = proc_lib:spawn_link(?MODULE, stream, [self(), Client]),
            {Pid, Client};
        {error, Reason} ->
            exit({error, accept_failed, Reason})
    end,
    {ok, ModState} = Module:init(State),
    gen_server:enter_loop(Module, [], ModState).

stream(Parent, Socket) ->
    case gen_tcp:recv(Socket, 4) of
        {ok, <<Length:32/integer>>} ->
            case gen_tcp:recv(Socket, Length) of
                {ok, Json} ->
                    gen_server:cast(Parent, {tcp, mochijson2:decode(Json)}),
                    stream(Parent, Socket);
                {error, closed} ->
                    ok;
                {error, RecvError} ->
                    exit({error, receive_error, RecvError})
            end;
        {error, closed} ->
            gen_server:cast(Parent, socket_closed),
            ok;
        {error, HdrError} ->
            exit({error, header_error, HdrError})
    end.

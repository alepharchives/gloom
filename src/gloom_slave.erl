% Copyright 2009 Paul J. Davis <paul.joseph.davis@gmail.com>
%
% This file is part of Gloom, which is released under the MIT license.
-module(gloom_slave).
-behaviour(gen_server).

-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_info/2, handle_cast/2]).

init(State) ->
    ok = gloom:add_slave(nil),
    {ok, State}.

terminate(_Reason, {_Stream, Client}) ->
    gen_tcp:close(Client).

handle_call(Msg, _From, State) ->
    io:format("Slave call: ~p~n", [Msg]),
    {reply, ok, State}.

handle_cast({tcp, Json}, {_, Socket}=State) ->
    case (catch handle_request(Json)) of
        ok -> ok;
        Error ->
            gloom_server:send_error(Socket, Error)
    end,
    {noreply, State};
handle_cast({job, Id, Body}, {_, Socket}=State) ->
    Job = {[
        {<<"type">>, <<"job">>},
        {<<"id">>, Id},
        {<<"body">>, Body}
    ]},
    gloom_server:send(Socket, Job),
    {noreply, State};
handle_cast({error, Error}, {_, Socket}=State) ->
    gloom_server:send_error(Socket, Error),
    {stop, normal, State};
handle_cast(socket_closed, State) ->
    {stop, normal, State};
handle_cast(Info, State) ->
    io:format("Slave cast: ~p~n", [Info]),
    {noreply, State}.

handle_info(Info, State) ->
    io:format("Slave info: ~p~n", [Info]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_request({Fields}) ->
    case proplists:get_value(<<"type">>, Fields) of
        undefined ->
            throw({missing_field, type});
        Type ->
            handle_request(Type, Fields)
    end.

handle_request(<<"join">>, Fields) ->
    case proplists:get_value(<<"job_type">>, Fields) of
        undefined ->
            throw({missing_field, job_type});
        JobType ->
            gloom:add_slave(JobType)
    end;
handle_request(<<"response">>, Fields) ->
    case proplists:get_value(<<"body">>, Fields) of
        undefined ->
            throw({missing_field, body});
        Body ->
            ok = gloom:respond(Body)
    end;
handle_request(Type, _Fields) ->
    throw({unknown_type, Type}).

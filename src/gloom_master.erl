% Copyright 2009 Paul J. Davis <paul.joseph.davis@gmail.com>
%
% This file is part of Gloom, which is released under the MIT license.
-module(gloom_master).
-behaviour(gen_server).

-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_info/2, handle_cast/2]).

init(State) ->
    {ok, State}.

terminate(_Reason, {_Stream, Client}) ->
    gen_tcp:close(Client).

handle_call(Msg, _From, State) ->
    io:format("Master call: ~p~n", [Msg]),
    {reply, ok, State}.

handle_cast({tcp, Json}, {_, Socket}=State) ->
    case (catch handle_request(Json)) of
        ok -> ok;
        Error ->
            io:format("Sending error: ~p~n", [Error]),
            gloom_server:send_error(Socket, Error),
            io:format("Error sent.~n", [])
    end,
    {noreply, State};
handle_cast({response, Resp}, {_, Socket}=State) ->
    gloom_server:send(Socket, Resp),
    {noreply, State};
handle_cast({error, Error}, {_, Socket}=State) ->
    gloom_server:send_error(Socket, Error),
    {noreply, State};
handle_cast(socket_closed, State) ->
    {stop, normal, State};
handle_cast(Info, State) ->
    io:format("Master cast: ~p~n", [Info]),
    {noreply, State}.

handle_info(Info, State) ->
    io:format("Master info: ~p~n", [Info]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_request(Json) ->
    {Fields} = Json,
    case proplists:get_value(<<"type">>, Fields) of
        undefined -> throw({missing_field, type});
        Type -> handle_request(Type, Fields)
    end.

handle_request(<<"job">>, Fields) ->
    JobId = case proplists:get_value(<<"id">>, Fields) of
        undefined -> throw({missing_field, id});
        JobIdVal -> JobIdVal
    end,
    JobType = case proplists:get_value(<<"job_type">>, Fields) of
        undefined -> throw({missing_field, job_type});
        JobTypeVal -> JobTypeVal
    end,
    Priority = case proplists:get_value(<<"priority">>, Fields) of
        undefined -> 0;
        PriorityVal when is_integer(PriorityVal) -> PriorityVal;
        _ -> throw({invalid_type, priority})
    end,
    Body = case proplists:get_value(<<"body">>, Fields) of
        undefined -> throw({missing_field, body});
        BodyVal -> BodyVal
    end,
    ok = gloom:add_job(JobId, JobType, Priority, Body);
handle_request(Type, _Fields) ->
    throw({unknown_type, Type}).

            
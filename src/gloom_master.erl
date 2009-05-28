% Copyright 2009 Paul J. Davis <paul.joseph.davis@gmail.com>
%
% This file is part of Gloom, which is released under the MIT license.
-module(gloom_master).
-behaviour(gen_server).

-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_info/2, handle_cast/2]).

init(State) ->
    gloom:info({master_connected, self()}),
    {ok, State}.

terminate(_Reason, {_Stream, Client}) ->
    gloom:info({master_disconnected, self()}),
    gloom:rem_jobs(),
    gen_tcp:close(Client).

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast({tcp, Json}, {_, Socket}=State) ->
    case (catch handle_request(Json)) of
        {ok, JobId} ->
            gloom_server:send(Socket, {[{<<"ok">>, true}, {<<"id">>, JobId}]});
        Error ->
            gloom_server:send_error(Socket, Error)
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
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_request(Json) ->
    {Fields} = Json,
    case proplists:get_value(<<"action">>, Fields) of
        undefined -> throw({missing_field, type});
        Type -> handle_request(Type, Fields)
    end.

handle_request(<<"submit">>, Fields) ->
    JobId = case proplists:get_value(<<"id">>, Fields) of
        undefined -> throw({missing_field, id});
        JobIdVal -> JobIdVal
    end,
    Type = case proplists:get_value(<<"type">>, Fields) of
        undefined -> throw({missing_field, type});
        TypeVal -> TypeVal
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
    ok = gloom:add_job(JobId, Type, Priority, Body),
    {ok, JobId};
handle_request(<<"join">>, _Fields) ->
    throw({invalid_action, not_a_slave});
handle_request(<<"respond">>, _Fields) ->
    throw({invalid_action, not_a_slave});
handle_request(Type, _Fields) ->
    throw({unknown_action, Type}).

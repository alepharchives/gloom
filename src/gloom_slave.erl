% Copyright 2009 Paul J. Davis <paul.joseph.davis@gmail.com>
%
% This file is part of Gloom, which is released under the MIT license.
-module(gloom_slave).
-behaviour(gen_server).

-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_info/2, handle_cast/2]).

-record(state, {
    stream=nil,
    sock=nil,
    processing=false
}).

init({Stream, Socket}) ->
    gloom:info({slave_connected, self()}),
    {ok, #state{stream=Stream, sock=Socket, processing=false}}.

terminate(_Reason, State) ->
    gloom:info({slave_disconnected, self()}),
    gen_tcp:close(State#state.sock).

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast({tcp, Json}, State) ->
    case (catch handle_request(Json, State)) of
        ok ->
            gloom_server:send(State#state.sock, {[{<<"ok">>, true}]}),
            {noreply, State};
        responded ->
            gloom_server:send(State#state.sock, {[{<<"ok">>, true}]}),
            {noreply, State#state{processing=false}};
        Error ->
            gloom_server:send_error(State#state.sock, Error),
            {noreply, State}
    end;
handle_cast({job, Id, Type, Body}, State) ->
    case State#state.processing of
        true -> exit({invalid_state, already_processing});
        false -> ok
    end,
    Job = {[
        {<<"action">>, <<"job">>},
        {<<"id">>, Id},
        {<<"type">>, Type},
        {<<"body">>, Body}
    ]},
    gloom_server:send(State#state.sock, Job),
    {noreply, State#state{processing=true}};
handle_cast({error, Error}, State) ->
    gloom:info({sending_error, Error}),
    gloom_server:send_error(State#state.sock, Error),
    {stop, normal, State};
handle_cast(socket_closed, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_request({Fields}, State) ->
    case proplists:get_value(<<"action">>, Fields) of
        undefined ->
            throw({missing_field, type});
        Type ->
            handle_request(Type, Fields, State)
    end.

handle_request(<<"join">>, Fields, _State) ->
    case proplists:get_value(<<"type">>, Fields) of
        undefined ->
            throw({missing_field, job_type});
        JobType ->
            ok = gloom:add_slave(JobType)
    end;
handle_request(<<"respond">>, Fields, State) ->
    case State#state.processing of
        false -> throw({invalid_state, not_processing});
        true -> ok
    end,
    case proplists:get_value(<<"body">>, Fields) of
        undefined ->
            throw({missing_field, body});
        Body ->
            ok = gloom:respond(Body),
            responded
    end;
handle_request(<<"submit">>, _Fields, _State) ->
    throw({invalid_action, not_a_master});
handle_request(Type, _Fields, _State) ->
    throw({unknown_type, Type}).

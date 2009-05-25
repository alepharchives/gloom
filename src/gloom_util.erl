% Copyright 2009 Paul J. Davis <paul.joseph.davis@gmail.com>
%
% This file is part of Gloom, which is released under the MIT license.
-module(gloom_util).

-export([add_slave/2, rem_slave/1, respond/2, add_job/5, rem_jobs/1]).

-record(slave, {pid, type, status, job}).
-record(job, {id, pid, type, status, priority, time, retries, body}).

add_slave(Pid, Type) ->
    Slave = #slave{pid=Pid, type=Type, status=idle, job=nil},
    true = ets:insert(slaves, {Pid, Slave}),
    ok = check_job_state(),
    ok.

rem_slave(Pid) ->
    case ets:lookup(slaves, Pid) of
        [] ->
            ok;
        [{_, Slave}] ->
            case Slave of
                #slave{status=busy, job=JobId} ->
                    [{_, Job}] = ets:lookup(jobs, JobId),
                    NewJob = Job#job{
                        status=queued,
                        priority=Job#job.priority+1,
                        retries=Job#job.retries-1
                    },
                    case {NewJob#job.pid, NewJob#job.retries} of
                        {nil, _} ->
                            % Master disconnected, drop the response
                            true = ets:delete(jobs, NewJob#job.id),
                            ok;
                        {_, Retries} when Retries < 0 ->
                            Error = {job_failed, NewJob#job.id},
                            true = ets:delete(jobs, NewJob#job.id),
                            gen_server:cast(NewJob#job.pid, {error, Error});
                        _ ->
                            true = ets:insert(jobs, {NewJob#job.id, NewJob}),
                            ok = check_job_state()
                    end;
                _ ->
                    ok
            end,
            true = ets:delete(slaves, Pid)
    end,
    ok.

respond(Pid, Body) ->
    case ets:lookup(slaves, Pid) of
        [] ->
            gen_server:cast(Pid, {error, {internal_error, unknown_slave}});
        [{_, Slave}] ->
            [{_, Job}] = ets:lookup(jobs, Slave#slave.job),
            case Job#job.pid of
                nil ->
                    % This master closed the socket before a job
                    % completed. Send the response to /dev/null
                    ok;
                _ ->
                    Resp = {[{id, Job#job.id}, {body, Body}]},
                    ok = gen_server:cast(Job#job.pid, {response, Resp}),
                    true = ets:delete(jobs, Job#job.id),
                    NewSlave = Slave#slave{status=idle, job=nil},
                    true = ets:insert(slaves, {Slave#slave.pid, NewSlave})
            end,
            ok = check_job_state()
    end,
    ok.

add_job(Pid, Id, Type, Priority, Body) ->
    case ets:lookup(jobs, Id) of
        [] -> ok;
        _ -> throw({id_exists, Id})
    end,
    Job = #job{
        id=Id,
        pid=Pid,
        type=Type,
        status=queued,
        time=erlang:localtime(),
        priority=Priority,
        retries=3,
        body=Body
    },
    true = ets:insert(jobs, {Id, Job}),
    ok = check_job_state().

rem_jobs(Pid) ->
    Pattern = #job{
        id='$1', pid=Pid, type='_', status=queued,
        priority='_', time='_', retries='_', body='_'
    },
    lists:foreach(fun([Id]) ->
        true = ets:delete(jobs, Id)
    end, ets:match(jobs, {'_', Pattern})),
    Pattern2 = Pattern#job{status='_'},
    lists:foreach(fun([Id]) ->
        [{Id, Job}] = ets:lookup(jobs, Id),
        true = ets:insert(jobs, {Id, Job#job{pid=nil}})
    end, ets:match(jobs, {'_', Pattern2})).

check_job_state() ->
    %gloom:info({slaves, ets:tab2list(slaves)}),
    %gloom:info({jobs, ets:tab2list(jobs)}),
    lists:foreach(fun([Pid, Type]) ->
        case find_job(Type) of
            nil ->
                ok;
            Job when is_record(Job, job) ->
                [{_, Slave}] = ets:lookup(slaves, Pid),
                NewRow = {Pid, Slave#slave{status=busy, job=Job#job.id}},
                true = ets:insert(slaves, NewRow),
                true = ets:insert(jobs, {Job#job.id, Job#job{status=running}}),
                gen_server:cast(
                    Slave#slave.pid, {job, Job#job.id, Type, Job#job.body}
                )
        end
    end, idle_slaves()).

idle_slaves() ->
    M1 = ets:match(slaves, {'_', {slave, '$1', '$2', idle, nil}}),
    M2 = [{random:uniform(), M} || M <- M1],
    [M || {_, M} <- lists:sort(M2)].
    
find_job(Type) ->
    Init = {{-1000000, {{1000000,0,0}, {0,0,0}}}, #job{}},
    case find_job(Type, nil, Init) of
        Init -> nil;
        {_, Job} -> Job
    end.
    
find_job(Type, Continue, Acc) ->
    case queued_jobs(Type, Continue) of
        '$end_of_table' ->
            Acc;
        {Matches, Continue2} ->
            Found = lists:foldl(
                fun([Id, NewPr, NewTm], {{CurrPr, CurrTm}, _}=Curr) ->
                    case NewPr >= CurrPr andalso NewTm < CurrTm of
                        true ->
                            [{_, NewJob}] = ets:lookup(jobs, Id),
                            {{NewPr, NewTm}, NewJob};
                        _ ->
                            Curr
                    end
                end,
            Acc, Matches),
            find_job(Type, Continue2, Found)
    end.

queued_jobs(Type, nil) ->
    Pattern = #job{
        id='$1', pid='_', type=Type, status=queued,
        priority='$2', time='$3', retries='_', body='_'
    },
    ets:match(jobs, {'_', Pattern}, 50);
queued_jobs(_Type, Continue) ->
    ets:match(Continue).

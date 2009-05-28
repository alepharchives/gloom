% Copyright 2009 Paul J. Davis <paul.joseph.davis@gmail.com>
%
% This file is part of Gloom, which is released under the MIT license.
-module(gloom_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link(gloom_sup, []).

init(_Args) ->
    {ok, MasterPort} = application:get_env(gloom, master_port),
    {ok, SlavePort} = application:get_env(gloom, slave_port),
    {ok, {
        {one_for_one, 1, 1}, [
        {
            gloom,
            {gloom, start_link, []},
            permanent,
            2000,
            worker,
            [gloom]
        },
        {
            gloom_master,
            {gloom_server, start_link, [
                gloom_master, MasterPort
            ]},
            permanent,
            2000,
            worker,
            [gloom_server]
        },
        {
            gloom_slave,
            {gloom_server, start_link, [
                gloom_slave, SlavePort
            ]},
            permanent,
            2000,
            worker,
            [gloom_server]
        }
    ]}}.

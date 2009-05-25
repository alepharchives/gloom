% Copyright 2009 Paul J. Davis <paul.joseph.davis@gmail.com>
%
% This file is part of Gloom, which is released under the MIT license.
-module(gloom_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    gloom_sup:start_link().

stop(_State) ->
    ok.

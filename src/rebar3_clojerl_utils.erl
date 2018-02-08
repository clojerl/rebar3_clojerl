%% The MIT License (MIT)

%% Copyright (c) 2014 Adam Lindberg

%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:

%% The above copyright notice and this permission notice shall be included in all
%% copies or substantial portions of the Software.

%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.
-module(rebar3_clojerl_utils).

% API
-export([stacktrace/1]).
-export([stacktrace/2]).

%--- API ----------------------------------------------------------------------

stacktrace(Stacktrace) -> stacktrace(Stacktrace, [{indent, 4}]).

stacktrace(Stacktrace, Options) ->
    Indent = lists:duplicate(proplists:get_value(indent, Options), <<" ">>),
    stacktrace_pretty(Indent, Stacktrace).

stacktrace_pretty(_Indent, []) ->
    [];
stacktrace_pretty(Indent, [Entry|Stacktrace]) ->
    {Mod, Func, Arity, [{file, File}, {line, Line}]} = Entry,
    Output = [
        Indent,
        atom_to_list(Mod),
        <<"/">>,
        atom_to_list(Func),
        <<".">>,
        integer_to_binary(Arity),
        <<" (">>,
        File,
        <<":">>,
        integer_to_binary(Line),
        <<")">>,
        io_lib:format("~n", [])
    ],
    [Output|stacktrace_pretty(Indent, Stacktrace)].

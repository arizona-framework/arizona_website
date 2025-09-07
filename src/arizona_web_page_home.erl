-module(arizona_web_page_home).
-behaviour(arizona_stateful).
-compile({parse_transform, arizona_parse_transform}).
-export([mount/1]).
-export([render/1]).

mount(Bindings) ->
    arizona_stateful:new(?MODULE, Bindings).

render(Bindings) ->
    arizona_template:from_string(~"""
    <div id="{arizona_template:get_binding(id, Bindings)}">
        Hello, {arizona_template:get_binding(name, Bindings)}!
    </div>
    """).

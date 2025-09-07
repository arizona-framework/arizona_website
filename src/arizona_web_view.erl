-module(arizona_web_view).
-behaviour(arizona_view).
-compile({parse_transform, arizona_parse_transform}).
-export([mount/2]).
-export([render/1]).

mount(#{title := Title}, _Req) ->
    Bindings = #{
        id => ~"view",
        name => ~"World"
    },
    Layout =
        {arizona_web_layout, render, main_content, #{
            title => Title
        }},
    arizona_view:new(?MODULE, Bindings, Layout).

render(Bindings) ->
    arizona_template:from_string(~"""
    <div id="{arizona_template:get_binding(id, Bindings)}">
        Hello, {arizona_template:get_binding(name, Bindings)}!
    </div>
    """).

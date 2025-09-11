-module(arizona_website_view).
-behaviour(arizona_view).
-compile({parse_transform, arizona_parse_transform}).
-export([mount/2]).
-export([render/1]).
-export([handle_event/3]).

mount({Page, PageBindings}, _Req) ->
    _ = arizona_live:is_connected(self()) andalso initialize_connected_session(),
    Bindings = #{
        id => ~"app",
        page_module => Page,
        page_bindings => PageBindings#{id => ~"page"}
    },
    Layout =
        {arizona_website_layout, render, main_content, #{
            env => application:get_env(arizona_website, env, dev),
            title => ~"Arizona Framework"
        }},
    arizona_view:new(?MODULE, Bindings, Layout).

render(Bindings) ->
    arizona_template:from_string(~"""
    <div id="{arizona_template:get_binding(id, Bindings)}">
        {arizona_template:render_stateful(
            arizona_template:get_binding(page_module, Bindings),
            arizona_template:get_binding(page_bindings, Bindings)
        )}
    </div>
    """).

handle_event(~"reload", FileType, View) ->
    {[{reply, #{~"reload" => FileType}}], View}.

initialize_connected_session() ->
    arizona_pubsub:join(~"reload", self()).

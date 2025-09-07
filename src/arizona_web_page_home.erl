-module(arizona_web_page_home).
-behaviour(arizona_stateful).
-compile({parse_transform, arizona_parse_transform}).
-export([mount/1]).
-export([render/1]).

mount(Bindings) ->
    arizona_stateful:new(?MODULE, Bindings).

render(Bindings) ->
    arizona_template:from_string(~""""
    <div
        id="{arizona_template:get_binding(id, Bindings)}"
        class="min-h-screen bg-gradient-to-br from-obsidian via-charcoal to-arizona-terracotta"
    >
        {arizona_template:render_stateless(arizona_web_components, header, #{})}
        {arizona_template:render_stateless(arizona_web_components, hero, #{})}
        {arizona_template:render_stateless(arizona_web_components, features, #{})}
        {arizona_template:render_stateless(arizona_web_components, code_example, #{})}
        {arizona_template:render_stateless(arizona_web_components, performance_stats, #{})}
        {arizona_template:render_stateless(arizona_web_components, footer, #{})}
    </div>
    """").

-module(arizona_website_cheatsheet).
-moduledoc """
The template cheat sheet. A view of its own so the page can pop it out into a
Picture-in-Picture window (`arizona_js:request_pip/2` takes a view id), and it
carries its own `theme` slot because the window takes it out of the page.
""".
-include_lib("arizona/include/arizona_stateful.hrl").

-export([mount/1]).
-export([render/1]).

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {#{id => maps:get(id, Bindings)}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {'div',
            [
                {id, ?get(id)},
                {'data-theme', ?local(~"theme", ~"system")},
                {class, ~"rounded-xl border border-line bg-surface p-5 font-sans"}
            ],
            [
                {p, [{class, ~"mb-4 text-xs font-semibold uppercase tracking-wider text-muted"}], [
                    ~"Template cheat sheet"
                ]},
                {dl, [{class, ~"space-y-3 text-sm"}], [
                    ?each(
                        fun(#{code := Code, text := Text}) ->
                            {'div', [], [
                                {dt, [{class, ~"font-mono text-[0.8125rem] text-accent"}], [Code]},
                                {dd, [{class, ~"text-muted"}], [Text]}
                            ]}
                        end,
                        entries()
                    )
                ]}
            ]}
    ).

entries() ->
    [
        #{code => ~"{Tag, Attrs, Children}", text => ~"An element, checked by the compiler."},
        #{code => ~"?get(Key)", text => ~"Read a binding; the slot re-renders when it changes."},
        #{code => ~"?each(Fun, Items)", text => ~"A list or stream, diffed item by item."},
        #{code => ~"?stateful(Mod, Props)", text => ~"Embed a live view with its own state."},
        #{code => ~"?stateless(Fun, Props)", text => ~"Reuse a pure template."},
        #{code => ~"?local(Key, Init)", text => ~"A value the browser owns: no round-trip."},
        #{code => ~"arizona_js:push_event(Name)", text => ~"Send an event to handle_event/3."},
        #{code => ~"arizona_js:set(Key, Value)", text => ~"Update a ?local in the browser."}
    ].

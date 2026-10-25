-module(arizona_website_guide).
-moduledoc """
The guide beside the example. A view of its own so the page can pop it out into
a Picture-in-Picture window (`arizona_js:request_pip/2` takes a view id), where
it keeps working both ways: it holds the same `tab` and `theme` slots as the
page, so it follows what the page shows, and its buttons drive the page, since a
command's selector is looked up in every window the page has.
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
                {'data-tab', ?local(~"tab", ~"view")},
                {class, ~"guide rounded-xl border border-line bg-surface p-5 font-sans"}
            ],
            [
                {p, [{class, ~"text-xs font-semibold uppercase tracking-wider text-muted"}], [
                    ~"Example guide"
                ]},
                {p, [{class, ~"mt-1 text-sm text-muted"}], [
                    ~"Pop it out: it follows the page, and drives it."
                ]},
                {'div',
                    [{class, ~"mt-4 grid grid-cols-3 gap-1 rounded-lg bg-sunken p-0.5 text-xs"}], [
                        tab(~"view", ~"View"),
                        tab(~"config", ~"Config"),
                        tab(~"static", ~"Static")
                    ]},
                {dl, [{class, ~"note note-view mt-4 space-y-3 text-sm"}], [
                    entry(
                        ~"?get(count)",
                        ~"Reads a binding. Only this slot re-renders when it changes."
                    ),
                    entry(
                        ~"arizona_js:push_event(~\"inc\")",
                        ~"The click becomes an event on the server."
                    ),
                    entry(
                        ~"handle_event/3",
                        ~"Returns the new bindings; the reply patches one number."
                    )
                ]},
                {dl, [{class, ~"note note-config mt-4 space-y-3 text-sm"}], [
                    entry(~"adapter", ~"The server package: Roadrunner or Cowboy."),
                    entry(~"{live, Path, View, Opts}", ~"A page that stays live over its socket."),
                    entry(~"{ws, ~\"/ws\", #{}}", ~"The WebSocket every live page connects to."),
                    entry(~"{asset, ...}", ~"Serves the client runtime from Arizona's priv.")
                ]},
                {dl, [{class, ~"note note-static mt-4 space-y-3 text-sm"}], [
                    entry(~"arizona_static:generate/3", ~"Renders views to HTML files, no server."),
                    entry(~"arizona-static.min.js", ~"Runs the page's commands in the browser."),
                    entry(~"start()", ~"What this site calls instead of connect('/ws').")
                ]},
                {'div',
                    [{class, ~"mt-5 flex items-center justify-between border-t border-line pt-4"}],
                    [
                        {span, [{class, ~"text-xs text-muted"}], [~"Theme"]},
                        {'div', [{class, ~"flex gap-1 text-xs text-muted"}], [
                            theme(~"system", ~"Auto"),
                            theme(~"light", ~"Light"),
                            theme(~"dark", ~"Dark")
                        ]}
                    ]}
            ]}
    ).

%% Sets the tab in every view: the page's example shows it and this guide marks
%% it. The page scrolls the example into view only when it is off screen, so the
%% switch is seen from a floating window too.
tab(Tab, Label) ->
    {button,
        [
            {type, ~"button"},
            {class, [~"guide-", Tab, ~" rounded-md px-2 py-1 font-medium text-muted"]},
            {az_click, [
                arizona_js:set_all(~"tab", Tab),
                arizona_js:scroll_to(~"#example-tabs", #{block => nearest})
            ]}
        ],
        [Label]}.

theme(Theme, Label) ->
    {button,
        [
            {type, ~"button"},
            {class, [~"theme-", Theme, ~" rounded-md px-2 py-1 font-medium"]},
            {az_click, [
                arizona_js:set_all(~"theme", Theme),
                arizona_js:dispatch_event(~"arizona:theme", #{theme => Theme})
            ]}
        ],
        [Label]}.

entry(Code, Text) ->
    {'div', [], [
        {dt, [{class, ~"font-mono text-[0.8125rem] text-accent"}], [Code]},
        {dd, [{class, ~"text-muted"}], [Text]}
    ]}.

-module(arizona_website_home_page).
-moduledoc """
The home page. Rendered once, at build time (`arizona_website:build/0`), and
served as plain files: the theme switch, tabs, dialog, copy buttons and the
cheat sheet's Picture-in-Picture window run in the browser on Arizona's static
client, with no server behind them. The sections are local element helpers the
parse transform inlines.
""".
-include_lib("arizona/include/arizona_stateful.hrl").

-export([mount/1]).
-export([render/1]).

-define(GITHUB, ~"https://github.com/arizona-framework/arizona").
-define(DOCS, ~"https://hexdocs.pm/arizona").
-define(QUICK_START, ~"https://github.com/arizona-framework/arizona#quick-start").
-define(CONTAINER, ~"mx-auto max-w-5xl px-6").
-define(EYEBROW, ~"mb-3 text-xs font-semibold uppercase tracking-wider text-accent").
-define(H2, ~"text-3xl font-semibold tracking-tight text-ink sm:text-4xl").
-define(LEAD, ~"mt-4 max-w-2xl text-lg leading-relaxed text-muted").
-define(BUTTON,
    ~"inline-flex items-center gap-2 rounded-lg px-4 py-2.5 text-sm font-semibold transition-colors"
).
-define(PRIMARY, [?BUTTON, ~" bg-accent text-accent-ink hover:opacity-90"]).
-define(SECONDARY, [?BUTTON, ~" border border-line bg-surface text-ink hover:bg-sunken"]).
-define(NAV_LINK, ~"text-sm text-muted transition-colors hover:text-ink").

-spec mount(az:bindings()) -> az:mount_ret().
mount(_Bindings) ->
    %% The sample is the real module, compiled with the site.
    {ok, Counter} = file:read_file("examples/counter.erl"),
    {#{id => ~"page", counter => Counter}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {'div',
            [
                %% Rendered once, at build time: no diff markers in the output.
                'az-nodiff',
                {id, ?get(id)},
                {'data-theme', ?local(~"theme", ~"system")},
                {class, ~"min-h-screen font-sans antialiased"}
            ],
            [
                header(),
                {main, [], [
                    hero(),
                    example(?get(counter)),
                    showcase(),
                    features(),
                    adapters()
                ]},
                footer(),
                quick_start()
            ]}
    ).

%% --------------------------------------------------------------------
%% Sections
%% --------------------------------------------------------------------

header() ->
    {header, [{class, ~"sticky top-0 z-10 border-b border-line bg-canvas/85 backdrop-blur"}], [
        {nav, [{class, [?CONTAINER, ~" flex h-16 items-center justify-between gap-6"]}], [
            {a, [{href, ~"/"}, {class, ~"flex items-center gap-2.5"}], [
                {img, [
                    {src, ~"images/arizona_256x256.jpeg"},
                    {alt, <<>>},
                    {class, ~"h-8 w-8 rounded-md"},
                    {width, ~"32"},
                    {height, ~"32"}
                ]},
                {span, [{class, ~"font-semibold text-ink"}], [~"Arizona"]}
            ]},
            {'div', [{class, ~"flex items-center gap-6"}], [
                {'div', [{class, ~"hidden items-center gap-6 md:flex"}], [
                    {a, [{href, ~"#example"}, {class, ?NAV_LINK}], [~"Example"]},
                    {a, [{href, ~"#this-page"}, {class, ?NAV_LINK}], [~"This page"]},
                    {a, [{href, ~"#features"}, {class, ?NAV_LINK}], [~"Features"]},
                    {a, [{href, ?DOCS}, {class, ?NAV_LINK}], [~"Docs"]},
                    {a, [{href, ?GITHUB}, {class, ?NAV_LINK}], [~"GitHub"]}
                ]},
                theme_switch()
            ]}
        ]}
    ]}.

theme_switch() ->
    {'div',
        [
            {role, ~"group"},
            {'aria-label', ~"Color theme"},
            {class, ~"flex rounded-lg border border-line bg-sunken p-0.5 text-xs text-muted"}
        ],
        [
            theme_button(~"system", ~"Auto"),
            theme_button(~"light", ~"Light"),
            theme_button(~"dark", ~"Dark")
        ]}.

theme_button(Theme, Label) ->
    {button,
        [
            {type, ~"button"},
            {class, [~"theme-", Theme, ~" rounded-md px-2.5 py-1 font-medium transition-colors"]},
            %% The page and the cheat sheet each hold a `theme` slot, so the
            %% switch sets it in every view; the page script remembers it.
            {az_click, [
                arizona_js:set_all(~"theme", Theme),
                arizona_js:dispatch_event(~"arizona:theme", #{theme => Theme})
            ]}
        ],
        [Label]}.

hero() ->
    {section, [{class, [?CONTAINER, ~" pt-20 pb-16 sm:pt-28"]}], [
        {p, [{class, ?EYEBROW}], [~"Erlang/OTP web framework"]},
        {h1,
            [
                {class,
                    ~"max-w-3xl text-4xl font-semibold leading-[1.1] tracking-tight text-ink sm:text-6xl"}
            ],
            [~"Real-time web apps in plain Erlang"]},
        {p, [{class, ?LEAD}], [
            ~"Write views as Erlang terms. Arizona renders them on the server, keeps each page ",
            ~"live over a WebSocket, and sends only the values that changed."
        ]},
        {'div', [{class, ~"mt-8 flex flex-wrap gap-3"}], [
            {button,
                [
                    {type, ~"button"},
                    {class, ?PRIMARY},
                    {az_click, arizona_js:show_modal(~"#quick-start")}
                ],
                [~"Get started"]},
            {a, [{href, ?DOCS}, {class, ?SECONDARY}], [~"Read the docs"]}
        ]},
        {p, [{class, ~"mt-6 text-sm text-muted"}], [
            ~"Arizona is in 0.x: the core is tested and in use, but the API can still change ",
            ~"between minor versions."
        ]}
    ]}.

example(Counter) ->
    {section, [{id, ~"example"}, {class, [?CONTAINER, ~" scroll-mt-20 py-16"]}], [
        {p, [{class, ?EYEBROW}], [~"Example"]},
        {h2, [{class, ?H2}], [~"A live counter, in full"]},
        {p, [{class, ?LEAD}], [
            ~"No JavaScript to write. A click is an event on the server, and the reply patches ",
            ~"the one number that changed."
        ]},
        {'div',
            [
                {class, ~"tabs mt-10 overflow-hidden rounded-xl border border-line bg-surface"},
                {'data-tab', ?local(~"tab", ~"view")}
            ],
            [
                {'div',
                    [
                        {role, ~"tablist"},
                        {class, ~"flex gap-1 border-b border-line px-3 text-sm text-muted"}
                    ],
                    [
                        tab(~"view", ~"counter.erl"),
                        tab(~"config", ~"sys.config"),
                        tab(~"static", ~"A static site")
                    ]},
                {'div', [{class, ~"panel panel-view"}], [code(~"code-view", Counter)]},
                {'div', [{class, ~"panel panel-config"}], [code(~"code-config", route_sample())]},
                {'div', [{class, ~"panel panel-static"}], [code(~"code-static", static_sample())]}
            ]}
    ]}.

tab(Tab, Label) ->
    {button,
        [
            {type, ~"button"},
            {role, ~"tab"},
            {class, [
                ~"tab-",
                Tab,
                ~" -mb-px border-b-2 border-transparent px-3 py-3 font-medium transition-colors hover:text-ink"
            ]},
            {az_click, arizona_js:set(~"tab", Tab)}
        ],
        [Label]}.

showcase() ->
    {section, [{id, ~"this-page"}, {class, ~"scroll-mt-16 border-y border-line bg-sunken"}], [
        {'div', [{class, [?CONTAINER, ~" py-16"]}], [
            {p, [{class, ?EYEBROW}], [~"This page"]},
            {h2, [{class, ?H2}], [~"Interactive, with no server"]},
            {p, [{class, ?LEAD}], [
                ~"This site is generated by ",
                {code, [{class, ~"font-mono text-[0.9em] text-ink"}], [~"arizona_static"]},
                ~" and served as plain files. Its controls are the same event attributes a live ",
                ~"page uses; Arizona's static client runs them in the browser, with no WebSocket."
            ]},
            {'div', [{class, ~"mt-10 grid gap-8 lg:grid-cols-[1fr_20rem]"}], [
                {ul,
                    [
                        {class,
                            ~"self-start divide-y divide-line rounded-xl border border-line bg-surface"}
                    ],
                    [
                        ?each(
                            fun(#{what := What, code := Code}) ->
                                {li, [{class, ~"px-5 py-4"}], [
                                    {p, [{class, ~"text-sm font-medium text-ink"}], [What]},
                                    {code,
                                        [
                                            {class,
                                                ~"mt-1 block font-mono text-[0.8125rem] text-teal"}
                                        ],
                                        [Code]}
                                ]}
                            end,
                            showcase_list()
                        )
                    ]},
                {'div', [], [
                    ?stateful(arizona_website_cheatsheet, #{id => ~"cheatsheet"}),
                    {button,
                        [
                            {type, ~"button"},
                            %% Hidden by the page script where the browser has
                            %% no Document Picture-in-Picture.
                            {az_hook, ~"NeedsPip"},
                            {class, [?SECONDARY, ~" mt-3 w-full justify-center"]},
                            {az_click,
                                arizona_js:request_pip(~"cheatsheet", #{width => 340, height => 520})}
                        ],
                        [~"Pop out the cheat sheet"]}
                ]}
            ]}
        ]}
    ]}.

features() ->
    {section, [{id, ~"features"}, {class, [?CONTAINER, ~" scroll-mt-20 py-16"]}], [
        {p, [{class, ?EYEBROW}], [~"Features"]},
        {h2, [{class, ?H2}], [~"Why Arizona"]},
        {'div', [{class, ~"mt-10 grid gap-x-10 gap-y-8 sm:grid-cols-2 lg:grid-cols-3"}], [
            ?each(
                fun(#{title := Title, text := Text}) ->
                    {'div', [{class, ~"border-t border-line pt-5"}], [
                        {h3, [{class, ~"font-semibold text-ink"}], [Title]},
                        {p, [{class, ~"mt-2 text-sm leading-relaxed text-muted"}], [Text]}
                    ]}
                end,
                feature_list()
            )
        ]}
    ]}.

adapters() ->
    {section, [{class, [?CONTAINER, ~" pb-20"]}], [
        {'div', [{class, ~"rounded-2xl border border-line bg-surface p-8 sm:p-10"}], [
            {p, [{class, ?EYEBROW}], [~"Servers"]},
            {h2, [{class, ~"text-2xl font-semibold tracking-tight text-ink"}], [
                ~"Runs on the server you already have"
            ]},
            {p, [{class, ~"mt-3 max-w-2xl text-muted"}], [
                ~"The HTTP and WebSocket server is an adapter package, so Arizona fits in beside ",
                ~"what you run today."
            ]},
            {'div', [{class, ~"mt-8 grid gap-4 md:grid-cols-3"}], [
                ?each(
                    fun(#{name := Name, text := Text, href := Href}) ->
                        {a,
                            [
                                {href, Href},
                                {class,
                                    ~"block rounded-xl border border-line p-5 transition-colors hover:border-accent"}
                            ],
                            [
                                {p, [{class, ~"font-semibold text-ink"}], [Name]},
                                {p, [{class, ~"mt-1 text-sm text-muted"}], [Text]}
                            ]}
                    end,
                    adapter_list()
                )
            ]}
        ]}
    ]}.

footer() ->
    {footer, [{class, ~"border-t border-line"}], [
        {'div',
            [
                {class, [
                    ?CONTAINER,
                    ~" flex flex-col gap-4 py-10 text-sm text-muted sm:flex-row sm:items-center sm:justify-between"
                ]}
            ],
            [
                {p, [], [~"Arizona Framework. Built with Arizona."]},
                {'div', [{class, ~"flex gap-6"}], [
                    {a, [{href, ?GITHUB}, {class, ?NAV_LINK}], [~"GitHub"]},
                    {a, [{href, ?DOCS}, {class, ?NAV_LINK}], [~"Documentation"]},
                    {a, [{href, ~"https://erlangforums.com"}, {class, ?NAV_LINK}], [
                        ~"Erlang Forums"
                    ]}
                ]}
            ]}
    ]}.

quick_start() ->
    {dialog,
        [
            {id, ~"quick-start"},
            {'aria-labelledby', ~"quick-start-title"},
            {class,
                ~"m-auto w-[min(44rem,calc(100%-2rem))] rounded-2xl border border-line bg-surface p-0 text-ink shadow-2xl"}
        ],
        [
            {'div', [{class, ~"flex items-center justify-between border-b border-line px-6 py-4"}],
                [
                    {h2, [{id, ~"quick-start-title"}, {class, ~"font-semibold"}], [~"Get started"]},
                    {button,
                        [
                            {type, ~"button"},
                            {'aria-label', ~"Close"},
                            {class,
                                ~"rounded-md px-2 py-1 text-muted hover:bg-sunken hover:text-ink"},
                            {az_click, arizona_js:close_modal(~"#quick-start")}
                        ],
                        [~"✕"]}
                ]},
            {ol, [{class, ~"space-y-6 px-6 py-6 text-sm"}], [
                step(~"1", ~"Add Arizona and a server adapter to rebar.config", [
                    code(~"code-deps", deps_sample())
                ]),
                step(~"2", ~"Route a view in config/sys.config", [
                    code(~"code-route", route_sample())
                ]),
                step(~"3", ~"Start a shell and open http://localhost:8080", [
                    code(~"code-run", ~"rebar3 shell")
                ])
            ]},
            {'div', [{class, ~"border-t border-line px-6 py-4 text-sm"}], [
                {a, [{href, ?QUICK_START}, {class, ~"font-medium text-accent hover:underline"}], [
                    ~"The full quick start, with the layout and the counter"
                ]}
            ]}
        ]}.

step(N, Title, Body) ->
    {li, [], [
        {p, [{class, ~"mb-2 flex items-center gap-2 font-medium"}], [
            {span,
                [
                    {class,
                        ~"flex h-5 w-5 items-center justify-center rounded-full bg-accent-soft text-xs text-accent"}
                ],
                [N]},
            Title
        ]},
        {'div', [{class, ~"overflow-hidden rounded-lg border border-line"}], Body}
    ]}.

%% A code block with a copy button. The button marks itself copied, which the
%% stylesheet shows; both are browser-side commands.
code(Id, Source) ->
    {'div', [{class, ~"group relative"}], [
        {button,
            [
                {type, ~"button"},
                {class,
                    ~"absolute top-2.5 right-2.5 rounded-md border border-white/10 bg-white/5 px-2 py-1 text-xs text-stone-300 opacity-0 transition-opacity group-hover:opacity-100 focus:opacity-100 data-[copied]:opacity-100"},
                {az_click, [
                    arizona_js:copy_to_clipboard(<<"#", Id/binary>>),
                    arizona_js:set_attr(<<"#", Id/binary, "-copy">>, ~"data-copied", ~"")
                ]},
                {id, <<Id/binary, "-copy">>}
            ],
            [
                {span, [{class, ~"in-data-[copied]:hidden"}], [~"Copy"]},
                {span, [{class, ~"hidden in-data-[copied]:inline"}], [~"Copied"]}
            ]},
        {pre, [{class, ~"language-erlang"}], [
            {code, [{id, Id}, {class, ~"language-erlang"}], [Source]}
        ]}
    ]}.

%% --------------------------------------------------------------------
%% Content
%% --------------------------------------------------------------------

showcase_list() ->
    [
        #{
            what => ~"The theme switch sets a value the browser owns",
            code => ~"arizona_js:set_all(~\"theme\", ~\"dark\")"
        },
        #{
            what => ~"Each tab sets the one the panels read",
            code => ~"arizona_js:set(~\"tab\", ~\"config\")"
        },
        #{
            what => ~"Get started opens a modal dialog",
            code => ~"arizona_js:show_modal(~\"#quick-start\")"
        },
        #{
            what => ~"Every code block copies itself",
            code => ~"arizona_js:copy_to_clipboard(~\"#code-view\")"
        },
        #{
            what => ~"The cheat sheet floats in its own window",
            code => ~"arizona_js:request_pip(~\"cheatsheet\")"
        }
    ].

feature_list() ->
    [
        #{
            title => ~"Live by default",
            text =>
                ~"The first load is plain HTML. After that each page stays live over a WebSocket, and PubSub pushes updates to every connected tab."
        },
        #{
            title => ~"Minimal updates",
            text =>
                ~"Templates split into static and dynamic parts at compile time. Statics go once; an update sends only the slots whose data changed."
        },
        #{
            title => ~"Plain Erlang templates",
            text =>
                ~"Views are {Tag, Attrs, Children} terms the compiler checks. Streams, client-owned state and SPA navigation are built in."
        },
        #{
            title => ~"BEAM reliability",
            text =>
                ~"Every live page is a supervised process. Deploys drain sessions gracefully and clients reconnect on their own."
        },
        #{
            title => ~"Beyond the browser",
            text =>
                ~"The same views and diff engine drive native Android and iOS clients, and render to a terminal over a TTY or SSH."
        },
        #{
            title => ~"Static when you want it",
            text =>
                ~"Generate pages like this one from the same views, with browser-side commands and no server to run."
        }
    ].

adapter_list() ->
    [
        #{
            name => ~"Roadrunner",
            text => ~"A pure-Erlang server with HTTP/1.1, HTTP/2 and HTTP/3.",
            href => ~"https://github.com/arizona-framework/arizona_roadrunner"
        },
        #{
            name => ~"Cowboy",
            text => ~"The classic Erlang server, TLS and HTTP/2 included.",
            href => ~"https://github.com/arizona-framework/arizona_cowboy"
        },
        #{
            name => ~"Nova",
            text => ~"Live Arizona pages inside a Nova application, beside its own routes.",
            href => ~"https://github.com/arizona-framework/arizona_nova"
        }
    ].

deps_sample() ->
    ~"""
    {deps, [
        {arizona, {git,
            "https://github.com/arizona-framework/arizona.git",
            {branch, "main"}}},
        {arizona_roadrunner, {git,
            "https://github.com/arizona-framework/arizona_roadrunner.git",
            {branch, "main"}}}
    ]}.
    """.

route_sample() ->
    ~"""
    [{arizona, [
        {server, #{
            adapter => arizona_roadrunner_server,
            transport_opts => [{port, 8080}],
            routes => [
                %% The page, its WebSocket and the client runtime.
                {live, ~"/", counter, #{
                    layouts => [{my_layout, render}]
                }},
                {ws, ~"/ws", #{}},
                {asset, ~"/assets", {priv_dir, arizona, "static/assets/js"}}
            ]
        }}
    ]}].
    """.

static_sample() ->
    ~"""
    %% Render views to HTML files. A layout that loads the static client
    %% (arizona-static.min.js) and calls start() keeps the browser-side
    %% commands working, with no server: this site is built this way.
    {Written, []} = arizona_static:generate(~"dist", [
        {home_page, ~"index.html"},
        {about_page, ~"about/index.html"}
    ], #{layouts => [{site_layout, render}]}).
    """.

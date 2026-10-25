-module(arizona_website_home_page).
-moduledoc """
The home page. Rendered once, at build time (`arizona_website:build/0`); the
sections are local element helpers the parse transform inlines.
""".
-include_lib("arizona/include/arizona_stateful.hrl").

-export([mount/1]).
-export([render/1]).

-define(GITHUB, ~"https://github.com/arizona-framework/arizona").
-define(DOCS, ~"https://hexdocs.pm/arizona").
-define(QUICK_START, ~"https://github.com/arizona-framework/arizona#quick-start").
-define(NOVA, ~"https://github.com/arizona-framework/arizona_nova").
-define(CARD,
    ~"bg-charcoal/50 backdrop-blur-sm rounded-xl p-8 border border-slate hover:border-arizona-teal/50 transition-all duration-300"
).
-define(REVEAL, ~"opacity-0 translate-y-8 transition-all duration-700 animate-on-scroll").

-spec mount(az:bindings()) -> az:mount_ret().
mount(_Bindings) ->
    %% The sample is the real module, compiled with the site.
    {ok, Example} = file:read_file("examples/counter.erl"),
    {#{id => ~"page", example => Example}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {'div',
            [
                %% Rendered once, at build time: no diff markers in the output.
                'az-nodiff',
                {id, ?get(id)},
                {class,
                    ~"min-h-screen bg-gradient-to-br from-obsidian via-charcoal to-arizona-terracotta"}
            ],
            [
                header(),
                hero(),
                features(),
                example(?get(example)),
                targets(),
                footer()
            ]}
    ).

%% --------------------------------------------------------------------
%% Sections
%% --------------------------------------------------------------------

header() ->
    {header,
        [
            {class,
                ~"sticky top-0 z-10 bg-obsidian/40 backdrop-blur-sm border-b border-slate shadow-lg"}
        ],
        [
            {nav, [{class, ~"container mx-auto px-6 py-4 flex items-center justify-between"}], [
                {a,
                    [
                        {href, ~"/"},
                        {class, ~"flex items-center gap-3 hover:opacity-80 transition-opacity"}
                    ],
                    [
                        logo(~"images/arizona_256x256.jpeg", ~"h-10 w-10 rounded-lg shadow-lg"),
                        {'div', [], [
                            {p, [{class, ~"text-xl font-bold text-pearl"}], [~"Arizona"]},
                            {p, [{class, ~"text-xs text-arizona-terracotta"}], [
                                ~"Real-time web framework"
                            ]}
                        ]}
                    ]},
                {'div', [{class, ~"hidden md:flex items-center gap-8"}], [
                    {a,
                        [
                            {href, ~"#features"},
                            {class, ~"text-silver hover:text-arizona-teal transition-colors"}
                        ],
                        [
                            ~"Features"
                        ]},
                    {a,
                        [
                            {href, ~"#example"},
                            {class, ~"text-silver hover:text-arizona-teal transition-colors"}
                        ],
                        [
                            ~"Example"
                        ]},
                    {a,
                        [
                            {href, ?DOCS},
                            {target, ~"_blank"},
                            {rel, ~"noopener noreferrer"},
                            {class, ~"text-silver hover:text-arizona-teal transition-colors"}
                        ],
                        [~"Docs"]},
                    {a,
                        [
                            {href, ?GITHUB},
                            {target, ~"_blank"},
                            {rel, ~"noopener noreferrer"},
                            {class,
                                ~"font-bold py-2 px-4 rounded-xl bg-gradient-to-r from-arizona-terracotta to-arizona-mesa text-pearl shadow-2xl shadow-arizona-terracotta/25 hover:scale-105 transition-all duration-300"}
                        ],
                        [~"GitHub"]}
                ]}
            ]}
        ]}.

hero() ->
    {section, [{class, [~"relative py-20 lg:py-32 px-6 ", ?REVEAL]}], [
        {'div', [{class, ~"container mx-auto text-center"}], [
            {'div', [{class, ~"flex justify-center mb-8"}], [
                {'div', [{class, ~"relative"}], [
                    logo(
                        ~"images/arizona_512x512.jpeg",
                        ~"h-32 w-32 lg:h-48 lg:w-48 rounded-2xl shadow-2xl shadow-arizona-terracotta/20 ring-4 ring-arizona-terracotta/30"
                    ),
                    {'div',
                        [
                            {class,
                                ~"absolute -inset-1 -z-10 bg-gradient-to-r from-arizona-terracotta to-arizona-mesa rounded-2xl blur opacity-30"}
                        ],
                        []}
                ]}
            ]},
            {h1, [{class, ~"text-4xl lg:text-7xl font-bold text-pearl mb-6 leading-tight"}], [
                ~"Build ",
                {span,
                    [
                        {class,
                            ~"bg-gradient-to-r from-arizona-teal to-arizona-terracotta bg-clip-text text-transparent"}
                    ],
                    [~"real-time"]},
                {br, []},
                ~"web applications in Erlang"
            ]},
            {p,
                [
                    {class,
                        ~"text-xl lg:text-2xl text-silver mb-10 max-w-3xl mx-auto leading-relaxed"}
                ],
                [
                    ~"Write views as plain Erlang. Arizona renders them on the server, keeps each page live over a WebSocket, and sends ",
                    {strong, [{class, ~"text-arizona-teal"}], [~"only what changed"]},
                    ~"."
                ]},
            {'div', [{class, ~"flex flex-col sm:flex-row gap-4 justify-center items-center mb-12"}],
                [
                    {a,
                        [
                            {href, ?QUICK_START},
                            {target, ~"_blank"},
                            {rel, ~"noopener noreferrer"},
                            {class,
                                ~"inline-block font-bold py-4 px-8 rounded-xl bg-gradient-to-r from-arizona-terracotta to-arizona-mesa text-pearl shadow-2xl shadow-arizona-terracotta/25 hover:scale-105 transition-all duration-300"}
                        ],
                        [~"Get started"]},
                    {a,
                        [
                            {href, ?DOCS},
                            {target, ~"_blank"},
                            {rel, ~"noopener noreferrer"},
                            {class,
                                ~"inline-block font-semibold py-4 px-8 rounded-xl bg-charcoal/50 hover:bg-slate/50 text-pearl border border-slate backdrop-blur-sm transition-all duration-300"}
                        ],
                        [~"Read the docs"]}
                ]},
            {p, [{class, ~"text-arizona-sand text-sm max-w-2xl mx-auto"}], [
                ~"Arizona is in 0.x: the core is tested and in use, but the API can still change between minor versions."
            ]}
        ]}
    ]}.

features() ->
    {section, [{id, ~"features"}, {class, [~"py-20 px-6 bg-obsidian/30 ", ?REVEAL]}], [
        {'div', [{class, ~"container mx-auto"}], [
            {h2, [{class, ~"text-4xl font-bold text-pearl text-center mb-16"}], [
                ~"Why ",
                {span, [{class, ~"text-arizona-teal"}], [~"Arizona?"]}
            ]},
            {'div', [{class, ~"grid md:grid-cols-2 lg:grid-cols-3 gap-8"}], [
                ?each(
                    fun(#{icon := Icon, title := Title, text := Text}) ->
                        {'div', [{class, ?CARD}], [
                            {'div',
                                [
                                    {class,
                                        ~"bg-gradient-to-r from-arizona-terracotta to-arizona-mesa w-12 h-12 rounded-lg flex items-center justify-center mb-6"}
                                ],
                                [
                                    {svg,
                                        [
                                            {class, ~"h-6 w-6 text-pearl"},
                                            {fill, ~"none"},
                                            {stroke, ~"currentColor"},
                                            {'viewBox', ~"0 0 24 24"},
                                            {'aria-hidden', ~"true"}
                                        ],
                                        [
                                            {path, [
                                                {stroke_linecap, ~"round"},
                                                {stroke_linejoin, ~"round"},
                                                {stroke_width, ~"2"},
                                                {d, Icon}
                                            ]}
                                        ]}
                                ]},
                            {h3, [{class, ~"text-xl font-bold text-pearl mb-4"}], [Title]},
                            {p, [{class, ~"text-silver"}], [Text]}
                        ]}
                    end,
                    feature_list()
                )
            ]}
        ]}
    ]}.

example(Source) ->
    {section, [{id, ~"example"}, {class, [~"py-20 px-6 ", ?REVEAL]}], [
        {'div', [{class, ~"container mx-auto max-w-4xl"}], [
            {h2, [{class, ~"text-4xl font-bold text-pearl text-center mb-4"}], [
                ~"A live counter, ",
                {span, [{class, ~"text-arizona-teal"}], [~"in full"]}
            ]},
            {p, [{class, ~"text-silver text-center mb-10 text-lg"}], [
                ~"No JavaScript to write. A click is an event on the server, and the reply patches the one number that changed."
            ]},
            {'div',
                [
                    {class,
                        ~"bg-obsidian/80 rounded-xl overflow-hidden border border-slate shadow-2xl hover:border-arizona-teal/50 transition-all duration-300"}
                ],
                [
                    {'div', [{class, ~"bg-charcoal px-4 py-2 border-b border-slate"}], [
                        {span, [{class, ~"text-silver text-sm"}], [~"counter.erl"]}
                    ]},
                    {pre,
                        [
                            {class, ~"text-sm leading-relaxed overflow-x-auto"},
                            {style, ~"margin: 0"}
                        ],
                        [
                            {code, [{class, ~"language-erlang"}], [Source]}
                        ]}
                ]}
        ]}
    ]}.

targets() ->
    {section, [{class, [~"py-20 px-6 bg-obsidian/30 ", ?REVEAL]}], [
        {'div', [{class, ~"container mx-auto text-center"}], [
            {h2, [{class, ~"text-4xl font-bold text-pearl mb-4"}], [
                ~"Runs on ",
                {span, [{class, ~"text-arizona-teal"}], [~"your stack"]}
            ]},
            {p, [{class, ~"text-silver mb-16 max-w-2xl mx-auto"}], [
                ~"The server is an adapter, so Arizona fits the application you already have."
            ]},
            {'div', [{class, ~"grid md:grid-cols-3 gap-8"}], [
                ?each(
                    fun(#{name := Name, text := Text, href := Href}) ->
                        {a,
                            [
                                {href, Href},
                                {target, ~"_blank"},
                                {rel, ~"noopener noreferrer"},
                                {class, [?CARD, ~" block"]}
                            ],
                            [
                                {p, [{class, ~"text-2xl font-bold text-arizona-teal mb-2"}], [Name]},
                                {p, [{class, ~"text-silver"}], [Text]}
                            ]}
                    end,
                    target_list()
                )
            ]}
        ]}
    ]}.

footer() ->
    {footer, [{class, ~"py-12 px-6 border-t border-slate bg-obsidian/50"}], [
        {'div', [{class, ~"container mx-auto text-center"}], [
            {'div', [{class, ~"flex items-center justify-center gap-3 mb-6"}], [
                logo(~"images/arizona_256x256.jpeg", ~"h-8 w-8 rounded-lg"),
                {span, [{class, ~"text-pearl font-bold text-lg"}], [~"Arizona Framework"]}
            ]},
            {p, [{class, ~"text-silver mb-4"}], [~"Real-time web applications on Erlang/OTP"]},
            {'div',
                [{class, ~"flex flex-col sm:flex-row justify-center items-center gap-4 sm:gap-6"}],
                [
                    {a,
                        [
                            {href, ?GITHUB},
                            {class, ~"text-silver hover:text-arizona-teal transition-colors"}
                        ],
                        [
                            ~"GitHub"
                        ]},
                    {a,
                        [
                            {href, ?DOCS},
                            {class, ~"text-silver hover:text-arizona-teal transition-colors"}
                        ],
                        [
                            ~"Documentation"
                        ]},
                    {a,
                        [
                            {href, ~"https://erlangforums.com"},
                            {class, ~"text-silver hover:text-arizona-teal transition-colors"}
                        ],
                        [~"Erlang Forums"]}
                ]},
            {'div', [{class, ~"mt-8 pt-8 border-t border-slate text-sm"}], [
                {p, [{class, ~"text-arizona-sand"}], [~"Built with Arizona 🌵"]},
                {p, [{class, ~"text-silver mt-2"}], [
                    ~"This site is powered by ",
                    {a,
                        [
                            {href, ~"https://www.netlify.com"},
                            {target, ~"_blank"},
                            {rel, ~"noopener noreferrer"},
                            {class, ~"text-arizona-teal hover:text-arizona-sage transition-colors"}
                        ],
                        [~"Netlify"]}
                ]}
            ]}
        ]}
    ]}.

logo(Src, Class) ->
    {img, [
        {src, Src},
        {alt, ~"Arizona Framework"},
        {class, Class},
        {loading, ~"lazy"},
        {decoding, ~"async"}
    ]}.

%% --------------------------------------------------------------------
%% Content
%% --------------------------------------------------------------------

feature_list() ->
    [
        #{
            icon => ~"M13 10V3L4 14h7v7l9-11h-7z",
            title => ~"Live by default",
            text =>
                ~"The first load is plain HTML. After that, each page stays live over a WebSocket, and PubSub pushes updates to every connected tab."
        },
        #{
            icon =>
                ~"M9 19v-6a2 2 0 00-2-2H5a2 2 0 00-2 2v6a2 2 0 002 2h2a2 2 0 002-2zm0 0V9a2 2 0 012-2h2a2 2 0 012 2v10m-6 0a2 2 0 002 2h2a2 2 0 002-2m0 0V5a2 2 0 012-2h2a2 2 0 012 2v14a2 2 0 01-2 2h-2a2 2 0 01-2-2z",
            title => ~"Minimal updates",
            text =>
                ~"Templates are split into static and dynamic parts at compile time. Statics go once; an update sends only the slots whose data changed."
        },
        #{
            icon => ~"M10 20l4-16m4 4l4 4-4 4M6 16l-4-4 4-4",
            title => ~"Plain Erlang templates",
            text =>
                ~"Views are {Tag, Attrs, Children} terms, checked by the compiler. Streams, client-owned state and SPA navigation are built in."
        },
        #{
            icon =>
                ~"M9 12l2 2 4-4m5.618-4.016A11.955 11.955 0 0112 2.944a11.955 11.955 0 01-8.618 3.04A12.02 12.02 0 003 9c0 5.591 3.824 10.29 9 11.622 5.176-1.332 9-6.03 9-11.622 0-1.042-.133-2.052-.382-3.016z",
            title => ~"BEAM reliability",
            text =>
                ~"Every live page is a supervised process. Deploys drain sessions gracefully and clients reconnect on their own."
        },
        #{
            icon => ~"M12 18h.01M8 21h8a2 2 0 002-2V5a2 2 0 00-2-2H8a2 2 0 00-2 2v14a2 2 0 002 2z",
            title => ~"Beyond the browser",
            text =>
                ~"The same views and diff engine also drive native Android and iOS clients, and render to a terminal over a TTY or SSH."
        },
        #{
            icon =>
                ~"M4 4v5h.582m15.356 2A8.001 8.001 0 004.582 9m0 0H9m11 11v-5h-.581m0 0a8.003 8.003 0 01-15.357-2m15.357 2H15",
            title => ~"Fast feedback",
            text =>
                ~"Save a file and the dev reloader recompiles it and refreshes the page. Static pages, like this one, are generated from the same views."
        }
    ].

target_list() ->
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
            href => ?NOVA
        }
    ].

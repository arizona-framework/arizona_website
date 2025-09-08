-module(arizona_web_home_page).
-behaviour(arizona_stateful).
-compile({parse_transform, arizona_parse_transform}).
-export([mount/1]).
-export([render/1]).

mount(Bindings) ->
    arizona_stateful:new(?MODULE, Bindings).

render(Bindings) ->
    arizona_template:from_string(~"""
    <div
        id="{arizona_template:get_binding(id, Bindings)}"
        class="min-h-screen bg-gradient-to-br from-obsidian via-charcoal to-arizona-terracotta"
    >
        {header()}
        {hero()}
        {features()}
        {code_example()}
        <div class="opacity-0 translate-y-8 transition-all duration-700 animate-on-scroll">
            {performance_stats()}
            {footer()}
        </div>
    </div>
    """).

%% Header navigation component - pure function, no bindings needed
header() ->
    arizona_template:from_string(~""""
    <header class="sticky top-0 z-10 bg-obsidian bg-opacity-20 backdrop-blur-sm border-b border-slate shadow-lg">
        <nav class="container mx-auto px-6 py-4">
            <div class="flex items-center justify-between">
                <a href="/" class="flex items-center space-x-3 hover:opacity-80 transition-opacity duration-300">
                    {arizona_template:render_stateless(arizona_web_components, arizona_image, #{
                        type => logo,
                        size => medium,
                        alt => ~"Arizona Framework",
                        classes => ~"h-10 w-10 rounded-lg shadow-lg"
                    })}
                    <div>
                        <h1 class="text-xl font-bold text-pearl">Arizona</h1>
                        <p class="text-xs text-arizona-terracotta">Real-time Web Framework</p>
                    </div>
                </a>
                <div class="hidden md:flex items-center space-x-8">
                    {arizona_template:render_list(fun(Link) ->
                        arizona_template:from_string(~"""
                        {arizona_template:render_stateless(arizona_web_components, nav_link, Link)}
                        """)
                    end, header_links())}
                </div>
            </div>
        </nav>
    </header>
    """").

header_links() ->
    [
        #{href => ~"#features", text => ~"Features"},
        #{href => ~"#examples", text => ~"Examples"},
        #{
            href => ~"https://github.com/arizona-framework/arizona",
            text => ~"GitHub",
            target => ~"_blank",
            rel => ~"noopener noreferrer",
            classes => [
                ~"font-bold py-2 px-4 rounded-xl transition-all duration-300 bg-gradient-to-r ",
                ~"from-arizona-terracotta to-arizona-mesa hover:from-arizona-mesa hover:to-arizona-terracotta ",
                ~"text-pearl shadow-2xl shadow-arizona-terracotta/25 transform hover:scale-105"
            ]
        }
    ].

%% Hero section component
hero() ->
    arizona_template:from_string(~""""
    <section class="relative py-20 lg:py-32 px-6 opacity-0 translate-y-8 transition-all duration-700 animate-on-scroll">
        <div class="container mx-auto text-center">
            <!-- Large Arizona Logo -->
            <div class="flex justify-center mb-8">
                <div class="relative">
                    {arizona_template:render_stateless(arizona_web_components, arizona_image, #{
                        type => logo,
                        size => large,
                        alt => ~"Arizona Framework Logo",
                        classes => [
                            ~"h-32 w-32 lg:h-48 lg:w-48 rounded-2xl shadow-2xl shadow-arizona-terracotta/20 ",
                            ~"ring-4 ring-arizona-terracotta/30 hover:scale-105 transition-transform duration-300"
                        ]
                    })}
                    <div class="{[
                        ~"absolute -inset-1 bg-gradient-to-r from-arizona-terracotta ",
                        ~"to-arizona-mesa rounded-2xl blur opacity-30"
                    ]}"></div>
                </div>
            </div>

            <!-- Compelling Headlines -->
            <h1 class="text-4xl lg:text-7xl font-bold text-pearl mb-6 leading-tight">
                Build <span class="{[
                    ~"bg-gradient-to-r from-arizona-teal to-arizona-terracotta ",
                    ~"bg-clip-text text-transparent"
                ]}">Real-time</span>
                <br>Web Applications
            </h1>

            <p class="text-xl lg:text-2xl text-silver mb-8 max-w-4xl mx-auto leading-relaxed">
                Arizona is a modern Erlang web framework that delivers
                <strong class="text-arizona-teal">real-time interactivity</strong>
                with the rock-solid reliability of the BEAM virtual machine.
            </p>

            <!-- Work in Progress Warning -->
            <div class="{[
                ~"bg-gradient-to-r from-arizona-gold/20 to-arizona-terracotta/20 border border-arizona-gold/50 ",
                ~"rounded-xl p-6 mb-12 max-w-2xl mx-auto backdrop-blur-sm"
            ]}">
                <div class="flex items-center justify-center space-x-3 mb-3">
                    <div class="text-arizona-gold text-2xl">⚠️</div>
                    <h3 class="text-lg font-semibold text-arizona-gold">Work in Progress</h3>
                </div>
                <p class="text-silver text-sm leading-relaxed">
                    Arizona is currently in active development. This framework is experimental
                    and not yet ready for production use.
                    <br>
                    <a
                        href="https://github.com/arizona-framework/arizona"
                        class="text-arizona-gold hover:text-arizona-sage underline transition-colors"
                    >
                        Follow our progress and contribute at GitHub
                    </a>.
                </p>
            </div>

            <!-- CTA Buttons -->
            <div class="flex flex-col sm:flex-row gap-4 justify-center items-center">
                {arizona_template:render_list(fun(Button) ->
                    arizona_template:from_string(~"""
                    {arizona_template:render_stateless(arizona_web_components, cta_button, Button)}
                    """)
                end, cta_buttons())}
            </div>
        </div>
    </section>
    """").

cta_buttons() ->
    [
        #{
            text => ~"Get Started",
            type => primary,
            href => ~"https://github.com/arizona-framework/arizona?tab=readme-ov-file#quick-start"
        },
        #{
            text => ~"View Documentation",
            type => secondary,
            href => ~"https://github.com/arizona-framework/arizona/blob/main/README.md"
        }
    ].

%% Features section component
features() ->
    arizona_template:from_string(~""""
    <section id="features" class="py-20 px-6 bg-obsidian bg-opacity-30 opacity-0 translate-y-8 transition-all duration-700 animate-on-scroll">
        <div class="container mx-auto">
            <h2 class="text-4xl font-bold text-pearl text-center mb-16">
                Why Choose <span class="text-arizona-teal">Arizona?</span>
            </h2>

            <div class="grid md:grid-cols-2 lg:grid-cols-3 gap-8">
                {arizona_template:render_list(fun(Feature) ->
                    arizona_template:from_string(~"""
                    {arizona_template:render_stateless(arizona_web_components, feature_card, Feature)}
                    """)
                end, feature_list())}
            </div>
        </div>
    </section>
    """").

feature_list() ->
    [
        #{
            icon => arizona_template:render_stateless(arizona_web_components, svg_icon, #{type => lightning}),
            title => ~"Real-time Updates",
            description => [
                ~"Built-in PubSub system enables real-time updates across connected ",
                ~"clients with automatic DOM synchronization."
            ]
        },
        #{
            icon => arizona_template:render_stateless(arizona_web_components, svg_icon, #{type => chart}),
            title => ~"BEAM Performance",
            description => [
                ~"Built on Erlang/OTP, Arizona inherits the legendary fault-tolerance, ",
                ~"concurrency, and performance of the BEAM virtual machine."
            ]
        },
        #{
            icon => arizona_template:render_stateless(arizona_web_components, svg_icon, #{type => code}),
            title => ~"Developer Experience",
            description => [
                ~"Clean template syntax, hot code reloading, and comprehensive development ",
                ~"tools make building web applications a joy."
            ]
        }
    ].

%% Code example component
code_example() ->
    arizona_template:from_string(~""""
    <section
        id="examples"
        class="py-20 px-6 opacity-0 translate-y-8 transition-all duration-700 animate-on-scroll"
    >
        <div class="container mx-auto">
            <div class="max-w-4xl mx-auto">
                <h2 class="text-4xl font-bold text-pearl text-center mb-12">
                    See Arizona in <span class="text-arizona-teal">Action</span>
                </h2>

                <div class="{[
                    ~"bg-obsidian bg-opacity-80 rounded-xl overflow-hidden border border-slate ",
                    ~"shadow-2xl hover:border-arizona-teal/50 hover:shadow-arizona-teal/20 ",
                    ~"transition-all duration-300 group"
                ]}">
                    <div class="{[
                        ~"bg-charcoal px-4 py-2 border-b border-slate flex items-center ",
                        ~"justify-between group-hover:bg-charcoal/80 transition-colors duration-300"
                    ]}">
                        <span class="{[
                            ~"text-silver text-sm group-hover:text-arizona-teal transition-colors ",
                            ~"duration-300"
                        ]}">
                            counter.erl
                        </span>
                    </div>
                    <div>
                        <pre class="text-sm leading-relaxed overflow-x-auto" style="margin: 0;">
                            <code class="language-erlang">
    {                           % Code is flush-left to align with closing triple-quotes
                                % preserving proper indentation in the example output
                                html_encode(example())}
                            </code>
                        </pre>
                    </div>
                </div>

                <p class="text-silver text-center mt-8 text-lg">
                    Interactive components you can actually understand.
                    <span class="text-arizona-teal font-semibold">No macros, no magic, no tears.</span>
                </p>
            </div>
        </div>
    </section>
    """").

%% HTML encoding helper for code examples
html_encode(Text) ->
    ReplacementList = [
        {"&", "\\&amp;"},
        {"<", "\\&lt;"},
        {">", "\\&gt;"},
        {"\"", "\\&quot;"},
        {"'", "\\&#39;"}
    ],
    lists:foldl(fun({Pattern, Replacement}, Acc) ->
        re:replace(Acc, Pattern, Replacement, [global, {return, binary}])
    end, Text, ReplacementList).

example() ->
    ~""""
    -module(counter).
    -behaviour(arizona_stateful).
    -compile({parse_transform, arizona_parse_transform}).

    -export([mount/1, render/1, handle_event/3]).

    mount(Bindings) ->
        arizona_stateful:new(?MODULE, Bindings#{
            count => maps:get(count, Bindings, 0)
        }).

    render(Bindings) ->
        arizona_template:from_string(~"""
        <div class="text-center">
            <span class="text-4xl font-bold">
                {arizona_template:get_binding(count, Bindings)}
            </span>
            <button
                class="btn-primary"
                onclick="arizona.sendEventTo(
                    '{arizona_template:get_binding(id, Bindings)}',
                    'increment'
                )"
            >
                +
            </button>
        </div>
        """).

    handle_event(~"increment", _Params, State) ->
        Count = arizona_stateful:get_binding(count, State),
        UpdatedState = arizona_stateful:put_binding(count, Count + 1, State),
        {noreply, UpdatedState}.
    """".

%% Performance stats component
performance_stats() ->
    arizona_template:from_string(~""""
    <section class="py-20 px-6 bg-obsidian bg-opacity-30">
        <div class="container mx-auto text-center">
            <h2 class="text-4xl font-bold text-pearl mb-16">
                Built for <span class="text-arizona-teal">Performance</span>
            </h2>

            <div class="grid md:grid-cols-3 gap-8">
                {arizona_template:render_list(fun(Stat) ->
                    arizona_template:from_string(~"""
                    {arizona_template:render_stateless(arizona_web_components, stat_card, Stat)}
                    """)
                end, performance_stats_list())}
            </div>
        </div>
    </section>
    """").

performance_stats_list() ->
    [
        #{
            value => ~"Millions",
            label => ~"Lightweight Processes",
            description => ~"BEAM VM capability"
        },
        #{
            value => ~"Instant",
            label => ~"Hot Reloading",
            description => ~"Zero downtime updates"
        },
        #{
            value => ~"Fault-Tolerant",
            label => ~"By Design",
            description => ~"Let it crash philosophy"
        }
    ].

%% Footer component
footer() ->
    arizona_template:from_string(~""""
    <footer class="py-12 px-6 border-t border-slate bg-obsidian bg-opacity-50">
        <div class="container mx-auto text-center">
            <div class="flex items-center justify-center space-x-3 mb-6">
                {arizona_template:render_stateless(arizona_web_components, arizona_image, #{
                    type => logo,
                    size => small,
                    alt => ~"Arizona Framework",
                    classes => ~"h-8 w-8 rounded-lg"
                })}
                <span class="text-pearl font-bold text-lg">Arizona Framework</span>
            </div>

            <p class="text-silver mb-4">
                Real-time web applications powered by Erlang/OTP
            </p>

            <div class="flex flex-col sm:flex-row justify-center items-center gap-4 sm:gap-6">
                {arizona_template:render_list(fun(Link) ->
                    arizona_template:from_string(~"""
                    {arizona_template:render_stateless(arizona_web_components, nav_link, Link)}
                    """)
                end, footer_links())}
            </div>

            <div class="mt-8 pt-8 border-t border-slate text-center">
                <p class="text-arizona-sand text-sm">
                    <span class="block sm:inline">© 2025 Arizona Framework.</span>
                    <span class="block sm:inline sm:ml-1">Built with Arizona 🌵</span>
                </p>
            </div>
        </div>
    </footer>
    """").

footer_links() ->
    [
        #{
            href => ~"https://github.com/arizona-framework/arizona",
            text => ~"GitHub",
            target => ~"_blank",
            rel => ~"noopener noreferrer"
        },
        #{href => ~"#", text => ~"Documentation"},
        #{href => ~"#", text => ~"Community"}
    ].

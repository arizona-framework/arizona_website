-module(arizona_web_components).
-compile({parse_transform, arizona_parse_transform}).
-export([
    feature_card/1,
    stat_card/1,
    nav_link/1,
    cta_button/1,
    svg_icon/1,
    arizona_image/1
]).

%% Feature card component - used 3 times in features section
feature_card(Bindings) ->
    arizona_template:from_string(~"""
    <div class="{[
        ~"bg-charcoal/50 backdrop-blur-sm rounded-xl p-8 border border-slate ",
        ~"hover:border-arizona-teal/50 transition-all duration-300"
    ]}">
        <div class="{[
            ~"bg-gradient-to-r from-arizona-terracotta to-arizona-mesa w-12 h-12 ",
            ~"rounded-lg flex items-center justify-center mb-6"
        ]}">
            {arizona_template:get_binding(icon, Bindings)}
        </div>
        <h3 class="text-xl font-bold text-pearl mb-4">
            {arizona_template:get_binding(title, Bindings)}
        </h3>
        <p class="text-silver">
            {arizona_template:get_binding(description, Bindings)}
        </p>
    </div>
    """).

%% Stat card component - used 3 times in performance stats section
stat_card(Bindings) ->
    arizona_template:from_string(~"""
    <div class="{[
        ~"bg-charcoal/50 backdrop-blur-sm rounded-xl p-8 border border-slate ",
        ~"hover:border-arizona-teal/50 transition-all duration-300"
    ]}">
        <div class="text-4xl font-bold text-arizona-teal mb-2">
            {arizona_template:get_binding(value, Bindings)}
        </div>
        <div class="text-pearl font-semibold mb-2">
            {arizona_template:get_binding(label, Bindings)}
        </div>
        <div class="text-silver">
            {arizona_template:get_binding(description, Bindings)}
        </div>
    </div>
    """).

%% Navigation link component - used 4 times (header + footer)
nav_link(Bindings) ->
    arizona_template:from_string(~"""
    <a
        href="{arizona_template:get_binding(href, Bindings)}"
        class="{arizona_template:get_binding(classes, Bindings, fun() ->
            ~"text-silver hover:text-arizona-teal focus:text-arizona-teal focus:outline-none focus:ring-2 focus:ring-arizona-teal/50 transition-colors duration-300"
        end)}"
        {
            case arizona_template:find_binding(target, Bindings) of
                {ok, Target} ->
                    [~"target=\"", Target, ~"\""];
                error ->
                    ~""
            end
        }
        {
            case arizona_template:find_binding(rel, Bindings) of
                {ok, Rel} ->
                    [~"rel=\"", Rel, ~"\""];
                error ->
                    ~""
            end
        }
    >
        {arizona_template:get_binding(text, Bindings)}
    </a>
    """).

%% CTA button component - used 2 times in hero section (renders as styled link)
cta_button(Bindings) ->
    ButtonType = arizona_template:get_binding(type, Bindings, fun() -> primary end),
    arizona_template:from_string(~"""
    <a
        href="{arizona_template:get_binding(href, Bindings)}"
        target="_blank"
        rel="noopener noreferrer"
        class="{[
            ~"inline-block font-bold py-4 px-8 rounded-xl transition-all duration-300 cursor-pointer ",
            ~"focus:outline-none focus:ring-2 focus:ring-arizona-teal/50 focus:ring-offset-2 focus:ring-offset-obsidian ",
            button_classes(ButtonType),
            arizona_template:get_binding(extra_classes, Bindings, fun() -> ~"" end)
        ]}"
    >
        {arizona_template:get_binding(text, Bindings)}
    </a>
    """).

button_classes(primary) ->
    [
        ~"bg-gradient-to-r from-arizona-terracotta to-arizona-mesa hover:from-arizona-mesa ",
        ~"hover:to-arizona-terracotta text-pearl shadow-2xl shadow-arizona-terracotta/25 transform hover:scale-105"
    ];
button_classes(secondary) ->
    ~"bg-charcoal/50 hover:bg-slate/50 text-pearl font-semibold border border-slate backdrop-blur-sm".

%% SVG icon component - used for feature cards
svg_icon(Bindings) ->
    arizona_template:from_string(~"""
    <svg class="h-6 w-6 text-pearl" fill="none" stroke="currentColor" viewBox="0 0 24 24">
        <path
            stroke-linecap="round"
            stroke-linejoin="round"
            stroke-width="2"
            d="{icon_path(arizona_template:get_binding(type, Bindings))}"
        />
    </svg>
    """).

icon_path(lightning) ->
    ~"M13 10V3L4 14h7v7l9-11h-7z";
icon_path(chart) ->
    [
        ~"M9 19v-6a2 2 0 00-2-2H5a2 2 0 00-2 2v6a2 2 0 002 2h2a2 2 0 002-2zm0 0V9a2 2 0 012-2h2a2 2 0 012 ",
        ~"2v10m-6 0a2 2 0 002 2h2a2 2 0 002-2m0 0V5a2 2 0 012-2h2a2 2 0 012 2v14a2 2 0 01-2 2h-2a2 2 0 01-2-2z"
    ];
icon_path(code) ->
    ~"M10 20l4-16m4 4l4 4-4 4M6 16l-4-4 4-4".

%% Arizona image component - resolves image paths
arizona_image(Bindings) ->
    arizona_template:from_string(~"""
    <img
        src="{arizona_image_path(
            arizona_template:get_binding(type, Bindings),
            arizona_template:get_binding(size, Bindings)
        )}"
        alt="{arizona_template:get_binding(alt, Bindings)}"
        class="{arizona_template:get_binding(classes, Bindings)}"
    >
    """).

arizona_image_path(logo, small) -> ~"images/arizona_256x256.jpeg";
arizona_image_path(logo, medium) -> ~"images/arizona_256x256.jpeg";
arizona_image_path(logo, large) -> ~"images/arizona_512x512.jpeg".


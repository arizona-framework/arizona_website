-module(arizona_website_layout).
-moduledoc "The page shell: head metadata, styles and scripts around the page.".
-include_lib("arizona/include/arizona_stateless.hrl").

-export([render/1]).

-define(DESCRIPTION,
    ~"Arizona is a real-time web framework for Erlang/OTP: views render on the server, and only what changed goes over the wire."
).
-define(IMAGE,
    ~"https://github.com/arizona-framework/arizona_website/raw/main/priv/static/images/arizona_512x512.jpeg"
).

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html([
        ~"<!DOCTYPE html>",
        {html, [{class, ~"scroll-smooth"}, {lang, ~"en"}], [
            {head, [], [
                {meta, [{charset, ~"UTF-8"}]},
                {meta, [{name, ~"viewport"}, {content, ~"width=device-width, initial-scale=1.0"}]},
                {title, [], [~"Arizona Framework"]},
                {meta, [{name, ~"description"}, {content, ?DESCRIPTION}]},
                {meta, [
                    {name, ~"keywords"},
                    {content,
                        ~"erlang, otp, beam, web framework, real-time, websocket, server-side rendering"}
                ]},
                {link, [
                    {rel, ~"icon"},
                    {type, ~"image/png"},
                    {href, ~"favicon-96x96.png"},
                    {sizes, ~"96x96"}
                ]},
                {link, [{rel, ~"icon"}, {type, ~"image/svg+xml"}, {href, ~"favicon.svg"}]},
                {link, [{rel, ~"shortcut icon"}, {href, ~"favicon.ico"}]},
                {link, [
                    {rel, ~"apple-touch-icon"}, {sizes, ~"180x180"}, {href, ~"apple-touch-icon.png"}
                ]},
                {meta, [{name, ~"apple-mobile-web-app-title"}, {content, ~"Arizona"}]},
                {link, [{rel, ~"manifest"}, {href, ~"site.webmanifest"}]},
                {meta, [{name, ~"theme-color"}, {content, ~"#d2691e"}]},
                {meta, [{property, ~"og:type"}, {content, ~"website"}]},
                {meta, [{property, ~"og:url"}, {content, ~"https://arizona-framework.org/"}]},
                {meta, [{property, ~"og:title"}, {content, ~"Arizona Framework"}]},
                {meta, [{property, ~"og:description"}, {content, ?DESCRIPTION}]},
                {meta, [{property, ~"og:image"}, {content, ?IMAGE}]},
                {meta, [{name, ~"twitter:card"}, {content, ~"summary_large_image"}]},
                {meta, [{name, ~"twitter:title"}, {content, ~"Arizona Framework"}]},
                {meta, [{name, ~"twitter:description"}, {content, ?DESCRIPTION}]},
                {meta, [{name, ~"twitter:image"}, {content, ?IMAGE}]},
                {link, [{rel, ~"stylesheet"}, {href, ~"assets/app.css"}]},
                {script, [{src, ~"assets/main.js"}], []},
                {script, [{src, ~"assets/prism.js"}], []}
            ]},
            {body, [{class, ~"bg-obsidian min-h-screen"}], [?inner_content]}
        ]}
    ]).

-module(arizona_web_layout).
-compile({parse_transform, arizona_parse_transform}).
-export([render/1]).

render(Bindings) ->
    arizona_template:from_string(~""""
    <!DOCTYPE html>
    <html class="scroll-smooth">
    <head>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <title>{arizona_template:get_binding(title, Bindings)}</title>
        <meta name="description" content="{[
            ~"Arizona Framework - Build real-time web applications with Erlang/OTP. Modern, ",
            ~"fault-tolerant web framework with real-time interactivity and BEAM performance."
        ]}">
        <meta name="keywords" content="{[
            ~"erlang, web framework, real-time, otp, beam, ",
            ~"functional programming, fault tolerance"
        ]}">
        <meta name="author" content="Arizona Framework Team">

        {% Open Graph / Facebook }
        <meta property="og:type" content="website">
        <meta property="og:url" content="https://arizona-framework.github.io/">
        <meta property="og:title" content="Arizona Framework - Real-time Web Applications">
        <meta property="og:description" content="{[
            ~"Build real-time web applications with Erlang/OTP. Modern, fault-tolerant ",
            ~"web framework with real-time interactivity and BEAM performance."
        ]}">
        <meta property="og:image" content="{[
            ~"https://github.com/arizona-framework/arizona_web/raw/main",
            ~"/priv/static/images/arizona_512x512.jpeg"
        ]}">

        {% Twitter }
        <meta property="twitter:card" content="summary_large_image">
        <meta property="twitter:url" content="https://arizona-framework.github.io/">
        <meta property="twitter:title" content="Arizona Framework - Real-time Web Applications">
        <meta property="twitter:description" content="{[
            ~"Build real-time web applications with Erlang/OTP. Modern, fault-tolerant ",
            ~"web framework with real-time interactivity and BEAM performance."
        ]}">
        <meta property="twitter:image" content="{[
            ~"https://github.com/arizona-framework/arizona_web/raw/main",
            ~"/priv/static/images/arizona_512x512.jpeg"
        ]}">

        <link rel="stylesheet" href="assets/app.css">
        <script src="assets/main.js"></script>
        <script src="assets/prism.js"></script>
        {
            case arizona_template:get_binding(env, Bindings, fun() -> prod end) of
                dev ->
                    ~"""
                    <script src="assets/dev.js" type="module" async></script>
                    """;
                prod ->
                    ~""
            end
        }
    </head>
    <body class="bg-gradient-to-br from-gray-900 via-gray-800 to-orange-900 min-h-screen">
        {arizona_template:render_slot(arizona_template:get_binding(main_content, Bindings))}
    </body>
    </html>
    """").

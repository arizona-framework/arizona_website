-module(arizona_web_layout).
-compile({parse_transform, arizona_parse_transform}).
-export([render/1]).

render(Bindings) ->
    arizona_template:from_string(~""""
    <!DOCTYPE html>
    <html>
    <head>
        <title>{arizona_template:get_binding(title, Bindings)}</title>
        <link rel="stylesheet" href="assets/app.css">
        {
            case arizona_template:get_binding(env, Bindings, fun() -> prod end) of
                dev ->
                    ~"""
                    <script src="assets/main.js" type="module" async></script>
                    """;
                prod ->
                    ~""
            end
        }
    </head>
    <body class="bg-gray-100 min-h-screen">
        <div class="container mx-auto px-4 py-8">
            {arizona_template:render_slot(arizona_template:get_binding(main_content, Bindings))}
        </div>
    </body>
    </html>
    """").

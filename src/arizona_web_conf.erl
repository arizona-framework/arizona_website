-module(arizona_web_conf).

-export([arizona/0]).

-spec arizona() -> Config when
    Config :: arizona:config().
arizona() ->
    #{
        server => #{
            routes => routes()
        },
        watcher => #{
            enabled => true,
            rules => [
                #{
                    directories => ["assets/css", "src"],
                    patterns => [".*\\.css$", ".*\\.erl$"],
                    callback => fun compile_css/1
                },
                #{
                    directories => ["src"],
                    patterns => [".*\\.erl$", ".*\\.hrl$"],
                    callback => fun compile_erl/1
                }
            ]
        }
    }.

routes() ->
    [
        {asset, ~"/favicon.ico", {priv_file, arizona_web, ~"static/favicon.ico"}},
        {asset, ~"/robots.txt", {priv_file, arizona_web, ~"static/robots.txt"}},
        {asset, ~"/assets/main.js", {priv_file, arizona_web, ~"static/assets/main.js"}},
        {asset, ~"/assets/prism.js", {priv_file, arizona_web, ~"static/assets/prism.js"}},
        {asset, ~"/assets/dev.js", {priv_file, arizona_web, ~"static/assets/dev.js"}},
        {asset, ~"/assets/app.css", {priv_file, arizona_web, ~"static/assets/app.css"}},
        {asset, ~"/assets", {priv_dir, arizona, ~"static/assets"}},
        {asset, ~"/images/arizona_256x256.jpeg", {priv_file, arizona_web, ~"static/images/arizona_256x256.jpeg"}},
        {asset, ~"/images/arizona_512x512.jpeg", {priv_file, arizona_web, ~"static/images/arizona_512x512.jpeg"}},
        {asset, ~"/images", {priv_dir, arizona_web, ~"static/images"}},
        {websocket, ~"/live"},
        {view, ~"/", arizona_web_view, {arizona_web_home_page, #{}}}
    ].

compile_erl(Files) ->
    try
        CompileResult = os:cmd("rebar3 compile", #{exception_on_failure => true}),
        ok = io:format("~ts", [CompileResult]),
        {ok, Cwd0} = file:get_cwd(),
        Cwd = Cwd0 ++ "/",
        ErlFiles = [F || F <- Files, filename:extension(F) =:= ".erl"],
        lists:foreach(
            fun(AbsFilename) ->
                Filename = case string:prefix(AbsFilename, Cwd) of
                    nomatch -> AbsFilename;
                    Suffix -> Suffix
                end,
                ok = io:format("===> Reloading module: ~s~n", [Filename]),
                BaseName = filename:basename(Filename, ".erl"),
                Module = list_to_existing_atom(BaseName),
                code:purge(Module),
                code:load_file(Module)
            end,
            ErlFiles
        ),
        ok = io:format("===> Reloading page~n"),
        arizona_pubsub:broadcast(~"reload", erl)
    catch
        error:{command_failed, ResultBeforeFailure, _ExitCode} ->
            io:format("~ts~n", [ResultBeforeFailure])
    end.

compile_css(_Files) ->
    try
        CompileResult = os:cmd("npm run build-css", #{exception_on_failure => true}),
        ok = io:format("~ts", [CompileResult]),
        arizona_pubsub:broadcast(~"reload", css)
    catch
        error:{command_failed, ResultBeforeFailure, _ExitCode} ->
            io:format("CSS build failed:~n~ts~n", [ResultBeforeFailure])
    end.

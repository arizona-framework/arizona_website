-module(arizona_website_conf).

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
        %% Website routes
        {asset, ~"/favicon.ico", {priv_file, arizona_website, ~"static/favicon.ico"}},
        {asset, ~"/robots.txt", {priv_file, arizona_website, ~"static/robots.txt"}},
        {asset, ~"/assets", {priv_dir, arizona_website, ~"static/assets"}},
        {asset, ~"/images", {priv_dir, arizona_website, ~"static/images"}},
        {view, ~"/", arizona_website_view, {arizona_website_home_page, #{}}},
        %% Arizona routes
        {asset, ~"/assets", {priv_dir, arizona, ~"static/assets"}},
        {websocket, ~"/live"}
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

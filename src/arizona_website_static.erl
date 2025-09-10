-module(arizona_website_static).
-export([generate/0]).

generate() ->
    ok = application:set_env(arizona_website, env, prod),
    {ok, Cwd} = file:get_cwd(),
    OutputDir = filename:join([Cwd, "dist"]),
    ok =
        case file:del_dir_r(OutputDir) of
            ok -> ok;
            {error, enoent} -> ok
        end,

    % Configuration for static site generation
    Config = #{
        base_url => ~"https://arizonaframework.org",
        route_paths => #{
            ~"/" => #{parallel => true},
            ~"/favicon.ico" => #{parallel => true},
            ~"/favicon.svg" => #{parallel => true},
            ~"/favicon-96x96.png" => #{parallel => true},
            ~"/apple-touch-icon.png" => #{parallel => true},
            ~"/web-app-manifest-192x192.png" => #{parallel => true},
            ~"/web-app-manifest-512x512.png" => #{parallel => true},
            ~"/site.webmanifest" => #{parallel => true},
            ~"/robots.txt" => #{parallel => true},
            ~"/assets/main.js" => #{parallel => true},
            ~"/assets/prism.js" => #{parallel => true},
            ~"/assets/app.css" => #{parallel => true},
            ~"/images/arizona_256x256.jpeg" => #{parallel => true},
            ~"/images/arizona_512x512.jpeg" => #{parallel => true}
        },
        output_dir => OutputDir
    },

    % Generate the static site
    Result =
        case arizona_static:generate(Config) of
            ok ->
                io:format("Static site generated successfully in '~s' directory!~n", [OutputDir]),
                ok;
            {error, Reason} ->
                io:format("Failed to generate static site: ~p~n", [Reason]),
                {error, Reason}
        end,
    ok = application:set_env(arizona_website, env, dev),
    Result.

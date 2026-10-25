-module(arizona_website).
-moduledoc """
Builds the site into `dist/`: the home page through `arizona_static`, the files
under `priv/static` copied beside it, and Arizona's static client from the
`arizona` dependency. Run with `make dist`.
""".

-export([build/0]).

-define(OUT, "dist").

-spec build() -> ok.
build() ->
    ok = clean(?OUT),
    {[_Index], []} = arizona_static:generate(
        ?OUT,
        [{arizona_website_home_page, ~"index.html"}],
        #{layouts => [{arizona_website_layout, render}]}
    ),
    Static = filename:join(code:priv_dir(arizona_website), "static"),
    ok = copy_tree(Static, ?OUT),
    %% Arizona's static client, which the page script starts.
    Client = filename:join(code:priv_dir(arizona), "static/assets/js/arizona-static.min.js"),
    {ok, _Bytes} = file:copy(Client, filename:join(?OUT, "assets/arizona-static.min.js")),
    ok.

clean(Dir) ->
    case file:del_dir_r(Dir) of
        ok -> ok;
        {error, enoent} -> ok
    end.

copy_tree(From, To) ->
    lists:foreach(
        fun(File) ->
            Target = filename:join(To, File),
            ok = filelib:ensure_dir(Target),
            {ok, _Bytes} = file:copy(filename:join(From, File), Target)
        end,
        [F || F <- filelib:wildcard("**", From), not filelib:is_dir(filename:join(From, F))]
    ).

-module(counter).
-include_lib("arizona/include/arizona_stateful.hrl").
-export([mount/1, render/1, handle_event/3]).

mount(_Bindings) ->
    {#{id => ~"counter", count => 0}, #{}}.

render(Bindings) ->
    ?html(
        {'div', [{id, ?get(id)}], [
            {span, [], [?get(count)]},
            {button, [{az_click, arizona_js:push_event(~"inc")}], [~"+"]}
        ]}
    ).

handle_event(~"inc", _Payload, #{count := Count} = Bindings) ->
    {Bindings#{count => Count + 1}, #{}, []}.

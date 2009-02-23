-module(lrb).

-export([fetch/0, fetch/1]).

fetch() -> log_roller_browser:fetch().
fetch(Args) -> log_roller_browser:fetch(Args).
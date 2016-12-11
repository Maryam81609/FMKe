-module(fmk_SUITE).

%% common_test callbacks
-export([%% suite/0,
  init_per_suite/1,
  end_per_suite/1,
  init_per_testcase/2,
  end_per_testcase/2,
  all/0]).

%% tests
-export([create_prescription/1]).


init_per_suite(Config) ->
  Pid = spawn_link(fmk_app, start, [ignore, ignore]),
  [{fmk_pid, Pid} | Config].

end_per_suite(Config) ->
  fmk_app:stop(ignore),
  Pid = proplists:get_value(fmk_pid, Config),
  exit(Pid, kill),
  Config.

init_per_testcase(_Case, Config) ->
  Config.

end_per_testcase(_, _) ->
  ok.

all() -> [create_prescription].

create_prescription(_Conf) ->
  ok.
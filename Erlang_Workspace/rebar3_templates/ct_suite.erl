-module({{name}}_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() -> [i_always_fail].

% groups() -> [{GroupName, GroupProperties, GroupMembers}].

% ----- Init -----
init_per_suite(Config) -> Config.
end_per_suite(_Config) -> ok.

init_per_group(_, Config) -> Config.
end_per_group(_, _Config) -> ok.

init_per_testcase(_, Config) -> Config.
end_per_testcase(_, _Config) -> ok.

% ----- Tests -----
i_always_fail(_Config) ->
  true = false.

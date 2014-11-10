PROJECT = mrecho

DEPS = cowboy jiffy jsx
#dep_cowboy = pkg://cowboy 1.0.0
#dep_jiffy = https://github.com/davisp/jiffy
#dep_jsx = pkg://jsx master

SHELL_DEPS = tddreloader
SHELL_OPTS = -s tddreloader

include erlang.mk

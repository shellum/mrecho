PROJECT = mrecho

DEPS = cowboy jiffy jsx
#dep_cowboy = pkg://cowboy 1.0.0
#dep_jiffy = https://github.com/davisp/jiffy
#dep_jsx = pkg://jsx master

SHELL_DEPS = tddreloader
SHELL_OPTS = -s tddreloader

build: all bin/start

include erlang.mk

.env: .env.example
	@heroku config | sed -e 's/:/=/' | sed -e 's/ //g' > $@

start: all bin/start .env
	@$(ENV) ./bin/start $(PROJECT)

bin/start:
	@mkdir -p bin
	@curl https://gist.githubusercontent.com/camshaft/372cc332241ac95ae335/raw/start -o $@
	@chmod a+x $@

.PHONY: start

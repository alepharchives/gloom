
SRC = src
EBIN = ebin

ERLC = $(shell which erlc)

SOURCES = \
	src/gloom.erl \
	src/gloom_app.erl \
	src/gloom_master.erl \
	src/gloom_server.erl \
	src/gloom_slave.erl \
	src/gloom_sup.erl \
	src/gloom_util.erl \
	src/mochijson2.erl \
	src/mochinum.erl

OBJECTS = \
	ebin/gloom.app \
	ebin/gloom.beam \
	ebin/gloom_app.beam \
	ebin/gloom_master.beam \
	ebin/gloom_server.beam \
	ebin/gloom_slave.beam \
	ebin/gloom_sup.beam \
	ebin/gloom_util.beam \
	ebin/mochijson2.beam \
	ebin/mochinum.beam

all: build

prepare:
	@if [ ! -d $(EBIN) ] ; then mkdir -p $(EBIN) ; fi

build: prepare $(OBJECTS)

${EBIN}/gloom.app: ${SRC}/gloom.app
	cp $(SRC)/gloom.app $(EBIN)/gloom.app

${EBIN}/%.beam: ${SRC}/%.erl
	$(ERLC) -W +debug -o ebin $<

test: build
	prove -v t/*.t

clean:
	rm $(EBIN)/*.beam $(EBIN)/*.app

run: build
	./bin/gloom-server

.PHONY: test deploy deps

.EXPORT_ALL_VARIABLES:

init:
	git submodule init && git submodule update

repl: init
	mkdir -p target/stylo/dev/ target/shadow/dev/
	clj -M:build:ui:test:nrepl -m nrepl.cmdline --middleware [cider.nrepl/cider-middleware]

test: parsers-java
	clj -M:ui:test:kaocha --skip-meta :todo  --skip-meta :slow  --skip-meta :external ${KAOCHA_OPTS}

up:
	docker-compose up -d

restart:
	docker-compose restart

stop:
	docker-compose stop

down:
	docker-compose down

npm:
	npm install

deps:
	clj -A:ui:test:build -X hello/hello

jar: npm parsers-java
	clj -M:ui:build  -m build

lint:
	clj -Sdeps '{:deps {clj-kondo/clj-kondo {:mvn/version "RELEASE"}}}' -m clj-kondo.main --lint src

outdated:
	clj -Sdeps '{:deps {olical/depot {:mvn/version "RELEASE"}}}' -m depot.outdated.main

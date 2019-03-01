.PHONY: clean setup build run-watch test test-watch install

clean:
	stack clean

setup:
	stack setup
	stack build ghcid

build:
	stack build

ghcid: setup
	stack exec ghcid

run-watch:
	env PORT=8080 SLEEPING_LIMIT=120 CAMPING_LIMIT=50 ADMIN_PASSWORD=admin DATABASE_URL='postgres://localhost/freiburg2019' stack exec ghcid -- -c "stack ghci --main-is freiburg2019:exe:freiburg2019-exe" -T="main"

test:
	env DATABASE_URL='postgres://localhost/freiburg2019_test' stack test --fast

test-watch:
	env DATABASE_URL='postgres://localhost/freiburg2019_test' stack exec ghcid -- -c "stack ghci test/Spec.hs" -T="main"

install:
	stack build --copy-bins

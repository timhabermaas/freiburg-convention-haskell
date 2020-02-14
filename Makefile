.PHONY: clean setup build run-watch run test test-watch install

clean:
	stack clean

setup:
	stack setup
	stack build ghcid

build:
	stack build

ghcid: setup
	stack exec ghcid

ghci:
	env PORT=8080 SLEEPING_LIMIT=120 CAMPING_LIMIT=50 ADMIN_PASSWORD=admin DATABASE_URL='postgres://postgres@localhost/freiburg_convention' stack ghci

run-watch:
	env PORT=8080 SLEEPING_LIMIT=120 CAMPING_LIMIT=50 ADMIN_PASSWORD=admin DATABASE_URL='postgres://postgres@localhost/freiburg_convention' stack exec ghcid -- -c "stack ghci --main-is freiburg-convention:exe:freiburg-convention-exe" -T="main"

run: build
	env PORT=8080 SLEEPING_LIMIT=120 CAMPING_LIMIT=50 ADMIN_PASSWORD=admin DATABASE_URL='postgres://postgres@localhost/freiburg_convention' stack exec freiburg-convention-exe

test:
	env DATABASE_URL='postgres://postgres@localhost/freiburg_convention_test' stack test --fast

test-watch:
	env DATABASE_URL='postgres://postgres@localhost/freiburg_convention_test' stack exec ghcid -- -c "stack ghci test/Spec.hs" -T="main"

install:
	stack build --copy-bins

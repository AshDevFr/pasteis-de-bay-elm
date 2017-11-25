all: test
	elm-make app/Main.elm --output build/app.js --warn

get-deps:
	elm package install

test:
	elm-test

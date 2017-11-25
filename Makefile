all: test
	./node_modules/.bin/elm-make app/Main.elm --output build/app.js --warn

get-deps:
	./node_modules/.bin/elm package install

test:
	./node_modules/.bin/elm-test

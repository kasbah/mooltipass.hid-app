all:
	mkdir -p build
	elm-make src/main.elm --output build/elm.js
	install src/index.html build/

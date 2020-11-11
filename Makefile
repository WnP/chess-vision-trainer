build:
	elm make src/Main.elm --output=app.js --optimize
	mkdir build
	cp app.js build
	cp index.html build
	cp style.css build
	cp favicon.ico build

clean:
	rm -rf build

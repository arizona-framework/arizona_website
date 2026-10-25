.PHONY: all css dist preview clean

all: dist

# Tailwind scans src/ for the classes the pages use.
css:
	npm ci --silent
	npm run build-css

dist: css
	rebar3 compile
	erl -noshell -pa _build/default/lib/*/ebin -eval 'ok = arizona_website:build(), halt().'

preview:
	python3 -m http.server --directory dist 8000

clean:
	rm -rf _build dist

# will compile src/HomePage.elm to index.html
elm make src/Main.elm --output elm.js
mkdir dist
cp public/index.html dist/index.html

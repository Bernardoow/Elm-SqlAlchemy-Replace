#!/bin/bash

elm-make src/Main.elm --output js/main.js
minify js/main.js
rm js/main.js



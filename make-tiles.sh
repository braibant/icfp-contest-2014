#!/bin/sh

sed -n -e '1,5d' \
       -e 's/^"\(..\) c #\(..\)..\(..\)..\(..\).*/ \1="0x\2\3\4;"/p' \
       -e '/^.. pixels ../d' \
       -e 's/^"\(.*\)".*$/\1/' \
       -e '/^[^ ]/s/../$&/g' \
       -e '/^\$/s/.*/echo "[|&|];"/p' \
    < tiles.xpm > make-tiles.temp

(echo "let raw = [|"; sh make-tiles.temp; echo "|]") >tiles.ml

rm -f make-tiles.temp

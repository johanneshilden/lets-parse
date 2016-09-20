#!/bin/bash

function conv {
    pdflatex $1.tex && convert -resize 50% -density 300 $1.pdf -quality 90 $1.png
}

for i in `seq 1 6`;
do
    if [ "$i" -lt "10" ]
      then
        conv 0$i
      else
        conv $i
    fi
done    


#!/bin/bash

function conv {
    pdflatex $1.tex && convert -density 300 $1.pdf -quality 90 $1.png
}

conv 01
conv 02
conv 03
conv 04

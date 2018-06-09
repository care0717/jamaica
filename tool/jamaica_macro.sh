#!/bin/bash
for file in `\ls ./text`; do
  echo $file | ./jamaicaRepl >> $file &
done

#!/bin/sh

for i in *.ck; do
  ../luck < $i > $i.out.tmp
  diff $i.out $i.out.tmp
done

rm *.tmp

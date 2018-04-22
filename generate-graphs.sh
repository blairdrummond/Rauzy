#!/bin/sh

for i in `seq 1 10`
do
    ./grapher -o images/thue-morse-$i.svg -w 2000 -h 2000 thueMorse $i
    # inkscape -z -b white -e jpgs/thue-morse-$i.jpg images/thue-morse-$i.svg
    inkscape -z -e jpgs/thue-morse-$i.jpg images/thue-morse-$i.svg
    
    ./grapher -o images/fibonacci-$i.svg -w 2000 -h 2000 fibonacci $i
    # inkscape -z -b white -e jpgs/fibonacci-$i.jpg images/fibonacci-$i.svg
    inkscape -z -e jpgs/fibonacci-$i.jpg images/fibonacci-$i.svg

    ./grapher -o images/npChacon-$i.svg -w 2000 -h 2000 npChacon $i
    # inkscape -z -b white -e jpgs/npChacon-$i.jpg images/npChacon-$i.svg
    inkscape -z -e jpgs/npChacon-$i.jpg images/npChacon-$i.svg

    ./grapher -o images/tribonacci-$i.svg -w 2000 -h 2000 tribonacci $i
    # inkscape -z -b white -e jpgs/tribonacci-$i.jpg images/tribonacci-$i.svg
    inkscape -z -e jpgs/tribonacci-$i.jpg images/tribonacci-$i.svg
done

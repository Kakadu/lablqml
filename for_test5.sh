#!/bin/sh

lst="QWidget Qt QKeyEvent QSpinBox"
#lst="QObject"
./xml/xmltool.native -in aaa.xml -out for_test5.xml $lst 

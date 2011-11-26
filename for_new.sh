#!/bin/sh

lst="QObject QPushButton Qt QKeyEvent"

./xml/xmltool.native -in aaa.xml -out for_new.xml $lst 

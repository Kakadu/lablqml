#!/bin/sh

lst="Qt QWidget QKeyEvent"
input="qt5-gui.xml"

./xml/xmltool.native -in $input -out qt5-minimal.xml $lst 

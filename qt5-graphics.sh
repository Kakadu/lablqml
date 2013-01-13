#!/bin/sh




lst="Qt QGraphicsView QGraphicsPixmapItem QGraphicsScene QPixmap"
input="qt5-gui.xml"

./xml/xmltool.native -in $input -out qt5-graphics.xml $lst 

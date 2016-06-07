#!/bin/sh

lst="Qt QWidget QKeyEvent QAbstractListModel QListView"
#lst="QVariant Qt QWidget QKeyEvent"
#lst="Qt QObject QWidget QKeyEvent"
#lst="QWidget QAbstractListModel Qt"
#lst="Qt QObject QWidget"

input="qt5-gui.xml"
tool=./xml/xmltool.native
make -C xml xmltool.native --no-print-directory && $tool -in $input -out qt5-minimal.xml $lst


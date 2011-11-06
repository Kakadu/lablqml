#!/bin/sh

lst="QDialog QMenuBar QMenu QAction QGroupBox QHBoxLayout QGridLayout QLabel QLineEdit QTextEdit QFormLayout QComboBox QLabel QSpinBox QHBoxLayout QVBoxLayout QPushButton QDialogButtonBox Qt QAbstractSlider QScrollBar QTabWidget QCheckBox QListWidget QTreeWidget"

./xml/xmltool.native -in aaa.xml -out for_test5.xml $lst 

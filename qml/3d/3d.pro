QML_IMPORT_PATH =

# DEFINES += QMLJSDEBUGGER


# The .cpp file which was generated for your project. Feel free to hack it.
SOURCES += main.cpp \
    gamemap.cpp


QT += Qt3DQuick declarative
LIBS+= -lQt3DQuick

HEADERS += \
    gamemap.h

OTHER_FILES += \
    qml/3d/main.qml

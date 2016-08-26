import QtQuick 2.0
import QtQuick.Controls 1.0
import QtQuick.Layouts 1.0

ApplicationWindow {
    width: 1366
    height: 768
    menuBar: MenuBar {
        Menu {
            title: "Edit"

            MenuItem {
                text: "Cut"
                shortcut: "Ctrl+X"
                onTriggered: console.log("")
            }

            MenuItem {
                text: "Copy"
                shortcut: "Ctrl+C"
                onTriggered: console.log("")
            }

            MenuItem {
                text: "Paste"
                shortcut: "Ctrl+V"
                onTriggered: console.log("")
            }
        }
    }
    toolBar: ToolBar {
        RowLayout {
            anchors.margins: 8
            anchors.fill: parent
            ToolButton {
                text: "Press me"
                onClicked: console.log("Press me clicked")
            }
        }
    }
    TableView {
        model: mainModel
        anchors.fill: parent

        TableViewColumn { title: "Title";  role: "title" ; width: 400  }
        TableViewColumn { title: "Author"; role: "author"; width: 500 }
    }
}
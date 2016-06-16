import QtQuick 2.1
import QtQuick.Controls 1.0
import QtQuick.Layouts 1.0

ApplicationWindow {
    // next two properties regulate how big  text blocks and latters will be
    property int defaultFontSize: 19
    property int defaultTextFieldHeight: defaultFontSize + 4
    property string backgroundColor: "#FFFFDF"

    menuBar: MenuBar {
        Menu {
            title: "File"
            //MenuItem { text: "Open..." }
            MenuItem {
                text: "Close"
                shortcut: "Ctrl-Q"
                onTriggered: { Qt.quit() }
            }
        }

        Menu {
            title: "Edit"
            MenuItem { text: "Cut" }
            MenuItem { text: "Copy" }
            MenuItem { text: "Paste" }
        }
    }
    ExclusiveGroup {
        Action {
            id: api_browsing_action
            text: "Api Browsing"
            checkable: true
            Component.onCompleted: checked = true
            onTriggered: {
                root.applyPaths();
                root.state = "BROWSE_API";
            }
        }
        Action {
            id: path_editing_action
            text: "Path Editing"
            checkable: true
            Component.onCompleted: checked = false
            onTriggered: {
                root.setCurrentPaths();
                console.log(editPathsContainer.pathsModel)
                root.state = "EDIT_PATHS";
            }
        }
    }
    toolBar: ToolBar {
        RowLayout {
            ToolButton { text: "Path Editing"; action: path_editing_action }
            ToolButton { text: "API browsing"; action: api_browsing_action }
        }
    }

    Rectangle {
        id: root
        color: backgroundColor
        width: 800; height: 600;
        anchors.fill: parent

        states: [
            State {
                name: "BROWSE_API"
                PropertyChanges { target: browseAPIContainer; visible: true }
                PropertyChanges { target: editPathsContainer; visible: false }
            },
            State {
                name: "EDIT_PATHS"
                PropertyChanges { target: editPathsContainer; visible: true }
                PropertyChanges { target: browseAPIContainer; visible: false }
            }
        ]
        state: "BROWSE_API"

        ApiBrowser {
            id: browseAPIContainer
            anchors.fill: parent
        }

        PathEditor {
            id: editPathsContainer
            anchors.fill: parent
        }

        function setCurrentPaths() {
            // get OCaml paths and set them to temporary model
            var lst = controller.paths() // So hackful because we need to convert QList<String> to Array
            var ans = [];
            for (var x in lst ) ans.push(lst[x])
            editPathsContainer.pathModel = ans
        }
        function applyPaths() {
            // transfer selected paths to OCaml
            controller.setPaths(editPathsContainer.pathModel)
        }
    }
}


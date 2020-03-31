import QtQuick 2.14
import QtQuick.Window 2.14

import com.lablqml.demo 1.0

Window {
    visible: true
    width: 640
    height: 480
    title: qsTr("Hello World")

    MyInstantiable {
        author: "Amelie"
        creationDate: new Date()

        MessageBoard.expired: creationDate < new Date("January 01, 2015 10:45:00")
        MessageBoard.onPublished: console.log("Message by", author, "has been published!")

        Component.onCompleted: console.log("MyInstantiable onCompleted in QML")
    }

    property int someValue: MyApi.someProperty
    Component.onCompleted:  console.log(MyApi.doSomething())

    MouseArea {
        MessageBoard.expired: false
    }

    MessageBoard {
        Message { author: "Naomi" }
        Message { author: "Clancy" }
    }


    //MyQmlType { Component.onCompleted: console.log("111")   }
}

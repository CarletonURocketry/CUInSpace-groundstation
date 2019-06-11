import QtLocation 5.9
import QtPositioning 5.9
import QtQuick 2.7
import QtQuick.Controls 1.4
import QtQuick.Layouts 1.2
import QtQuick.Window 2.2

Window {
	title: "CU In Space Ground Station"
	width: 1100
	height: 700
	visible: true

	Map {
		id: mainMap
		anchors.fill: parent
		plugin: Plugin {
			name: "osm"
			PluginParameter { name: "osm.mapping.offline.directory"; value: "map-cache" }
		}
		center: QtPositioning.coordinate(32.9406, -106.91492)
		zoomLevel: 14

		MapQuickItem {
			id: rocketMark
			coordinate: gps == null ? QtPositioning.coordinate(0, 0) : QtPositioning.coordinate(gps.latitude, gps.longitude)
			visible: gps != null
			sourceItem: Text { text: "R"; color: "red"; font.weight: Font.Bold }
		}
	}

	Button {
		id: rocketButton
		anchors.left: parent.left
		anchors.bottom: parent.bottom
		anchors.margins: 20
		text: "Snap to Rocket"
		checkable: true
		visible: gps != null
		onClicked: {
			if (checked) {
				mainMap.center = Qt.binding(function () { return rocketMark.coordinate });
			} else {
				mainMap.center = mainMap.center;
			}
		}
	}

	Rectangle {
		antialiasing: true
		radius: 10
		color: "#CCCCCCCC"
		width: parent.width > 1250 ? 300 : (0.2 * parent.width)
		height: 0.85 * parent.height
		anchors.top: parent.top
		anchors.right: parent.right
		anchors.margins: 20

		ListView {
			anchors.fill: parent
			anchors.margins: 5
			clip: true
			model: panelModel
			delegate: RowLayout {
				width: parent.width
				Text { text: caption; font.pointSize: 11 }
				Text {
					font.pointSize: 11;
					horizontalAlignment: Text.AlignRight;
					Layout.fillWidth: true
					Component.onCompleted: text = Qt.binding(modelFunctions[group + ":" + caption])
				}
			}
			section.property: "group"
			section.delegate: Text {
				text: section
				font.pointSize: 9
				anchors.horizontalCenter: parent.horizontalCenter
			}
		}
	}

	ListModel {
		id: panelModel

		ListElement { group: "Rocket Status"; caption: "State" }
		ListElement { group: "Rocket Status"; caption: "Chute Deployed" }
		ListElement { group: "Rocket Status"; caption: "Mission Time" }
		ListElement { group: "Rocket Status"; caption: "Signal:Noise" }
		ListElement { group: "Rocket Location"; caption: "Latitude" }
		ListElement { group: "Rocket Location"; caption: "Longitude" }
		ListElement { group: "Rocket Location"; caption: "Altitude" }
		ListElement { group: "Rocket Location"; caption: "Speed" }
		ListElement { group: "Rocket Location"; caption: "Course" }
		ListElement { group: "Rocket Location"; caption: "Time" }
		ListElement { group: "Altimeter"; caption: "Altitude" }
		ListElement { group: "Altimeter"; caption: "Temperature" }
		ListElement { group: "Accelerometer"; caption: "X" }
		ListElement { group: "Accelerometer"; caption: "Y" }
		ListElement { group: "Accelerometer"; caption: "Z" }
		ListElement { group: "Accelerometer"; caption: "Temperature" }
	}

	property var modelFunctions: {
		"Rocket Status:State": function() { return rocket == null ? "ND" : rocket == null ? "ND" : rocket.state },
		"Rocket Status:Chute Deployed": function() { return rocket == null ? "ND" : (rocket.parachuteDeployed ? "Yes" : "No") },
		"Rocket Status:Mission Time": function() { return rocket == null ? "ND" : rocket.missionTime + " ms" },
		"Rocket Status:Signal:Noise": function() { return rocket == null ? "ND" : rocket.signalNoise },
		"Rocket Location:Latitude": function() { return gps == null ? "ND" : niceLatLong(gps.latitude, "N", "S") },
		"Rocket Location:Longitude": function() { return gps == null ? "ND" : niceLatLong(gps.longitude, "E", "W") },
		"Rocket Location:Altitude": function() { return gps == null ? "ND" : gps.altitude.toPrecision(5) + " m" },
		"Rocket Location:Speed": function() { return gps == null ? "ND" : gps.groundSpeed.toPrecision(5) + " m/s" },
		"Rocket Location:Course": function() { return gps == null ? "ND" : gps.course.toPrecision(5) + "°" },
		"Rocket Location:Time": function() { return gps == null ? "ND" : gps.time },
		"Altimeter:Altitude": function() { return rocket == null ? "ND" : rocket.altitude.toPrecision(5) + " m" },
		"Altimeter:Temperature": function() { return rocket == null ? "ND" : rocket.altimeterTemp.toPrecision(5) + "°" },
		"Accelerometer:X": function() { return rocket == null ? "ND" : rocket.accelX.toPrecision(5) + " <i>g</i>" },
		"Accelerometer:Y": function() { return rocket == null ? "ND" : rocket.accelY.toPrecision(5) + " <i>g</i>" },
		"Accelerometer:Z": function() { return rocket == null ? "ND" : rocket.accelZ.toPrecision(5) + " <i>g</i>" },
		"Accelerometer:Temperature": function() { return rocket == null ? "ND" : rocket.accelerometerTemp.toPrecision(5) + "°" },
	}

	function niceLatLong(val, pos, neg) {
		var degs = Math.floor(Math.abs(val));
		var fdegs = Math.abs(val) - degs;
		var mins = Math.floor(fdegs * 60);
		var secs = 60 * ((fdegs * 60) - mins);
		return degs + "°" + mins + "'" + secs.toPrecision(6) + "\"" + (val > 0 ? pos : neg);
	}
}

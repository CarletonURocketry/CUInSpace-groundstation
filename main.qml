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
			coordinate: rocketGps == null ? QtPositioning.coordinate(0, 0) : QtPositioning.coordinate(rocketGps.latitude, rocketGps.longitude)
			visible: rocketGps != null
			sourceItem: Text { text: "R"; color: "red"; font.weight: Font.Bold }
		}
		MapQuickItem {
			id: containerMark
			coordinate: container == null ? QtPositioning.coordinate(0, 0) : QtPositioning.coordinate(container.gps.latitude, container.gps.longitude)
			visible: container != null
			sourceItem: Text { text: "C"; color: "blue"; font.weight: Font.Bold }
		}
		MapQuickItem {
			id: uavMark
			coordinate: payload == null ? QtPositioning.coordinate(0, 0) : QtPositioning.coordinate(payload.gps.latitude, payload.gps.longitude)
			visible: payload != null
			sourceItem: Text { text: "P"; color: "green"; font.weight: Font.Bold }
		}
	}

	Row {
		anchors.left: parent.left
		anchors.bottom: parent.bottom
		anchors.margins: 20
		spacing: 20

		Button {
			id: rocketButton
			text: "Rocket"
			checkable: true
			visible: rocketGps != null
			onClicked: {
				if (checked) {
					containerButton.checked = uavButton.checked = false;
					mainMap.center = Qt.binding(function () { return rocketMark.coordinate });
				} else {
					mainMap.center = mainMap.center;
				}
			}
		}
		Button {
			id: containerButton
			text: "Payload Container"
			checkable: true
			visible: container != null
			onClicked: {
				if (checked) {
					rocketButton.checked = uavButton.checked = false;
					mainMap.center = Qt.binding(function () { return containerMark.coordinate });
				} else {
					mainMap.center = mainMap.center;
				}
			}
		}
		Button {
			id: uavButton
			text: "Payload UAV"
			checkable: true
			visible: payload != null
			onClicked: {
				if (checked) {
					rocketButton.checked = containerButton.checked = false;
					mainMap.center = Qt.binding(function () { return uavMark.coordinate });
				} else {
					mainMap.center = mainMap.center;
				}
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
		ListElement { group: "Rocket Status"; caption: "E-Match 1" }
		ListElement { group: "Rocket Status"; caption: "E-Match 2" }
		ListElement { group: "Rocket Status"; caption: "Chute Deployed" }
		ListElement { group: "Rocket Status"; caption: "Mission Time" }
		ListElement { group: "Rocket Status"; caption: "Battery Voltage" }
		ListElement { group: "Rocket Status"; caption: "Capacitor Voltage" }
		ListElement { group: "Rocket Location"; caption: "Latitude" }
		ListElement { group: "Rocket Location"; caption: "Longitude" }
		ListElement { group: "Rocket Location"; caption: "Altitude" }
		ListElement { group: "Rocket Location"; caption: "Speed" }
		ListElement { group: "Rocket Location"; caption: "Course" }
		ListElement { group: "Rocket Location"; caption: "Time" }
		ListElement { group: "Rocket Temperatures"; caption: "Ambient" }
		ListElement { group: "Rocket Temperatures"; caption: "Board" }
		ListElement { group: "Rocket Temperatures"; caption: "Battery" }
		ListElement { group: "Accelerometer"; caption: "X" }
		ListElement { group: "Accelerometer"; caption: "Y" }
		ListElement { group: "Accelerometer"; caption: "Z" }
		ListElement { group: "Gyroscope"; caption: "Pitch" }
		ListElement { group: "Gyroscope"; caption: "Yaw" }
		ListElement { group: "Gyroscope"; caption: "Roll" }
		ListElement { group: "Payload Container Status"; caption: "State" }
		ListElement { group: "Payload Container Status"; caption: "Deployed" }
		ListElement { group: "Payload Container Status"; caption: "Battery Voltage" }
		ListElement { group: "Payload Container Status"; caption: "Pressure" }
		ListElement { group: "Payload Container Status"; caption: "Temperature" }
		ListElement { group: "Payload Container Location"; caption: "Latitude" }
		ListElement { group: "Payload Container Location"; caption: "Longitude" }
		ListElement { group: "Payload Container Location"; caption: "Altitude" }
		ListElement { group: "Payload Container Location"; caption: "Speed" }
		ListElement { group: "Payload Container Location"; caption: "Course" }
		ListElement { group: "Payload Container Location"; caption: "Time" }
		ListElement { group: "Payload UAV Status"; caption: "State" }
		ListElement { group: "Payload UAV Status"; caption: "Pressure" }
		ListElement { group: "Payload UAV Status"; caption: "Temperature" }
		ListElement { group: "Payload UAV Status"; caption: "Battery Voltage" }
		ListElement { group: "Payload UAV Status"; caption: "Airspeed" }
		ListElement { group: "Payload UAV Status"; caption: "Attitude" }
		ListElement { group: "Payload UAV Location"; caption: "Latitude" }
		ListElement { group: "Payload UAV Location"; caption: "Longitude" }
		ListElement { group: "Payload UAV Location"; caption: "Altitude" }
		ListElement { group: "Payload UAV Location"; caption: "Speed" }
		ListElement { group: "Payload UAV Location"; caption: "Course" }
		ListElement { group: "Payload UAV Location"; caption: "Time" }
	}

	property var modelFunctions: {
		"Rocket Status:State": function() { return rocket == null ? "ND" : rocket == null ? "ND" : rocket.state },
		"Rocket Status:E-Match 1": function() { return rocket == null ? "ND" : (rocket.eMatch1Present ? "Present" : "Not Present") },
		"Rocket Status:E-Match 2": function() { return rocket == null ? "ND" : (rocket.eMatch2Present ? "Present" : "Not Present") },
		"Rocket Status:Chute Deployed": function() { return rocket == null ? "ND" : (rocket.parachuteDeployed ? "Yes" : "No") },
		"Rocket Status:Mission Time": function() { return rocket == null ? "ND" : rocket.missionTime + " ms" },
		"Rocket Status:Battery Voltage": function() { return rocket == null ? "ND" : rocket.batteryVoltage.toPrecision(5) + " V" },
		"Rocket Status:Capacitor Voltage": function() { return rocket == null ? "ND" : rocket.capacitorVoltage.toPrecision(5) + " V" },
		"Rocket Location:Latitude": function() { return rocketGps == null ? "ND" : niceLatLong(rocketGps.latitude, "N", "S") },
		"Rocket Location:Longitude": function() { return rocketGps == null ? "ND" : niceLatLong(rocketGps.longitude, "E", "W") },
		"Rocket Location:Altitude": function() { return rocket == null ? "ND" : rocket == null ? "NA" : rocket.altitude.toPrecision(5) + " m" },
		"Rocket Location:Speed": function() { return rocketGps == null ? "ND" : rocketGps.groundSpeed.toPrecision(5) + " m/s" },
		"Rocket Location:Course": function() { return rocketGps == null ? "ND" : rocketGps.course.toPrecision(5) + "°" },
		"Rocket Location:Time": function() { return rocketGps == null ? "ND" : rocketGps.time },
		"Rocket Temperatures:Ambient": function() { return rocket == null ? "ND" : rocket.ambientTemp.toPrecision(4) + " °C" },
		"Rocket Temperatures:Board": function() { return rocket == null ? "ND" : rocket.altimeterTemp.toPrecision(4) + " °C" },
		"Rocket Temperatures:Battery": function() { return rocket == null ? "ND" : rocket.batteryTemp.toPrecision(4) + " °C" },
		"Accelerometer:X": function() { return rocket == null ? "ND" : rocket.accelX.toPrecision(5) + " <i>g</i>" },
		"Accelerometer:Y": function() { return rocket == null ? "ND" : rocket.accelY.toPrecision(5) + " <i>g</i>" },
		"Accelerometer:Z": function() { return rocket == null ? "ND" : rocket.accelZ.toPrecision(5) + " <i>g</i>" },
		"Gyroscope:Pitch": function() { return rocket == null ? "ND" : rocket.pitch.toPrecision(5) + " °/s"},
		"Gyroscope:Yaw": function() { return rocket == null ? "ND" : rocket.yaw.toPrecision(5) + " °/s"},
		"Gyroscope:Roll": function() { return rocket == null ? "ND" : rocket.roll.toPrecision(5) + " °/s"},
		"Payload Container Status:State": function() { return container == null ? "ND" : container.state },
		"Payload Container Status:Deployed": function() { return container == null ? "ND" : container.deployed },
		"Payload Container Status:Battery Voltage": function() { return container == null ? "ND" : container.batteryVoltage.toPrecision(5) + " V" },
		"Payload Container Status:Pressure": function() { return container == null ? "ND" : container.pressure.toPrecision(4) + " Pa" },
		"Payload Container Status:Temperature": function() { return container == null ? "ND" : container.temperature.toPrecision(4) + " °C" },
		"Payload Container Location:Latitude": function() { return container == null ? "ND" : niceLatLong(container.gps.latitude, "N", "S") },
		"Payload Container Location:Longitude": function() { return container == null ? "ND" : niceLatLong(container.gps.longitude, "E", "W") },
		"Payload Container Location:Altitude": function() { return container == null ? "ND" : container.altitude.toPrecision(5) + " m" },
		"Payload Container Location:Speed": function() { return container == null ? "ND" : container.gps.groundSpeed.toPrecision(5) + " m/s" },
		"Payload Container Location:Course": function() { return container == null ? "ND" : container.gps.course.toPrecision(5) + "°" },
		"Payload Container Location:Time": function() { return container == null ? "ND" : container.gps.time },
		"Payload UAV Status:State": function() { return payload == null ? "ND" : payload.state },
		"Payload UAV Status:Pressure": function() { return payload == null ? "ND" : payload.pressure.toPrecision(4) + " Pa" },
		"Payload UAV Status:Temperature": function() { return payload == null ? "ND" : payload.temperature.toPrecision(4) + " °C" },
		"Payload UAV Status:Battery Voltage": function() { return payload == null ? "ND" : payload.batteryVoltage.toPrecision(5) + " V" },
		"Payload UAV Status:Airspeed": function() { return payload == null ? "ND" : payload.airspeed.toPrecision(5) + " m/s" },
		"Payload UAV Status:Attitude": function() {
			if (payload == null) return "ND";
			var h = payload.attitude;
			return h[0].toPrecision(4) + " + " + h[1].toPrecision(4) + "i + " + h[2].toPrecision(4) + "j + " + h[3].toPrecision(4) + "k"
		},
		"Payload UAV Location:Latitude": function() { return payload == null ? "ND" : niceLatLong(payload.gps.latitude, "N", "S") },
		"Payload UAV Location:Longitude": function() { return payload == null ? "ND" : niceLatLong(payload.gps.longitude, "E", "W") },
		"Payload UAV Location:Altitude": function() { return payload == null ? "ND" : payload.altitude.toPrecision(5) + " m" },
		"Payload UAV Location:Speed": function() { return payload == null ? "ND" : payload.gps.groundSpeed.toPrecision(5) + " m/s"},
		"Payload UAV Location:Course": function() { return payload == null ? "ND" : payload.gps.course.toPrecision(5) + "°" },
		"Payload UAV Location:Time": function() { return payload == null ? "ND" : payload.gps.time }
	}

	function niceLatLong(val, pos, neg) {
		var degs = Math.floor(Math.abs(val));
		var fdegs = Math.abs(val) - degs;
		var mins = Math.floor(fdegs * 60);
		var secs = 60 * ((fdegs * 60) - mins);
		return degs + "°" + mins + "'" + secs.toPrecision(6) + "\"" + (val > 0 ? pos : neg);
	}
}

CREATE TABLE GPS (
        FrameID INTEGER PRIMARY KEY,
        GPS_Time TEXT NOT NULL,
        Latitude REAL NOT NULL,
        Longitude REAL NOT NULL,
        Speed REAL NOT NULL,
        Course REAL NOT NULL,
        GPS_Altitude REAL NOT NULL
);
CREATE TABLE Rocket_Telemetry (
        Mission_Time INTEGER NOT NULL,
        State TEXT NOT NULL,
        Parachute_Deployed INTEGER NOT NULL,
        Acceleration_X REAL NOT NULL,
        Acceleration_Y REAL NOT NULL,
        Acceleration_Z REAL NOT NULL,
        Altitude REAL NOT NULL,
        Alt_Temp REAL NOT NULL,
	Accel_Temp REAL NOT NULL,
        GPS_Data INTEGER,
        FOREIGN KEY(GPS_Data) REFERENCES GPS(FrameID)
);

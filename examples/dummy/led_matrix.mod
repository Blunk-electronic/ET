-- SYSTEM ET module
-- date 2019-01-03T11:39:43
-- ======================================================================================================================================================

[NET_CLASSES BEGIN]
	[NET_CLASS BEGIN]
		name Default
		description "This is the default net class."
		clearance 0.200
		track_width_min 0.250
		via_drill_min 0.400
		via_restring_min 0.100
		micro_via_drill_min 0.100
		micro_via_restring_min 0.100
	[NET_CLASS END]
[NET_CLASSES END]
[NETS BEGIN]
	[NET BEGIN]
		name /IN
		class Default
		scope local
		[STRANDS BEGIN]
			[STRAND BEGIN]
				[SEGMENTS BEGIN]
					[SEGMENT BEGIN]
						start sheet 1 x 88.90 y 108.40
						end sheet 1 x 78.74 y 108.40
						[LABELS BEGIN]
							[LABEL BEGIN]
								position x 78.74 y 108.40
								rotation 180.0
								size 1.30
								style normal
								line_width  0.00
								appearance simple
							[LABEL END]
						[LABELS END]
						[JUNCTIONS BEGIN]
						[JUNCTIONS END]
						[PORTS BEGIN]
						[PORTS END]
						[SUBMODULE_PORTS BEGIN]
						[SUBMODULE_PORTS END]
					[SEGMENT END]
					[SEGMENT BEGIN]
						start sheet 1 x 88.90 y 108.40
						end sheet 1 x 88.90 y 105.86
						[LABELS BEGIN]
						[LABELS END]
						[JUNCTIONS BEGIN]
						[JUNCTIONS END]
						[PORTS BEGIN]
							device IC10 port I1B
						[PORTS END]
						[SUBMODULE_PORTS BEGIN]
						[SUBMODULE_PORTS END]
					[SEGMENT END]
					[SEGMENT BEGIN]
						start sheet 1 x 88.90 y 110.94
						end sheet 1 x 88.90 y 108.40
						[LABELS BEGIN]
						[LABELS END]
						[JUNCTIONS BEGIN]
							position x 88.90 y 108.40
						[JUNCTIONS END]
						[PORTS BEGIN]
							device IC10 port I1A
						[PORTS END]
						[SUBMODULE_PORTS BEGIN]
						[SUBMODULE_PORTS END]
					[SEGMENT END]
				[SEGMENTS END]
			[STRAND END]
			[STRAND BEGIN]
				[SEGMENTS BEGIN]
					[SEGMENT BEGIN]
						start sheet 1 x 50.80 y 146.50
						end sheet 1 x 60.96 y 146.50
						[LABELS BEGIN]
							[LABEL BEGIN]
								position x 55.88 y 146.50
								rotation 180.0
								size 1.30
								style normal
								line_width  0.00
								appearance simple
							[LABEL END]
						[LABELS END]
						[JUNCTIONS BEGIN]
						[JUNCTIONS END]
						[PORTS BEGIN]
							device X1 port 3
						[PORTS END]
						[SUBMODULE_PORTS BEGIN]
						[SUBMODULE_PORTS END]
					[SEGMENT END]
				[SEGMENTS END]
			[STRAND END]
		[STRANDS END]
		[ROUTE BEGIN]
		[ROUTE END]
	[NET END]
	[NET BEGIN]
		name /LED_ANODE
		class Default
		scope local
		[STRANDS BEGIN]
			[STRAND BEGIN]
				[SEGMENTS BEGIN]
					[SEGMENT BEGIN]
						start sheet 1 x 68.58 y 145.23
						end sheet 1 x 68.58 y 141.42
						[LABELS BEGIN]
							[LABEL BEGIN]
								position x 68.58 y 142.69
								rotation 180.0
								size 1.30
								style normal
								line_width  0.00
								appearance simple
							[LABEL END]
						[LABELS END]
						[JUNCTIONS BEGIN]
						[JUNCTIONS END]
						[PORTS BEGIN]
							device LED2 port A
							device R1 port 2
						[PORTS END]
						[SUBMODULE_PORTS BEGIN]
						[SUBMODULE_PORTS END]
					[SEGMENT END]
				[SEGMENTS END]
			[STRAND END]
		[STRANDS END]
		[ROUTE BEGIN]
		[ROUTE END]
	[NET END]
	[NET BEGIN]
		name /N$3
		class Default
		scope local
		[STRANDS BEGIN]
			[STRAND BEGIN]
				[SEGMENTS BEGIN]
					[SEGMENT BEGIN]
						start sheet 1 x 134.62 y 141.42
						end sheet 1 x 129.54 y 141.42
						[LABELS BEGIN]
						[LABELS END]
						[JUNCTIONS BEGIN]
						[JUNCTIONS END]
						[PORTS BEGIN]
							device IC2 port PL
						[PORTS END]
						[SUBMODULE_PORTS BEGIN]
						[SUBMODULE_PORTS END]
					[SEGMENT END]
					[SEGMENT BEGIN]
						start sheet 1 x 129.54 y 141.42
						end sheet 1 x 129.54 y 131.26
						[LABELS BEGIN]
						[LABELS END]
						[JUNCTIONS BEGIN]
						[JUNCTIONS END]
						[PORTS BEGIN]
						[PORTS END]
						[SUBMODULE_PORTS BEGIN]
						[SUBMODULE_PORTS END]
					[SEGMENT END]
					[SEGMENT BEGIN]
						start sheet 1 x 129.54 y 131.26
						end sheet 1 x 134.62 y 131.26
						[LABELS BEGIN]
						[LABELS END]
						[JUNCTIONS BEGIN]
						[JUNCTIONS END]
						[PORTS BEGIN]
							device IC2 port R
						[PORTS END]
						[SUBMODULE_PORTS BEGIN]
						[SUBMODULE_PORTS END]
					[SEGMENT END]
				[SEGMENTS END]
			[STRAND END]
		[STRANDS END]
		[ROUTE BEGIN]
		[ROUTE END]
	[NET END]
	[NET BEGIN]
		name /OUT
		class Default
		scope local
		[STRANDS BEGIN]
			[STRAND BEGIN]
				[SEGMENTS BEGIN]
					[SEGMENT BEGIN]
						start sheet 1 x 187.96 y 124.91
						end sheet 1 x 196.85 y 124.91
						[LABELS BEGIN]
						[LABELS END]
						[JUNCTIONS BEGIN]
						[JUNCTIONS END]
						[PORTS BEGIN]
							device N2 port 1
						[PORTS END]
						[SUBMODULE_PORTS BEGIN]
						[SUBMODULE_PORTS END]
					[SEGMENT END]
					[SEGMENT BEGIN]
						start sheet 1 x 187.96 y 132.53
						end sheet 1 x 187.96 y 124.91
						[LABELS BEGIN]
						[LABELS END]
						[JUNCTIONS BEGIN]
							position x 187.96 y 108.40
							position x 187.96 y 124.91
						[JUNCTIONS END]
						[PORTS BEGIN]
						[PORTS END]
						[SUBMODULE_PORTS BEGIN]
						[SUBMODULE_PORTS END]
					[SEGMENT END]
					[SEGMENT BEGIN]
						start sheet 1 x 187.96 y 132.53
						end sheet 1 x 196.85 y 132.53
						[LABELS BEGIN]
						[LABELS END]
						[JUNCTIONS BEGIN]
						[JUNCTIONS END]
						[PORTS BEGIN]
							device N1 port 1
						[PORTS END]
						[SUBMODULE_PORTS BEGIN]
						[SUBMODULE_PORTS END]
					[SEGMENT END]
					[SEGMENT BEGIN]
						start sheet 1 x 187.96 y 124.91
						end sheet 1 x 187.96 y 117.29
						[LABELS BEGIN]
						[LABELS END]
						[JUNCTIONS BEGIN]
							position x 187.96 y 108.40
							position x 187.96 y 117.29
						[JUNCTIONS END]
						[PORTS BEGIN]
						[PORTS END]
						[SUBMODULE_PORTS BEGIN]
						[SUBMODULE_PORTS END]
					[SEGMENT END]
					[SEGMENT BEGIN]
						start sheet 1 x 187.96 y 117.29
						end sheet 1 x 196.85 y 117.29
						[LABELS BEGIN]
						[LABELS END]
						[JUNCTIONS BEGIN]
						[JUNCTIONS END]
						[PORTS BEGIN]
							device N3 port 1
						[PORTS END]
						[SUBMODULE_PORTS BEGIN]
						[SUBMODULE_PORTS END]
					[SEGMENT END]
					[SEGMENT BEGIN]
						start sheet 1 x 187.96 y 117.29
						end sheet 1 x 187.96 y 108.40
						[LABELS BEGIN]
						[LABELS END]
						[JUNCTIONS BEGIN]
							position x 187.96 y 108.40
						[JUNCTIONS END]
						[PORTS BEGIN]
						[PORTS END]
						[SUBMODULE_PORTS BEGIN]
						[SUBMODULE_PORTS END]
					[SEGMENT END]
					[SEGMENT BEGIN]
						start sheet 1 x 187.96 y 108.40
						end sheet 1 x 187.96 y 105.86
						[LABELS BEGIN]
						[LABELS END]
						[JUNCTIONS BEGIN]
						[JUNCTIONS END]
						[PORTS BEGIN]
							device R002 port 1
						[PORTS END]
						[SUBMODULE_PORTS BEGIN]
						[SUBMODULE_PORTS END]
					[SEGMENT END]
					[SEGMENT BEGIN]
						start sheet 1 x 187.96 y 108.40
						end sheet 1 x 167.64 y 108.40
						[LABELS BEGIN]
							[LABEL BEGIN]
								position x 172.72 y 108.40
								rotation 180.0
								size 1.30
								style normal
								line_width  0.00
								appearance simple
							[LABEL END]
						[LABELS END]
						[JUNCTIONS BEGIN]
						[JUNCTIONS END]
						[PORTS BEGIN]
							device IC10 port O1
						[PORTS END]
						[SUBMODULE_PORTS BEGIN]
						[SUBMODULE_PORTS END]
					[SEGMENT END]
				[SEGMENTS END]
			[STRAND END]
			[STRAND BEGIN]
				[SEGMENTS BEGIN]
					[SEGMENT BEGIN]
						start sheet 1 x 50.80 y 143.96
						end sheet 1 x 60.96 y 143.96
						[LABELS BEGIN]
							[LABEL BEGIN]
								position x 55.88 y 143.96
								rotation 180.0
								size 1.30
								style normal
								line_width  0.00
								appearance simple
							[LABEL END]
						[LABELS END]
						[JUNCTIONS BEGIN]
						[JUNCTIONS END]
						[PORTS BEGIN]
							device X1 port 2
						[PORTS END]
						[SUBMODULE_PORTS BEGIN]
						[SUBMODULE_PORTS END]
					[SEGMENT END]
				[SEGMENTS END]
			[STRAND END]
		[STRANDS END]
		[ROUTE BEGIN]
		[ROUTE END]
	[NET END]
	[NET BEGIN]
		name /OUT#
		class Default
		scope local
		[STRANDS BEGIN]
			[STRAND BEGIN]
				[SEGMENTS BEGIN]
					[SEGMENT BEGIN]
						start sheet 1 x 137.16 y 108.40
						end sheet 1 x 137.16 y 105.86
						[LABELS BEGIN]
						[LABELS END]
						[JUNCTIONS BEGIN]
						[JUNCTIONS END]
						[PORTS BEGIN]
							device IC10 port I3A
						[PORTS END]
						[SUBMODULE_PORTS BEGIN]
						[SUBMODULE_PORTS END]
					[SEGMENT END]
					[SEGMENT BEGIN]
						start sheet 1 x 119.38 y 108.40
						end sheet 1 x 137.16 y 108.40
						[LABELS BEGIN]
							[LABEL BEGIN]
								position x 127.00 y 108.40
								rotation 180.0
								size 1.30
								style normal
								line_width  0.00
								appearance simple
							[LABEL END]
						[LABELS END]
						[JUNCTIONS BEGIN]
						[JUNCTIONS END]
						[PORTS BEGIN]
							device IC10 port O1
						[PORTS END]
						[SUBMODULE_PORTS BEGIN]
						[SUBMODULE_PORTS END]
					[SEGMENT END]
					[SEGMENT BEGIN]
						start sheet 1 x 137.16 y 110.94
						end sheet 1 x 137.16 y 108.40
						[LABELS BEGIN]
						[LABELS END]
						[JUNCTIONS BEGIN]
							position x 137.16 y 108.40
						[JUNCTIONS END]
						[PORTS BEGIN]
							device IC10 port I1A
						[PORTS END]
						[SUBMODULE_PORTS BEGIN]
						[SUBMODULE_PORTS END]
					[SEGMENT END]
				[SEGMENTS END]
			[STRAND END]
			[STRAND BEGIN]
				[SEGMENTS BEGIN]
					[SEGMENT BEGIN]
						start sheet 1 x 50.80 y 149.04
						end sheet 1 x 60.96 y 149.04
						[LABELS BEGIN]
							[LABEL BEGIN]
								position x 55.88 y 149.04
								rotation 180.0
								size 1.30
								style normal
								line_width  0.00
								appearance simple
							[LABEL END]
						[LABELS END]
						[JUNCTIONS BEGIN]
						[JUNCTIONS END]
						[PORTS BEGIN]
							device X1 port 4
						[PORTS END]
						[SUBMODULE_PORTS BEGIN]
						[SUBMODULE_PORTS END]
					[SEGMENT END]
				[SEGMENTS END]
			[STRAND END]
		[STRANDS END]
		[ROUTE BEGIN]
		[ROUTE END]
	[NET END]
	[NET BEGIN]
		name GND
		class Default
		scope global
		[STRANDS BEGIN]
			[STRAND BEGIN]
				[SEGMENTS BEGIN]
					[SEGMENT BEGIN]
						start sheet 1 x 147.32 y 99.51
						end sheet 1 x 147.32 y 103.32
						[LABELS BEGIN]
						[LABELS END]
						[JUNCTIONS BEGIN]
						[JUNCTIONS END]
						[PORTS BEGIN]
							device IC10 port GND
						[PORTS END]
						[SUBMODULE_PORTS BEGIN]
						[SUBMODULE_PORTS END]
					[SEGMENT END]
					[SEGMENT BEGIN]
						start sheet 1 x 147.32 y 99.51
						end sheet 1 x 157.48 y 99.51
						[LABELS BEGIN]
						[LABELS END]
						[JUNCTIONS BEGIN]
						[JUNCTIONS END]
						[PORTS BEGIN]
						[PORTS END]
						[SUBMODULE_PORTS BEGIN]
						[SUBMODULE_PORTS END]
					[SEGMENT END]
					[SEGMENT BEGIN]
						start sheet 1 x 157.48 y 95.70
						end sheet 1 x 157.48 y 99.51
						[LABELS BEGIN]
						[LABELS END]
						[JUNCTIONS BEGIN]
						[JUNCTIONS END]
						[PORTS BEGIN]
						[PORTS END]
						[SUBMODULE_PORTS BEGIN]
						[SUBMODULE_PORTS END]
					[SEGMENT END]
					[SEGMENT BEGIN]
						start sheet 1 x 187.96 y 95.70
						end sheet 1 x 157.48 y 95.70
						[LABELS BEGIN]
						[LABELS END]
						[JUNCTIONS BEGIN]
						[JUNCTIONS END]
						[PORTS BEGIN]
						[PORTS END]
						[SUBMODULE_PORTS BEGIN]
						[SUBMODULE_PORTS END]
					[SEGMENT END]
					[SEGMENT BEGIN]
						start sheet 1 x 187.96 y 98.24
						end sheet 1 x 187.96 y 95.70
						[LABELS BEGIN]
						[LABELS END]
						[JUNCTIONS BEGIN]
						[JUNCTIONS END]
						[PORTS BEGIN]
							device R002 port 2
						[PORTS END]
						[SUBMODULE_PORTS BEGIN]
						[SUBMODULE_PORTS END]
					[SEGMENT END]
					[SEGMENT BEGIN]
						start sheet 1 x 147.32 y 96.97
						end sheet 1 x 147.32 y 99.51
						[LABELS BEGIN]
						[LABELS END]
						[JUNCTIONS BEGIN]
							position x 147.32 y 99.51
						[JUNCTIONS END]
						[PORTS BEGIN]
							device PWR01 port GND
						[PORTS END]
						[SUBMODULE_PORTS BEGIN]
						[SUBMODULE_PORTS END]
					[SEGMENT END]
					[SEGMENT BEGIN]
						start sheet 1 x 59.69 y 99.51
						end sheet 1 x 147.32 y 99.51
						[LABELS BEGIN]
						[LABELS END]
						[JUNCTIONS BEGIN]
							position x 147.32 y 99.51
						[JUNCTIONS END]
						[PORTS BEGIN]
						[PORTS END]
						[SUBMODULE_PORTS BEGIN]
						[SUBMODULE_PORTS END]
					[SEGMENT END]
					[SEGMENT BEGIN]
						start sheet 1 x 59.69 y 99.51
						end sheet 1 x 59.69 y 129.99
						[LABELS BEGIN]
						[LABELS END]
						[JUNCTIONS BEGIN]
						[JUNCTIONS END]
						[PORTS BEGIN]
						[PORTS END]
						[SUBMODULE_PORTS BEGIN]
						[SUBMODULE_PORTS END]
					[SEGMENT END]
					[SEGMENT BEGIN]
						start sheet 1 x 59.69 y 129.99
						end sheet 1 x 68.58 y 129.99
						[LABELS BEGIN]
						[LABELS END]
						[JUNCTIONS BEGIN]
						[JUNCTIONS END]
						[PORTS BEGIN]
						[PORTS END]
						[SUBMODULE_PORTS BEGIN]
						[SUBMODULE_PORTS END]
					[SEGMENT END]
					[SEGMENT BEGIN]
						start sheet 1 x 68.58 y 129.99
						end sheet 1 x 68.58 y 126.18
						[LABELS BEGIN]
						[LABELS END]
						[JUNCTIONS BEGIN]
						[JUNCTIONS END]
						[PORTS BEGIN]
						[PORTS END]
						[SUBMODULE_PORTS BEGIN]
						[SUBMODULE_PORTS END]
					[SEGMENT END]
					[SEGMENT BEGIN]
						start sheet 1 x 68.58 y 133.80
						end sheet 1 x 68.58 y 129.99
						[LABELS BEGIN]
						[LABELS END]
						[JUNCTIONS BEGIN]
							position x 68.58 y 129.99
						[JUNCTIONS END]
						[PORTS BEGIN]
							device LED2 port C
						[PORTS END]
						[SUBMODULE_PORTS BEGIN]
						[SUBMODULE_PORTS END]
					[SEGMENT END]
					[SEGMENT BEGIN]
						start sheet 1 x 50.80 y 129.99
						end sheet 1 x 59.69 y 129.99
						[LABELS BEGIN]
						[LABELS END]
						[JUNCTIONS BEGIN]
							position x 59.69 y 129.99
						[JUNCTIONS END]
						[PORTS BEGIN]
						[PORTS END]
						[SUBMODULE_PORTS BEGIN]
						[SUBMODULE_PORTS END]
					[SEGMENT END]
					[SEGMENT BEGIN]
						start sheet 1 x 50.80 y 129.99
						end sheet 1 x 50.80 y 126.18
						[LABELS BEGIN]
						[LABELS END]
						[JUNCTIONS BEGIN]
						[JUNCTIONS END]
						[PORTS BEGIN]
						[PORTS END]
						[SUBMODULE_PORTS BEGIN]
						[SUBMODULE_PORTS END]
					[SEGMENT END]
					[SEGMENT BEGIN]
						start sheet 1 x 50.80 y 141.42
						end sheet 1 x 50.80 y 129.99
						[LABELS BEGIN]
						[LABELS END]
						[JUNCTIONS BEGIN]
							position x 50.80 y 129.99
						[JUNCTIONS END]
						[PORTS BEGIN]
							device X1 port 1
						[PORTS END]
						[SUBMODULE_PORTS BEGIN]
						[SUBMODULE_PORTS END]
					[SEGMENT END]
				[SEGMENTS END]
			[STRAND END]
		[STRANDS END]
		[ROUTE BEGIN]
			[POLYGON BEGIN]
				priority 0
				layer 1
				min_width 0.150
				isolation 0.800
				fill_style solid
				hatching_line_width 0.508
				hatching_line_spacing 2.000
				corner_easing fillet
				easing_radius 1.000
				pad_connection thermal
				pad_technology smt_and_tht
				thermal_width 0.500
				thermal_gap 0.600
				[CORNERS BEGIN]
					position x 145.000 y 110.000
					position x 145.000 y 120.000
					position x 170.000 y 120.000
					position x 175.000 y 110.000
				[CORNERS END]
			[POLYGON END]
		[ROUTE END]
	[NET END]
	[NET BEGIN]
		name MCU_CLK
		class default
		scope global
		[STRANDS BEGIN]
			[STRAND BEGIN]
				[SEGMENTS BEGIN]
					[SEGMENT BEGIN]
						start sheet 1 x 207.01 y 132.53
						end sheet 1 x 243.84 y 132.53
						[LABELS BEGIN]
							[LABEL BEGIN]
								position x 243.84 y 132.53
								rotation 0.0
								size 1.30
								style normal
								line_width  0.00
								appearance tag
								direction input
							[LABEL END]
						[LABELS END]
						[JUNCTIONS BEGIN]
						[JUNCTIONS END]
						[PORTS BEGIN]
							device N1 port 2
						[PORTS END]
						[SUBMODULE_PORTS BEGIN]
						[SUBMODULE_PORTS END]
					[SEGMENT END]
				[SEGMENTS END]
			[STRAND END]
		[STRANDS END]
		[ROUTE BEGIN]
		[ROUTE END]
	[NET END]
	[NET BEGIN]
		name MCU_OUT
		class default
		scope global
		[STRANDS BEGIN]
			[STRAND BEGIN]
				[SEGMENTS BEGIN]
					[SEGMENT BEGIN]
						start sheet 1 x 207.01 y 117.29
						end sheet 1 x 243.84 y 117.29
						[LABELS BEGIN]
							[LABEL BEGIN]
								position x 243.84 y 117.29
								rotation 0.0
								size 1.30
								style normal
								line_width  0.00
								appearance tag
								direction output
							[LABEL END]
						[LABELS END]
						[JUNCTIONS BEGIN]
						[JUNCTIONS END]
						[PORTS BEGIN]
							device N3 port 2
						[PORTS END]
						[SUBMODULE_PORTS BEGIN]
						[SUBMODULE_PORTS END]
					[SEGMENT END]
				[SEGMENTS END]
			[STRAND END]
		[STRANDS END]
		[ROUTE BEGIN]
		[ROUTE END]
	[NET END]
	[NET BEGIN]
		name P3V3
		class default
		scope global
		[STRANDS BEGIN]
			[STRAND BEGIN]
				[SEGMENTS BEGIN]
					[SEGMENT BEGIN]
						start sheet 1 x 207.01 y 124.91
						end sheet 1 x 243.84 y 124.91
						[LABELS BEGIN]
							[LABEL BEGIN]
								position x 243.84 y 124.91
								rotation 0.0
								size 1.30
								style normal
								line_width  0.00
								appearance tag
								direction input
							[LABEL END]
						[LABELS END]
						[JUNCTIONS BEGIN]
						[JUNCTIONS END]
						[PORTS BEGIN]
							device N2 port 2
						[PORTS END]
						[SUBMODULE_PORTS BEGIN]
						[SUBMODULE_PORTS END]
					[SEGMENT END]
				[SEGMENTS END]
			[STRAND END]
		[STRANDS END]
		[ROUTE BEGIN]
		[ROUTE END]
	[NET END]
	[NET BEGIN]
		name VCC
		class Default
		scope global
		[STRANDS BEGIN]
			[STRAND BEGIN]
				[SEGMENTS BEGIN]
					[SEGMENT BEGIN]
						start sheet 1 x 87.63 y 154.12
						end sheet 1 x 96.52 y 154.12
						[LABELS BEGIN]
						[LABELS END]
						[JUNCTIONS BEGIN]
						[JUNCTIONS END]
						[PORTS BEGIN]
							device PWR02 port P3V3
						[PORTS END]
						[SUBMODULE_PORTS BEGIN]
						[SUBMODULE_PORTS END]
					[SEGMENT END]
					[SEGMENT BEGIN]
						start sheet 1 x 96.52 y 165.55
						end sheet 1 x 96.52 y 154.12
						[LABELS BEGIN]
						[LABELS END]
						[JUNCTIONS BEGIN]
						[JUNCTIONS END]
						[PORTS BEGIN]
						[PORTS END]
						[SUBMODULE_PORTS BEGIN]
						[SUBMODULE_PORTS END]
					[SEGMENT END]
					[SEGMENT BEGIN]
						start sheet 1 x 147.32 y 165.55
						end sheet 1 x 96.52 y 165.55
						[LABELS BEGIN]
						[LABELS END]
						[JUNCTIONS BEGIN]
						[JUNCTIONS END]
						[PORTS BEGIN]
						[PORTS END]
						[SUBMODULE_PORTS BEGIN]
						[SUBMODULE_PORTS END]
					[SEGMENT END]
					[SEGMENT BEGIN]
						start sheet 1 x 147.32 y 161.74
						end sheet 1 x 147.32 y 165.55
						[LABELS BEGIN]
						[LABELS END]
						[JUNCTIONS BEGIN]
						[JUNCTIONS END]
						[PORTS BEGIN]
							device IC2 port VCC
						[PORTS END]
						[SUBMODULE_PORTS BEGIN]
						[SUBMODULE_PORTS END]
					[SEGMENT END]
					[SEGMENT BEGIN]
						start sheet 1 x 87.63 y 154.12
						end sheet 1 x 87.63 y 118.56
						[LABELS BEGIN]
						[LABELS END]
						[JUNCTIONS BEGIN]
						[JUNCTIONS END]
						[PORTS BEGIN]
							device PWR02 port P3V3
						[PORTS END]
						[SUBMODULE_PORTS BEGIN]
						[SUBMODULE_PORTS END]
					[SEGMENT END]
					[SEGMENT BEGIN]
						start sheet 1 x 87.63 y 118.56
						end sheet 1 x 147.32 y 118.56
						[LABELS BEGIN]
						[LABELS END]
						[JUNCTIONS BEGIN]
						[JUNCTIONS END]
						[PORTS BEGIN]
						[PORTS END]
						[SUBMODULE_PORTS BEGIN]
						[SUBMODULE_PORTS END]
					[SEGMENT END]
					[SEGMENT BEGIN]
						start sheet 1 x 147.32 y 118.56
						end sheet 1 x 147.32 y 113.48
						[LABELS BEGIN]
						[LABELS END]
						[JUNCTIONS BEGIN]
						[JUNCTIONS END]
						[PORTS BEGIN]
							device IC10 port VCC
						[PORTS END]
						[SUBMODULE_PORTS BEGIN]
						[SUBMODULE_PORTS END]
					[SEGMENT END]
					[SEGMENT BEGIN]
						start sheet 1 x 147.32 y 123.64
						end sheet 1 x 147.32 y 118.56
						[LABELS BEGIN]
						[LABELS END]
						[JUNCTIONS BEGIN]
							position x 147.32 y 118.56
						[JUNCTIONS END]
						[PORTS BEGIN]
							device IC2 port GND
						[PORTS END]
						[SUBMODULE_PORTS BEGIN]
						[SUBMODULE_PORTS END]
					[SEGMENT END]
					[SEGMENT BEGIN]
						start sheet 1 x 76.20 y 154.12
						end sheet 1 x 87.63 y 154.12
						[LABELS BEGIN]
						[LABELS END]
						[JUNCTIONS BEGIN]
							position x 87.63 y 154.12
						[JUNCTIONS END]
						[PORTS BEGIN]
							device FLG03 port pwr
							device PWR02 port P3V3
						[PORTS END]
						[SUBMODULE_PORTS BEGIN]
						[SUBMODULE_PORTS END]
					[SEGMENT END]
					[SEGMENT BEGIN]
						start sheet 1 x 68.58 y 154.12
						end sheet 1 x 76.20 y 154.12
						[LABELS BEGIN]
						[LABELS END]
						[JUNCTIONS BEGIN]
							position x 87.63 y 154.12
							position x 76.20 y 154.12
						[JUNCTIONS END]
						[PORTS BEGIN]
							device FLG03 port pwr
						[PORTS END]
						[SUBMODULE_PORTS BEGIN]
						[SUBMODULE_PORTS END]
					[SEGMENT END]
					[SEGMENT BEGIN]
						start sheet 1 x 68.58 y 152.85
						end sheet 1 x 68.58 y 154.12
						[LABELS BEGIN]
						[LABELS END]
						[JUNCTIONS BEGIN]
						[JUNCTIONS END]
						[PORTS BEGIN]
							device R1 port 1
						[PORTS END]
						[SUBMODULE_PORTS BEGIN]
						[SUBMODULE_PORTS END]
					[SEGMENT END]
					[SEGMENT BEGIN]
						start sheet 1 x 50.80 y 154.12
						end sheet 1 x 68.58 y 154.12
						[LABELS BEGIN]
						[LABELS END]
						[JUNCTIONS BEGIN]
							position x 68.58 y 154.12
						[JUNCTIONS END]
						[PORTS BEGIN]
							device X1 port 6
						[PORTS END]
						[SUBMODULE_PORTS BEGIN]
						[SUBMODULE_PORTS END]
					[SEGMENT END]
				[SEGMENTS END]
			[STRAND END]
		[STRANDS END]
		[ROUTE BEGIN]
			[LINE BEGIN]
				start x 170.000 y 105.000
				end x 170.000 y 110.000
				layer 1
				width 0.250
			[LINE END]
			[LINE BEGIN]
				start x 166.520 y 113.480
				end x 170.000 y 110.000
				layer 1
				width 0.250
			[LINE END]
			[LINE BEGIN]
				start x 166.520 y 113.480
				end x 165.100 y 113.480
				layer 1
				width 0.250
			[LINE END]
			[VIA BEGIN]
				position x 170.000 y 105.000
				diameter 0.400
				layer_start 1
				layer_end 32
				restring_outer_layers 0.100
				restring_inner_layers 0.100
			[VIA END]
			[POLYGON BEGIN]
				priority 1
				layer 1
				min_width 0.150
				isolation 0.800
				fill_style solid
				hatching_line_width 0.508
				hatching_line_spacing 2.000
				corner_easing chamfer
				easing_radius 1.000
				pad_connection thermal
				pad_technology smt_and_tht
				thermal_width 0.500
				thermal_gap 0.600
				[CORNERS BEGIN]
					position x 160.000 y 115.000
					position x 170.000 y 100.000
					position x 175.000 y 130.000
					position x 180.000 y 125.000
				[CORNERS END]
			[POLYGON END]
		[ROUTE END]
	[NET END]
[NETS END]
[DRAWING_FRAMES BEGIN]
	[SCHEMATIC BEGIN]
		template dummy_frame
	[SCHEMATIC END]
	[BOARD BEGIN]
		template dummy_frame
	[BOARD END]
[DRAWING_FRAMES END]
[TEXTS BEGIN]
	[TEXT BEGIN]
		position sheet 1 x 180.34 y 39.82
		content "ET Test Circuit"
		size 1.70
		line_width 0.33
		rotation 180.0
		style italic
		alignment horizontal center vertical center
	[TEXT END]
[TEXTS END]
[SUBMODULES BEGIN]
[SUBMODULES END]
[DEVICES BEGIN]
	[DEVICE BEGIN]
		name FLG03
		appearance sch
		value PWR_FLAG
		model libraries/devices/__#__#lbr#bel_supply_PWR_FLAG.dev
		[UNITS BEGIN]
			[UNIT BEGIN]
				name 1
				position sheet 1 x 76.20 y 154.12
				rotation 0.0
				mirrored no
				[PLACEHOLDERS BEGIN]
					[PLACEHOLDER BEGIN]
						meaning reference
						position x 76.20 y 54.61
						size 1.30
						line_width 0.00
						rotation 0.0
						style normal
						alignment horizontal center vertical center
					[PLACEHOLDER END]
					[PLACEHOLDER BEGIN]
						meaning value
						position x 76.20 y 59.69
						size 1.30
						line_width 0.00
						rotation 0.0
						style normal
						alignment horizontal center vertical center
					[PLACEHOLDER END]
				[PLACEHOLDERS END]
			[UNIT END]
		[UNITS END]
	[DEVICE END]
	[DEVICE BEGIN]
		name IC2
		appearance sch_pcb
		value 74193
		model libraries/devices/__#__#lbr#bel_logic_74193.dev
		variant S_SO16
		partcode IC_PAC_S_SO16_VAL_74193
		purpose "?PURPOSE?"
		bom yes
		[PACKAGE BEGIN]
			position x 145.000 y 57.854 rotation 180.00 face bottom
			[PLACEHOLDERS BEGIN]
				[PLACEHOLDER BEGIN]
					layer silk_screen
					meaning reference
					position x 0.000 y 5.555 rotation 0.00 face bottom
					size width 1.000 height 1.000
					line_width 0.150
					alignment horizontal center vertical bottom
				[PLACEHOLDER END]
				[PLACEHOLDER BEGIN]
					layer assembly_documentation
					meaning value
					position x 0.000 y 0.000 rotation 270.00 face bottom
					size width 1.000 height 1.000
					line_width 0.150
					alignment horizontal center vertical bottom
				[PLACEHOLDER END]
			[PLACEHOLDERS END]
		[PACKAGE END]
		[UNITS BEGIN]
			[UNIT BEGIN]
				name 1
				position sheet 1 x 147.32 y 143.96
				rotation 0.0
				mirrored no
				[PLACEHOLDERS BEGIN]
					[PLACEHOLDER BEGIN]
						meaning reference
						position x 139.70 y 80.01
						size 1.30
						line_width 0.00
						rotation 0.0
						style normal
						alignment horizontal center vertical center
					[PLACEHOLDER END]
					[PLACEHOLDER BEGIN]
						meaning value
						position x 139.70 y 49.53
						size 1.30
						line_width 0.00
						rotation 0.0
						style normal
						alignment horizontal center vertical center
					[PLACEHOLDER END]
					[PLACEHOLDER BEGIN]
						meaning purpose
						position x 156.21 y 30.48
						size 1.30
						line_width 0.00
						rotation 0.0
						style normal
						alignment horizontal left vertical bottom
					[PLACEHOLDER END]
				[PLACEHOLDERS END]
			[UNIT END]
		[UNITS END]
	[DEVICE END]
	[DEVICE BEGIN]
		name IC10
		appearance sch_pcb
		value 7400
		model libraries/devices/__#__#lbr#bel_logic_7400.dev
		variant S_SO14
		partcode IC_PAC_S_SO14_VAL_7400
		purpose "?PURPOSE?"
		bom yes
		[PACKAGE BEGIN]
			position x 154.940 y 98.240 rotation 0.00 face top
			[PLACEHOLDERS BEGIN]
				[PLACEHOLDER BEGIN]
					layer silk_screen
					meaning reference
					position x 0.000 y -5.225 rotation 0.00 face top
					size width 1.000 height 1.000
					line_width 0.150
					alignment horizontal center vertical bottom
				[PLACEHOLDER END]
				[PLACEHOLDER BEGIN]
					layer assembly_documentation
					meaning value
					position x 0.000 y 0.000 rotation 90.00 face top
					size width 1.000 height 1.000
					line_width 0.150
					alignment horizontal center vertical bottom
				[PLACEHOLDER END]
			[PLACEHOLDERS END]
		[PACKAGE END]
		[UNITS BEGIN]
			[UNIT BEGIN]
				name 1
				position sheet 1 x 104.14 y 108.40
				rotation 0.0
				mirrored no
				[PLACEHOLDERS BEGIN]
					[PLACEHOLDER BEGIN]
						meaning reference
						position x 100.96 y 94.61
						size 1.30
						line_width 0.00
						rotation 0.0
						style normal
						alignment horizontal left vertical bottom
					[PLACEHOLDER END]
					[PLACEHOLDER BEGIN]
						meaning value
						position x 100.96 y 92.71
						size 1.30
						line_width 0.00
						rotation 0.0
						style normal
						alignment horizontal left vertical bottom
					[PLACEHOLDER END]
					[PLACEHOLDER BEGIN]
						meaning purpose
						position x 100.96 y 107.95
						size 1.30
						line_width 0.00
						rotation 0.0
						style normal
						alignment horizontal left vertical bottom
					[PLACEHOLDER END]
				[PLACEHOLDERS END]
			[UNIT END]
			[UNIT BEGIN]
				name 3
				position sheet 1 x 152.40 y 108.40
				rotation 0.0
				mirrored no
				[PLACEHOLDERS BEGIN]
					[PLACEHOLDER BEGIN]
						meaning reference
						position x 149.22 y 94.61
						size 1.30
						line_width 0.00
						rotation 0.0
						style normal
						alignment horizontal left vertical bottom
					[PLACEHOLDER END]
					[PLACEHOLDER BEGIN]
						meaning value
						position x 149.22 y 92.71
						size 1.30
						line_width 0.00
						rotation 0.0
						style normal
						alignment horizontal left vertical bottom
					[PLACEHOLDER END]
					[PLACEHOLDER BEGIN]
						meaning purpose
						position x 149.22 y 107.95
						size 1.30
						line_width 0.00
						rotation 0.0
						style normal
						alignment horizontal left vertical bottom
					[PLACEHOLDER END]
				[PLACEHOLDERS END]
			[UNIT END]
		[UNITS END]
	[DEVICE END]
	[DEVICE BEGIN]
		name LED2
		appearance sch_pcb
		value red
		model libraries/devices/__#__#lbr#bel_primitive_LED.dev
		variant S_0805
		partcode LED_PAC_S_0805_VAL_red
		purpose "PWR ON"
		bom yes
		[PACKAGE BEGIN]
			position x 166.370 y 102.050 rotation 180.00 face top
			[PLACEHOLDERS BEGIN]
				[PLACEHOLDER BEGIN]
					layer silk_screen
					meaning reference
					position x 0.000 y -1.750 rotation 180.00 face top
					size width 1.000 height 1.000
					line_width 0.150
					alignment horizontal center vertical bottom
				[PLACEHOLDER END]
				[PLACEHOLDER BEGIN]
					layer assembly_documentation
					meaning value
					position x 0.000 y 1.750 rotation 180.00 face top
					size width 1.000 height 1.000
					line_width 0.150
					alignment horizontal center vertical bottom
				[PLACEHOLDER END]
			[PLACEHOLDERS END]
		[PACKAGE END]
		[UNITS BEGIN]
			[UNIT BEGIN]
				name 1
				position sheet 1 x 68.58 y 137.61
				rotation 0.0
				mirrored no
				[PLACEHOLDERS BEGIN]
					[PLACEHOLDER BEGIN]
						meaning reference
						position x 71.12 y 72.39
						size 1.30
						line_width 0.00
						rotation 90.0
						style normal
						alignment horizontal center vertical center
					[PLACEHOLDER END]
					[PLACEHOLDER BEGIN]
						meaning value
						position x 73.02 y 72.39
						size 1.30
						line_width 0.00
						rotation 90.0
						style normal
						alignment horizontal center vertical center
					[PLACEHOLDER END]
					[PLACEHOLDER BEGIN]
						meaning purpose
						position x 76.20 y 68.58
						size 1.30
						line_width 0.00
						rotation 90.0
						style normal
						alignment horizontal left vertical bottom
					[PLACEHOLDER END]
				[PLACEHOLDERS END]
			[UNIT END]
		[UNITS END]
	[DEVICE END]
	[DEVICE BEGIN]
		name N1
		appearance sch_pcb
		value NETCHANGER
		model libraries/devices/__#__#lbr#bel_primitive_NETCHANGER.dev
		variant 0mm2
		partcode ?PARTCODE?
		purpose "?PURPOSE?"
		bom no
		[PACKAGE BEGIN]
			position x 148.590 y 107.130 rotation 180.00 face top
			[PLACEHOLDERS BEGIN]
				[PLACEHOLDER BEGIN]
					layer silk_screen
					meaning reference
					position x 0.000 y -1.900 rotation 0.00 face top
					size width 1.000 height 1.000
					line_width 0.150
					alignment horizontal center vertical bottom
				[PLACEHOLDER END]
				[PLACEHOLDER BEGIN]
					layer assembly_documentation
					meaning value
					position x 0.000 y 1.900 rotation 0.00 face top
					size width 1.000 height 1.000
					line_width 0.150
					alignment horizontal center vertical bottom
				[PLACEHOLDER END]
			[PLACEHOLDERS END]
		[PACKAGE END]
		[UNITS BEGIN]
			[UNIT BEGIN]
				name 1
				position sheet 1 x 201.93 y 132.53
				rotation 0.0
				mirrored no
				[PLACEHOLDERS BEGIN]
					[PLACEHOLDER BEGIN]
						meaning reference
						position x 201.93 y 77.47
						size 1.30
						line_width 0.00
						rotation 0.0
						style normal
						alignment horizontal center vertical center
					[PLACEHOLDER END]
					[PLACEHOLDER BEGIN]
						meaning value
						position x 201.93 y 77.47
						size 1.30
						line_width 0.00
						rotation 0.0
						style normal
						alignment horizontal center vertical center
					[PLACEHOLDER END]
					[PLACEHOLDER BEGIN]
						meaning purpose
						position x 210.82 y 67.31
						size 1.30
						line_width 0.00
						rotation 0.0
						style normal
						alignment horizontal left vertical bottom
					[PLACEHOLDER END]
				[PLACEHOLDERS END]
			[UNIT END]
		[UNITS END]
	[DEVICE END]
	[DEVICE BEGIN]
		name N2
		appearance sch_pcb
		value NETCHANGER
		model libraries/devices/__#__#lbr#bel_primitive_NETCHANGER.dev
		variant 0mm2
		partcode ?PARTCODE?
		purpose "?PURPOSE?"
		bom no
		[PACKAGE BEGIN]
			position x 148.590 y 102.050 rotation 180.00 face top
			[PLACEHOLDERS BEGIN]
				[PLACEHOLDER BEGIN]
					layer silk_screen
					meaning reference
					position x 0.000 y -1.900 rotation 0.00 face top
					size width 1.000 height 1.000
					line_width 0.150
					alignment horizontal center vertical bottom
				[PLACEHOLDER END]
				[PLACEHOLDER BEGIN]
					layer assembly_documentation
					meaning value
					position x 0.000 y 1.900 rotation 0.00 face top
					size width 1.000 height 1.000
					line_width 0.150
					alignment horizontal center vertical bottom
				[PLACEHOLDER END]
			[PLACEHOLDERS END]
		[PACKAGE END]
		[UNITS BEGIN]
			[UNIT BEGIN]
				name 1
				position sheet 1 x 201.93 y 124.91
				rotation 0.0
				mirrored no
				[PLACEHOLDERS BEGIN]
					[PLACEHOLDER BEGIN]
						meaning reference
						position x 201.93 y 85.09
						size 1.30
						line_width 0.00
						rotation 0.0
						style normal
						alignment horizontal center vertical center
					[PLACEHOLDER END]
					[PLACEHOLDER BEGIN]
						meaning value
						position x 201.93 y 85.09
						size 1.30
						line_width 0.00
						rotation 0.0
						style normal
						alignment horizontal center vertical center
					[PLACEHOLDER END]
					[PLACEHOLDER BEGIN]
						meaning purpose
						position x 210.82 y 74.93
						size 1.30
						line_width 0.00
						rotation 0.0
						style normal
						alignment horizontal left vertical bottom
					[PLACEHOLDER END]
				[PLACEHOLDERS END]
			[UNIT END]
		[UNITS END]
	[DEVICE END]
	[DEVICE BEGIN]
		name N3
		appearance sch_pcb
		value NETCHANGER
		model libraries/devices/__#__#lbr#bel_primitive_NETCHANGER.dev
		variant 0mm2
		partcode ?PARTCODE?
		purpose "?PURPOSE?"
		bom no
		[PACKAGE BEGIN]
			position x 148.501 y 104.996 rotation 180.00 face top
			[PLACEHOLDERS BEGIN]
				[PLACEHOLDER BEGIN]
					layer silk_screen
					meaning reference
					position x 0.000 y -1.900 rotation 0.00 face top
					size width 1.000 height 1.000
					line_width 0.150
					alignment horizontal center vertical bottom
				[PLACEHOLDER END]
				[PLACEHOLDER BEGIN]
					layer assembly_documentation
					meaning value
					position x 0.000 y 1.900 rotation 0.00 face top
					size width 1.000 height 1.000
					line_width 0.150
					alignment horizontal center vertical bottom
				[PLACEHOLDER END]
			[PLACEHOLDERS END]
		[PACKAGE END]
		[UNITS BEGIN]
			[UNIT BEGIN]
				name 1
				position sheet 1 x 201.93 y 117.29
				rotation 0.0
				mirrored no
				[PLACEHOLDERS BEGIN]
					[PLACEHOLDER BEGIN]
						meaning reference
						position x 201.93 y 92.71
						size 1.30
						line_width 0.00
						rotation 0.0
						style normal
						alignment horizontal center vertical center
					[PLACEHOLDER END]
					[PLACEHOLDER BEGIN]
						meaning value
						position x 201.93 y 92.71
						size 1.30
						line_width 0.00
						rotation 0.0
						style normal
						alignment horizontal center vertical center
					[PLACEHOLDER END]
					[PLACEHOLDER BEGIN]
						meaning purpose
						position x 210.82 y 82.55
						size 1.30
						line_width 0.00
						rotation 0.0
						style normal
						alignment horizontal left vertical bottom
					[PLACEHOLDER END]
				[PLACEHOLDERS END]
			[UNIT END]
		[UNITS END]
	[DEVICE END]
	[DEVICE BEGIN]
		name PWR01
		appearance sch
		value GND
		model libraries/devices/__#__#lbr#bel_supply_GND.dev
		[UNITS BEGIN]
			[UNIT BEGIN]
				name 1
				position sheet 1 x 147.32 y 96.97
				rotation 0.0
				mirrored no
				[PLACEHOLDERS BEGIN]
					[PLACEHOLDER BEGIN]
						meaning reference
						position x 147.32 y 107.95
						size 1.30
						line_width 0.00
						rotation 0.0
						style normal
						alignment horizontal center vertical center
					[PLACEHOLDER END]
					[PLACEHOLDER BEGIN]
						meaning value
						position x 147.32 y 110.49
						size 1.30
						line_width 0.00
						rotation 0.0
						style normal
						alignment horizontal center vertical center
					[PLACEHOLDER END]
				[PLACEHOLDERS END]
			[UNIT END]
		[UNITS END]
	[DEVICE END]
	[DEVICE BEGIN]
		name PWR02
		appearance sch
		value P3V3
		model libraries/devices/__#__#lbr#bel_supply_P3V3.dev
		[UNITS BEGIN]
			[UNIT BEGIN]
				name 1
				position sheet 1 x 87.63 y 154.12
				rotation 0.0
				mirrored no
				[PLACEHOLDERS BEGIN]
					[PLACEHOLDER BEGIN]
						meaning reference
						position x 87.63 y 52.07
						size 1.30
						line_width 0.00
						rotation 0.0
						style normal
						alignment horizontal center vertical center
					[PLACEHOLDER END]
					[PLACEHOLDER BEGIN]
						meaning value
						position x 87.63 y 59.44
						size 1.30
						line_width 0.00
						rotation 0.0
						style normal
						alignment horizontal center vertical center
					[PLACEHOLDER END]
				[PLACEHOLDERS END]
			[UNIT END]
		[UNITS END]
	[DEVICE END]
	[DEVICE BEGIN]
		name R1
		appearance sch_pcb
		value 100R
		model libraries/devices/__#__#lbr#bel_primitive_RESISTOR.dev
		variant S_0805
		partcode R_PAC_S_0805_VAL_100R
		purpose "?PURPOSE?"
		bom yes
		[PACKAGE BEGIN]
			position x 162.560 y 98.240 rotation 180.00 face top
			[PLACEHOLDERS BEGIN]
				[PLACEHOLDER BEGIN]
					layer silk_screen
					meaning reference
					position x 0.000 y -2.100 rotation 0.00 face top
					size width 1.000 height 1.000
					line_width 0.150
					alignment horizontal center vertical bottom
				[PLACEHOLDER END]
				[PLACEHOLDER BEGIN]
					layer assembly_documentation
					meaning value
					position x 0.000 y 2.100 rotation 0.00 face top
					size width 1.000 height 1.000
					line_width 0.150
					alignment horizontal center vertical bottom
				[PLACEHOLDER END]
			[PLACEHOLDERS END]
		[PACKAGE END]
		[UNITS BEGIN]
			[UNIT BEGIN]
				name 1
				position sheet 1 x 68.58 y 149.04
				rotation 0.0
				mirrored no
				[PLACEHOLDERS BEGIN]
					[PLACEHOLDER BEGIN]
						meaning reference
						position x 70.61 y 60.96
						size 1.30
						line_width 0.00
						rotation 90.0
						style normal
						alignment horizontal center vertical center
					[PLACEHOLDER END]
					[PLACEHOLDER BEGIN]
						meaning value
						position x 72.39 y 60.96
						size 1.30
						line_width 0.00
						rotation 90.0
						style normal
						alignment horizontal center vertical center
					[PLACEHOLDER END]
					[PLACEHOLDER BEGIN]
						meaning purpose
						position x 73.66 y 50.80
						size 1.30
						line_width 0.00
						rotation 0.0
						style normal
						alignment horizontal left vertical bottom
					[PLACEHOLDER END]
				[PLACEHOLDERS END]
			[UNIT END]
		[UNITS END]
	[DEVICE END]
	[DEVICE BEGIN]
		name R002
		appearance sch_pcb
		value 220R
		model libraries/devices/__#__#lbr#bel_primitive_RESISTOR.dev
		variant S_0805
		partcode R_PAC_S_0805_VAL_220R
		purpose "?PURPOSE?"
		bom yes
		[PACKAGE BEGIN]
			position x 162.560 y 105.860 rotation 180.00 face top
			[PLACEHOLDERS BEGIN]
				[PLACEHOLDER BEGIN]
					layer silk_screen
					meaning reference
					position x 0.000 y -2.100 rotation 0.00 face top
					size width 1.000 height 1.000
					line_width 0.150
					alignment horizontal center vertical bottom
				[PLACEHOLDER END]
				[PLACEHOLDER BEGIN]
					layer assembly_documentation
					meaning value
					position x 0.000 y 2.100 rotation 0.00 face top
					size width 1.000 height 1.000
					line_width 0.150
					alignment horizontal center vertical bottom
				[PLACEHOLDER END]
			[PLACEHOLDERS END]
		[PACKAGE END]
		[UNITS BEGIN]
			[UNIT BEGIN]
				name 1
				position sheet 1 x 187.96 y 102.05
				rotation 0.0
				mirrored no
				[PLACEHOLDERS BEGIN]
					[PLACEHOLDER BEGIN]
						meaning reference
						position x 189.99 y 107.95
						size 1.30
						line_width 0.00
						rotation 90.0
						style normal
						alignment horizontal center vertical center
					[PLACEHOLDER END]
					[PLACEHOLDER BEGIN]
						meaning value
						position x 191.77 y 107.95
						size 1.30
						line_width 0.00
						rotation 90.0
						style normal
						alignment horizontal center vertical center
					[PLACEHOLDER END]
					[PLACEHOLDER BEGIN]
						meaning purpose
						position x 193.04 y 97.79
						size 1.30
						line_width 0.00
						rotation 0.0
						style normal
						alignment horizontal left vertical bottom
					[PLACEHOLDER END]
				[PLACEHOLDERS END]
			[UNIT END]
		[UNITS END]
	[DEVICE END]
	[DEVICE BEGIN]
		name X1
		appearance sch_pcb
		value FEMALE_01X06
		model libraries/devices/__#__#lbr#bel_connector_and_jumper_FEMALE_01X06.dev
		variant T_FEMALE_01X06_VERTICAL_PITCH_2mm54
		partcode X_PAC_T_FEMALE_01X06_VERTICAL_PITCH_2mm54_VAL_FEMALE_01X06
		purpose "PWR CTRL IN"
		bom yes
		[PACKAGE BEGIN]
			position x 152.400 y 113.480 rotation 180.00 face top
			[PLACEHOLDERS BEGIN]
				[PLACEHOLDER BEGIN]
					layer silk_screen
					meaning reference
					position x -0.508 y 3.302 rotation 0.00 face top
					size width 1.000 height 1.000
					line_width 0.150
					alignment horizontal center vertical bottom
				[PLACEHOLDER END]
				[PLACEHOLDER BEGIN]
					layer assembly_documentation
					meaning value
					position x 5.588 y -3.556 rotation 0.00 face top
					size width 1.000 height 1.000
					line_width 0.150
					alignment horizontal center vertical bottom
				[PLACEHOLDER END]
			[PLACEHOLDERS END]
		[PACKAGE END]
		[UNITS BEGIN]
			[UNIT BEGIN]
				name 1
				position sheet 1 x 46.99 y 141.42
				rotation 180.0
				mirrored no
				[PLACEHOLDERS BEGIN]
					[PLACEHOLDER BEGIN]
						meaning reference
						position x 48.26 y 71.12
						size 1.30
						line_width 0.00
						rotation 0.0
						style normal
						alignment horizontal center vertical center
					[PLACEHOLDER END]
					[PLACEHOLDER BEGIN]
						meaning value
						position x 50.80 y 62.23
						size 1.30
						line_width 0.00
						rotation 90.0
						style normal
						alignment horizontal center vertical center
					[PLACEHOLDER END]
					[PLACEHOLDER BEGIN]
						meaning purpose
						position x 54.61 y 55.88
						size 1.30
						line_width 0.00
						rotation 90.0
						style normal
						alignment horizontal left vertical bottom
					[PLACEHOLDER END]
				[PLACEHOLDERS END]
			[UNIT END]
		[UNITS END]
	[DEVICE END]
[DEVICES END]
[BOARD BEGIN]
	[SILK_SCREEN BEGIN]
		[TOP BEGIN]
			[LINE BEGIN]
				start x 165.000 y 125.000
				end x 150.000 y 125.000
				width 0.200
			[LINE END]
			[ARC BEGIN]
				center x 160.000 y 140.000
				start x 165.000 y 140.000
				end x 0.000 y 210.000
				width 0.200
			[ARC END]
			[CIRCLE BEGIN]
				center x 175.000 y 150.000
				radius 5.000
				width 0.200
				filled no
				fill_style solid
				hatching_line_width 0.300
				hatching_line_spacing 2.000
			[CIRCLE END]

			[TEXT BEGIN]
				position x 40 y 20 rotation 0
				size width 3.0 height 3.0
				line_width 0.2
				content "bla"
				alignment horizontal center vertical center
			[TEXT END]
			
			[PLACEHOLDER BEGIN]
				position x 40 y 20 rotation 0
				size width 3.0 height 3.0
				line_width 0.2
				meaning project_name
				alignment horizontal center vertical center
			[PLACEHOLDER END]

		[TOP END]
		[BOTTOM BEGIN]
			[LINE BEGIN]
				start x 145.000 y 125.000
				end x 135.000 y 125.000
				width 0.200
			[LINE END]
			[ARC BEGIN]
				center x 175.000 y 140.000
				start x 175.000 y 135.000
				end x 0.000 y 210.000
				width 0.200
			[ARC END]
			[CIRCLE BEGIN]
				center x 190.000 y 150.000
				radius 5.000
				width 0.200
				filled no
				fill_style solid
				hatching_line_width 0.300
				hatching_line_spacing 2.000
			[CIRCLE END]
			[POLYGON BEGIN]
				fill_style solid
				corner_easing none
				easing_radius 0.3
				hatching_line_width 0.3
				hatching_line_spacing 1
				[CORNERS BEGIN]
					position x 123.54 y 2.7
					position x 133.54 y 335.3
					position x 523.54 y 6.7
				[CORNERS END]
			[POLYGON END]
		[BOTTOM END]
	[SILK_SCREEN END]
	[ASSEMBLY_DOCUMENTATION BEGIN]
		[TOP BEGIN]
			[LINE BEGIN]
				start x 165.000 y 130.000
				end x 150.000 y 130.000
				width 0.200
			[LINE END]
			[ARC BEGIN]
				center x 140.000 y 140.000
				start x 140.000 y 135.000
				end x 0.000 y 210.000
				width 0.200
			[ARC END]
			[CIRCLE BEGIN]
				center x 175.000 y 165.000
				radius 5.000
				width 0.200
				filled no
				fill_style solid
				hatching_line_width 0.300
				hatching_line_spacing 2.000
			[CIRCLE END]
		[TOP END]
		[BOTTOM BEGIN]
			[LINE BEGIN]
				start x 155.000 y 145.000
				end x 155.000 y 135.000
				width 0.200
			[LINE END]
			[ARC BEGIN]
				center x 145.000 y 140.000
				start x 150.000 y 140.000
				end x 0.000 y 210.000
				width 0.200
			[ARC END]
			[CIRCLE BEGIN]
				center x 190.000 y 165.000
				radius 5.000
				width 0.200
				filled no
				fill_style solid
				hatching_line_width 0.300
				hatching_line_spacing 2.000
			[CIRCLE END]
		[BOTTOM END]
	[ASSEMBLY_DOCUMENTATION END]
	[STENCIL BEGIN]
		[TOP BEGIN]
			[LINE BEGIN]
				start x 145.000 y 130.000
				end x 140.000 y 130.000
				width 0.200
			[LINE END]
			[ARC BEGIN]
				center x 115.000 y 140.000
				start x 115.000 y 135.000
				end x 0.000 y 210.000
				width 0.200
			[ARC END]
			[CIRCLE BEGIN]
				center x 230.000 y 165.000
				radius 5.000
				width 0.200
				filled no
				fill_style solid
				hatching_line_width 0.300
				hatching_line_spacing 2.000
			[CIRCLE END]
		[TOP END]
		[BOTTOM BEGIN]
			[LINE BEGIN]
				start x 135.000 y 130.000
				end x 130.000 y 130.000
				width 0.200
			[LINE END]
			[ARC BEGIN]
				center x 120.000 y 140.000
				start x 125.000 y 140.000
				end x 0.000 y 210.000
				width 0.200
			[ARC END]
			[CIRCLE BEGIN]
				center x 250.000 y 165.000
				radius 5.000
				width 0.200
				filled no
				fill_style solid
				hatching_line_width 0.300
				hatching_line_spacing 2.000
			[CIRCLE END]
		[BOTTOM END]
	[STENCIL END]
	[STOP_MASK BEGIN]
		[TOP BEGIN]
			[ARC BEGIN]
				center x 115.000 y 155.000
				start x 110.000 y 155.000
				end x 0.000 y 210.000
				width 0.200
			[ARC END]
			[CIRCLE BEGIN]
				center x 230.000 y 145.000
				radius 5.000
				width 0.200
				filled no
				fill_style solid
				hatching_line_width 0.300
				hatching_line_spacing 2.000
			[CIRCLE END]
		[TOP END]
		[BOTTOM BEGIN]
			[ARC BEGIN]
				center x 125.000 y 160.000
				start x 125.000 y 155.000
				end x 0.000 y 210.000
				width 0.200
			[ARC END]
			[CIRCLE BEGIN]
				center x 250.000 y 145.000
				radius 5.000
				width 0.200
				filled no
				fill_style solid
				hatching_line_width 0.300
				hatching_line_spacing 2.000
			[CIRCLE END]
		[BOTTOM END]
	[STOP_MASK END]
	[KEEPOUT BEGIN]
		[TOP BEGIN]
			[LINE BEGIN]
				start x 165.000 y 120.000
				end x 150.000 y 120.000
				width 0.200
			[LINE END]
			[ARC BEGIN]
				center x 140.000 y 160.000
				start x 140.000 y 155.000
				end x 0.000 y 210.000
				width 0.200
			[ARC END]
			[CIRCLE BEGIN]
				center x 250.000 y 125.000
				radius 5.000
				width 0.200
				filled no
				fill_style solid
				hatching_line_width 0.300
				hatching_line_spacing 2.000
			[CIRCLE END]
		[TOP END]
		[BOTTOM BEGIN]
			[ARC BEGIN]
				center x 145.000 y 160.000
				start x 150.000 y 160.000
				end x 0.000 y 210.000
				width 0.200
			[ARC END]
			[CIRCLE BEGIN]
				center x 230.000 y 125.000
				radius 5.000
				width 0.200
				filled no
				fill_style solid
				hatching_line_width 0.300
				hatching_line_spacing 2.000
			[CIRCLE END]
		[BOTTOM END]
	[KEEPOUT END]

	[ROUTE_RESTRICT BEGIN]
		[LINE BEGIN]
			start x 22.3 y 34
			end x 10 y 32.5 
			width 0.2
			layers 2 3 15
		[LINE END]
		
		[ARC BEGIN]
			center x 140.000 y 160.000
			start x 140.000 y 155.000
			end x 0.000 y 210.000
			width 0.200
			layers 1 2 12
		[ARC END]
		
		[CIRCLE BEGIN]
			center x 250.000 y 125.000
			radius 5.000
			width 0.200
			filled no
			fill_style solid
			hatching_line_width 0.300
			hatching_line_spacing 2.000
			layers 1 2 12			
		[CIRCLE END]
		
		[POLYGON BEGIN]
			fill_style solid
			corner_easing none
			easing_radius 0.3
			hatching_line_width 0.3
			hatching_line_spacing 1
			width 0.200
			layers 1 2 11
			[CORNERS BEGIN]
				position x 123.54 y 2.7
				position x 133.54 y 335.3
				position x 523.54 y 6.7
			[CORNERS END]
		[POLYGON END]
		
	[ROUTE_RESTRICT END]

	[VIA_RESTRICT BEGIN]
		[LINE BEGIN]
			start x 22.3 y 34
			end x 10 y 32.5 
			width 0.2
			layers 2 3 15
		[LINE END]

		[ARC BEGIN]
			center x 140.000 y 160.000
			start x 140.000 y 155.000
			end x 0.000 y 210.000
			width 0.200
			layers 1 2 12
		[ARC END]
		
		[CIRCLE BEGIN]
			center x 250.000 y 125.000
			radius 5.000
			width 0.200
			filled no
			fill_style solid
			hatching_line_width 0.300
			hatching_line_spacing 2.000
			layers 1 2 12			
		[CIRCLE END]
		
		[POLYGON BEGIN]
			fill_style solid
			corner_easing none
			easing_radius 0.3
			hatching_line_width 0.3
			hatching_line_spacing 1
			layers 1 2 11
			width 0.200
			[CORNERS BEGIN]
				position x 123.54 y 2.7
				position x 133.54 y 335.3
				position x 523.54 y 6.7
			[CORNERS END]
		[POLYGON END]
	
	[VIA_RESTRICT END]

	
	[COPPER BEGIN]
		[LINE BEGIN]
			start x 22.3 y 334.0
			end x 24.3 y 32.5
			layer 2
			width 0.2
		[LINE END]

		[ARC BEGIN]
			center x 45 y 4.2
			start x 42 y 54
			end x 45 y 65
			layer 2
			width 0.5
		[ARC END]

		[CIRCLE BEGIN]
			center x 45 y 4.2
			radius 10
			layer 2
			width 0.5
			filled no
			fill_style solid
			hatching_line_width 0.3
			hatching_line_spacing 1
		[CIRCLE END]
	
		[POLYGON BEGIN]
			priority 2
			isolation 1.2
			fill_style solid
			hatching_line_width 0.508
			hatching_line_spacing 2.000
			corner_easing none
			easing_radius 0.000
			layer 1
			[CORNERS BEGIN]
				position x 131.800 y 108.500
				position x 131.800 y 108.650
				position x 132.000 y 108.500
				position x 132.000 y 108.650
			[CORNERS END]
		[POLYGON END]
		
		[TEXT BEGIN]
			position x 40 y 20 rotation 0
			size width 3.0 height 3.0
			layer 2
			line_width 0.2
			content "bla"
			alignment horizontal center vertical center
		[TEXT END]

		[PLACEHOLDER BEGIN]
			position x 40 y 20 rotation 0
			size width 3.0 height 3.0
			layer 2
			line_width 0.2
			meaning project_name
			alignment horizontal center vertical center
		[PLACEHOLDER END]
		
	[COPPER END]
	
	[PCB_CONTOUR_NON_PLATED BEGIN]
		[LINE BEGIN]
			start x 22.3 y 45
			end x 35 y 32.5
			locked yes
		[LINE END]
		[ARC BEGIN]
			center x 150.000 y 75.000
			start x 155.000 y 70.000
			end x 0.000 y 210.000
			locked no
		[ARC END]
		[ARC BEGIN]
			center x 145.000 y 110.000
			start x 150.000 y 115.000
			end x 0.000 y 210.000
			locked no
		[ARC END]
	[PCB_CONTOUR_NON_PLATED END]
[BOARD END]

-- ======================================================================================================================================================
-- date 2019-01-03T11:39:43
-- module file end


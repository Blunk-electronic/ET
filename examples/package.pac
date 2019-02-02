# SYSTEM ET package
# date 2018-11-06T09:31:48
# ======================================================================================================================================================

description "blahab"
appearance real/virtual
technology SMT/THT

[COPPER BEGIN] # non-electric !
	[TOP BEGIN]
		[LINE BEGIN]
			start x 22.3
			end y 32.5
			width 0.2
		[LINE END]

		[ARC BEGIN]
			center x 45 y 4.2
			start x 42 y 54
			end x 45 y 65
			width 0.5
		[ARC END]

		[CIRCLE BEGIN]
			center x 45 y 4.2
			radius 10
			width 0.5
			filled yes/no
			fill_style solid/hatched/cutout
			hatching_line_width 0.3
			hatching_line_spacing 1
		[CIRCLE END]
		
		[POLYGON BEGIN]
			priority 0
			isolation 0.800
			corner_easing none/chamfer/fillet
			easing_radius 0.3
			fill_style solid,hatched,cutout
			hatching_line_width 0.3
			hatching_line_spacing 1
			min_width 0.3
			[CORNERS BEGIN]
				position x 123.54 y 2.7
				position x 133.54 y 335.3
				position x 523.54 y 6.7
			[CORNERS END]
		[POLYGON END]

		[TEXT BEGIN]
			position x 40 y 20 rotation 0
			size x 3.0 y 3.0
			line_width 0.2
			content "bla"
			alignment center
		[TEXT END]
	
	[TOP END]
[COPPER END]

[KEEPOUT BEGIN]
	[TOP BEGIN]
		[LINE BEGIN]
			start x 22.3
			end y 32.5
			width 0.2
		[LINE END]

		[ARC BEGIN]
			center x 45 y 4.2
			start x 42 y 54
			end x 45 y 65
			width 0.5
		[ARC END]

		[CIRCLE BEGIN]
			center x 45 y 4.2
			radius 10
			width 0.5
			filled yes/no
			fill_style solid/hatched/cutout
			hatching_line_width 0.3
			hatching_line_spacing 1
		[CIRCLE END]
		
		[POLYGON BEGIN]
			corner_easing none/chamfer/fillet
			easing_radius 0.3
			fill_style solid,hatched,cutout
			hatching_line_width 0.3
			hatching_line_spacing 1
			[CORNERS BEGIN]
				position x 123.54 y 2.7
				position x 133.54 y 335.3
				position x 523.54 y 6.7
			[CORNERS END]
		[POLYGON END]
	[TOP END]
[KEEPOUT END]

[STOP_MASK]
	same as keepout but with texts
[STOP_MASK END]

[STENCIL BEGIN]
	same as keepout
[STENCIL END]

[ROUTE_RESTRICT BEGIN]
	[LINE BEGIN]
		start x 22.3
		end y 32.5
		width 0.2
		layers 1 2 15
	[LINE END]

	[ARC BEGIN]
		center x 45 y 4.2
		start x 42 y 54
		end x 45 y 65
		width 0.5
		layers 1 2 15		
	[ARC END]

	[CIRCLE BEGIN]
		center x 45 y 4.2
		radius 10
		width 0.5
		layers 1 2 15		
		filled yes/no
		fill_style solid/hatched/cutout
		hatching_line_width 0.3
		hatching_line_spacing 1
	[CIRCLE END]
	
	[POLYGON BEGIN]
		layers 2 15	
		corner_easing none/chamfer/fillet
		easing_radius 0.3
		fill_style solid,hatched,cutout
		hatching_line_width 0.3
		hatching_line_spacing 1
		[CORNERS BEGIN]
			position x 123.54 y 2.7
			position x 133.54 y 335.3
			position x 523.54 y 6.7
		[CORNERS END]
	[POLYGON END]
[ROUTE_RESTRICT END]

[VIA_RESTRICT BEGIN]
	same as route restrict
[VIA_RESTRICT END]

[PCB_CONTOURS_NON_PLATED BEGIN]
	[LINE BEGIN]
		start x 22.3
		end y 32.5
		locked yes/no
	[LINE END]

	[ARC BEGIN]
		center x 45 y 4.2
		start x 42 y 54
		end x 45 y 65
		locked yes/no
	[ARC END]

	[CIRCLE BEGIN]
		center x 45 y 4.2
		radius 10
		locked yes/no
	[CIRCLE END]
[PCB_CONTOURS_NON_PLATED END]

# if real
[PACKAGE_3D_CONTOURS BEGIN]

[PACKAGE_3D_CONTOURS END]



[SILK_SCREEN BEGIN]
	[TOP BEGIN]
		[LINE BEGIN]
			start x 22.3
			end y 32.5
			width 0.2
		[LINE END]

		[ARC BEGIN]
			center x 45 y 4.2
			start x 42 y 54
			end x 45 y 65
			width 0.5
		[ARC END]

		[CIRCLE BEGIN]
			center x 45 y 4.2
			radius 10
			width 0.5
			filled yes/no
			fill_style solid/hatched/cutout
			hatching_line_width 0.3
			hatching_line_spacing 1
		[CIRCLE END]
		
		[POLYGON BEGIN]
			fill_style solid,hatched,cutout
			corner_easing none/chamfer/fillet
			easing_radius 0.3
			hatching_line_width 0.3
			hatching_line_spacing 1
			[CORNERS BEGIN]
				position x 123.54 y 2.7
				position x 133.54 y 335.3
				position x 523.54 y 6.7
			[CORNERS END]
		[POLYGON END]
		
		[TEXT BEGIN]
			position x 40 y 20 rotation 0
			size width 1.000 height 1.000
			line_width 0.2
			content "bla"
			alignment center
		[TEXT END]

		[PLACEHOLDER BEGIN]
			position x 40 y 20 rotation 0
			size width 1.000 height 1.000
			line_width 0.2
			meaning reference/value/purpose
			alignment center
		[PLACEHOLDER END]
		
	[TOP END]
	
	[BOTTOM BEGIN]
	
	[BOTTOM END]
[SILK_SCREEN END]

[ASSEMBLY_DOCUMENTATION BEGIN]
	same as in SILK_SCREEN
[ASSEMBLY_DOCUMENTATION END]	

[TERMINALS BEGIN]
	[TERMINAL BEGIN]
		name 6
		technology tht
		position x 12.700 y 0.000 rotation 0
		[PAD_CONTOUR BEGIN]
			[TOP BEGIN]
				[LINE BEGIN]
					start x -0.900 y 1.200
					end x -0.900 y -0.800
				[LINE END]
				[LINE BEGIN]
					start x 1.100 y 1.200
					end x 1.100 y -0.800
				[LINE END]
				[ARC BEGIN]
					center x 0.100 y 1.200
					start x -0.900 y 1.200
					end x 1.100 y 1.200
				[ARC END]
				[ARC BEGIN]
					center x 0.100 y -0.800
					start x -0.900 y -0.800
					end x 1.100 y -0.800
				[ARC END]
			[TOP END]
			[BOTTOM BEGIN]
				[LINE BEGIN]
					start x -0.900 y 1.200
					end x -0.900 y -0.800
				[LINE END]
				[LINE BEGIN]
					start x 1.100 y 1.200
					end x 1.100 y -0.800
				[LINE END]
				[ARC BEGIN]
					center x 0.100 y 1.200
					start x -0.900 y 1.200
					end x 1.100 y 1.200
				[ARC END]
				[ARC BEGIN]
					center x 0.100 y -0.800
					start x -0.900 y -0.800
					end x 1.100 y -0.800
				[ARC END]
			[BOTTOM END]
		[PAD_CONTOUR END]
		width_inner_layers 1.000
		hole milled
		[MILLINGS BEGIN]
			[LINE BEGIN]
				start x -0.400 y 1.200
				end x -0.400 y -0.800
				locked no
			[LINE END]
			[LINE BEGIN]
				start x 0.600 y 1.200
				end x 0.600 y -0.800
				locked no
			[LINE END]
			[LINE BEGIN]
				start x -0.400 y 1.200
				end x 0.600 y 1.200
				locked no
			[LINE END]
			[LINE BEGIN]
				start x -0.400 y -0.800
				end x 0.600 y -0.800
				locked no
			[LINE END]
		[MILLINGS END]
	[TERMINAL END]

	[TERMINAL BEGIN]
		name 9
		technology smt
		position x 2.925 y 2.540 rotation 0
		[PAD_CONTOUR BEGIN]
			[LINE BEGIN]
				start x -1.000 y -0.700
				end x -1.000 y 0.700
			[LINE END]
			[LINE BEGIN]
				start x 1.000 y -0.700
				end x 1.000 y 0.700
			[LINE END]
			[ARC BEGIN]
				center x 0.000 y -0.700
				start x -1.000 y -0.700
				end x 1.000 y -0.700
			[ARC END]
			[ARC BEGIN]
				center x 0.000 y 0.700
				start x -1.000 y 0.700
				end x 1.000 y 0.700
			[ARC END]
			[CIRCLE BEGIN]
				center x 0.000 y 0.700
				radius 1
			[CIRCLE END]
		[PAD_CONTOUR END]
		face top
		stop_mask open
		solder_paste applied
	[TERMINAL END]
	
[TERMINALS END]

# ======================================================================================================================================================
# package model file end

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
				x 123.54 y 2.7
				x 133.54 y 335.3
				x 523.54 y 6.7
			[CORNERS END]
		[POLYGON END]

		[TEXT BEGIN]
			position x 40 y 20
			size x 3.0 y 3.0
			line_width 0.2
			rotation 0
			content "bla"
			alignment center
			hidden yes/no
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
				x 123.54 y 2.7
				x 133.54 y 335.3
				x 523.54 y 6.7
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
		layer 2..15
	[LINE END]

	[ARC BEGIN]
		center x 45 y 4.2
		start x 42 y 54
		end x 45 y 65
		width 0.5
		layer 2..15		
	[ARC END]

	[CIRCLE BEGIN]
		center x 45 y 4.2
		radius 10
		width 0.5
		layer 2..15		
		filled yes/no
		fill_style solid/hatched/cutout
		hatching_line_width 0.3
		hatching_line_spacing 1
	[CIRCLE END]
	
	[POLYGON BEGIN]
		layer 2..15	
		corner_easing none/chamfer/fillet
		easing_radius 0.3
		fill_style solid,hatched,cutout
		hatching_line_width 0.3
		hatching_line_spacing 1
		[CORNERS BEGIN]
			x 123.54 y 2.7
			x 133.54 y 335.3
			x 523.54 y 6.7
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

[PCB_CONTOURS_PLATED BEGIN]
	same as PCB_CONTOURS_NON_PLATED
[PCB_CONTOURS_PLATED END]

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
				x 123.54 y 2.7
				x 133.54 y 335.3
				x 523.54 y 6.7
			[CORNERS END]
		[POLYGON END]
		
		[TEXT BEGIN]
			position x 40 y 20
			size x 3.0 y 3.0
			line_width 0.2
			rotation 0
			content "bla"
			alignment center
			hidden yes/no
		[TEXT END]

		[PLACEHOLDER BEGIN]
			position x 40 y 20
			size x 3.0 y 3.0
			line_width 0.2
			rotation 0
			meaning reference/value/purpose
			alignment center
			hidden yes/no
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
		name H7
		technology SMT/THT
		shape circular/non_circular
		tht_hole drilled/milled # relevant if techno is THT
		position x 443.2 y 45.0
		
		#if techno is THT
			width_inner_layers 1.2
			shape octagon/circular/rectangle/long/long_offset
			#if shape circular
				drill_size 0.8
				offset x 4 y 0
			#if shape non_circular
				size x 5 y 3
				#if tht_hole drilled
					drill_size 1
				#if tht_hole milled
					[MILLINGS BEGIN]
					
					[MILLINGS END]
					
		#if techno is SMT
			shape_smt rectangle/circular/long
			face top/bottom
			stop_mask open/closed
			solder_paste none/applied
			#if shape non_circular
				size x 2 y 3
		
	[TERMINAL END]
[TERMINALS END]

# ======================================================================================================================================================
# package model file end

------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                          PRIMITIVE DRAW OPS                              --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2022 Mario Blunk, Blunk electronic          --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

--   For correct displaying set tab width in your editor to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with ada.text_io;				use ada.text_io;
with ada.numerics;				use ada.numerics;
with glib;						use glib;
-- with glib.types;				use glib.types;
-- with Interfaces.C.Strings;
with gtkada.types;

package body et_canvas_primitive_draw_ops is
	
package body pac_draw is

	-- This function converts a x-value from the drawing to a x-value in the view.
	function convert_x (x : in pac_geometry_1.type_distance) 
		return type_view_coordinate is 
	begin
		return type_view_coordinate (x);
	end;

	
	-- This function converts a y-value from the drawing to a y-value in the view.
	function shift_y (
		--y		: in pac_shapes.pac_geometry_1.type_distance;
		--height	: in pac_shapes.pac_geometry_1.type_distance)
		y		: in type_float_internal;
		height	: in type_float_internal)
		return type_view_coordinate is
	begin
		return type_view_coordinate (height - y);
	end;

	
	function shift_y (
		y		: in pac_geometry_1.type_distance;
		height	: in type_float_internal)
		return type_view_coordinate is
	begin
		return type_view_coordinate (height - type_float_internal (y));
	end;


	
	--function shift_y (
		--y		: in pac_shapes.pac_geometry_1.type_distance;
		--height	: in pac_shapes.pac_geometry_1.type_distance)
		--return pac_shapes.pac_geometry_1.type_distance is
	--begin
		--return (height - y);
	--end;

	function shift_y (
		y		: in type_float_internal;
		height	: in type_float_internal)
		return type_float_internal is
	begin
		return (height - y);
	end;

	
	--function shift_y (
		--y		: in type_distance;
		--height	: in type_float_internal)
		--return type_float_internal is
	--begin
		--return (height - type_float_internal (y));
	--end;


	
	function make_bounding_box (
		height		: in pac_geometry_1.type_float_internal; -- pac_shapes.pac_geometry_1.type_distance;
		boundaries	: in pac_geometry_1.type_boundaries)
		return pac_geometry_1.type_rectangle 
	is begin
		--put_line (to_string (boundaries));
		
		return (
			-- The bounding box origin is the upper left corner.
			-- The box position in x is the smallest_x.
			-- The box position in y is the greatest_y (upwards going axis).
			-- Since the bounding box is something required in the model plane,
			-- the box position in y is afterwards converted to y axis going downwards.
			x		=> boundaries.smallest_x,
			y		=> shift_y (boundaries.greatest_y, height),

			-- The box width is the difference between greatest x and smallest x.
			-- The box height is the difference between greatest y and smallest y.
			width	=> boundaries.greatest_x - boundaries.smallest_x,
			height	=> boundaries.greatest_y - boundaries.smallest_y
			);
	end make_bounding_box;

	
	procedure draw_line (
		area	: in type_rectangle;
		context	: in type_draw_context;
		line	: in type_line'class;
		width	: in type_distance_positive;
		--height	: in pac_shapes.pac_geometry_1.type_distance)
		height	: in type_float_internal_positive)
	is
		-- compute the boundaries (greatest/smallest x/y) of the given line:
		boundaries : type_boundaries := get_boundaries (line, width);

		-- compute the bounding box of the given line
		bounding_box : type_rectangle := make_bounding_box (height, boundaries);
	begin
		-- We draw the segment if:
		--  - no area given or
		--  - if the bounding box of the segment intersects the given area
		if (area = no_rectangle
			or else intersects (area, bounding_box)) 
		then
			--put_line (to_string (line));
-- 			put_line (to_string (boundaries));
-- 			put_line (to_string (bounding_box));
	-- CS test size 
	-- 			if not size_above_threshold (self, context.view) then
	-- 				return;
	-- 			end if;

			-- The ends of the line are round:
			set_line_cap (context.cr, cairo_line_cap_round);
-- 			set_line_join (context.cr, cairo_line_join_miter);
			
			-- start point
			move_to (
				context.cr,
				convert_x (get_x (line.start_point)),
				shift_y (get_y (line.start_point), height)
				);

			-- end point
			line_to (
				context.cr,
				convert_x (get_x (line.end_point)),
				shift_y (get_y (line.end_point), height)
				);

			stroke (context.cr);
		end if;
	end draw_line;

	
	procedure draw_arc (
		area	: in type_rectangle;
		context	: in type_draw_context;
		arc		: in type_arc'class;
		width	: in type_distance_positive;
		--		height	: in pac_shapes.pac_geometry_1.type_distance)
		height	: in type_float_internal_positive)
	is
		-- compute the boundaries (greatest/smallest x/y) of the given arc:
		boundaries : type_boundaries := get_boundaries (arc, width);

		-- compute the bounding box of the given arc
		bounding_box : type_rectangle := make_bounding_box (height, boundaries);

		-- Convert the given arc so that it is expressed by start and end arc:
		arc_temp : type_arc_angles := to_arc_angles (arc);
	begin
		-- We draw the segment if:
		--  - no area given or
		--  - if the bounding box of the segment intersects the given area
		if (area = no_rectangle
			or else intersects (area, bounding_box)) 
		then
	-- CS test size 
	-- 			if not size_above_threshold (self, context.view) then
	-- 				return;
	-- 			end if;
			
			new_sub_path (context.cr); -- required to suppress an initial line

			-- The ends of the arc are round:
			set_line_cap (context.cr, cairo_line_cap_round);
			
			if arc.direction = CW then
				
				cairo.arc (
					context.cr,
					xc		=> convert_x (get_x (arc_temp.center)),
					yc		=> shift_y (get_y (arc_temp.center), height),
					radius	=> type_view_coordinate (arc_temp.radius),
					angle1	=> - type_view_coordinate (to_radians (arc_temp.angle_start)),
					angle2	=> - type_view_coordinate (to_radians (arc_temp.angle_end))
					);

			else -- CCW
				
				cairo.arc_negative (
					context.cr,
					xc		=> convert_x (get_x (arc_temp.center)),
					yc		=> shift_y (get_y (arc_temp.center), height),
					radius	=> type_view_coordinate (arc_temp.radius),
					angle1	=> - type_view_coordinate (to_radians (arc_temp.angle_start)),
					angle2	=> - type_view_coordinate (to_radians (arc_temp.angle_end))
					);
			end if;

			stroke (context.cr);
		end if;
	end draw_arc;

	
	procedure draw_circle (
		area	: in type_rectangle;
		context	: in type_draw_context;
		circle	: in type_circle'class;
		filled	: in type_filled;
		width	: in type_distance_positive;
		--height	: in pac_shapes.pac_geometry_1.type_distance)
		height	: in type_float_internal_positive)
	is
		-- compute the boundaries (greatest/smallest x/y) of the given circle:
		boundaries : type_boundaries := get_boundaries (circle, width);

		-- compute the bounding box of the given arc
		bounding_box : type_rectangle := make_bounding_box (height, boundaries);

		-- backup previous line width
		line_width_before : constant type_view_coordinate := get_line_width (context.cr);
	begin

		-- We draw the segment if:
		--  - no area given or
		--  - if the bounding box of the segment intersects the given area
		if (area = no_rectangle
			or else intersects (area, bounding_box)) 
		then
			null;
	-- CS test size 
	-- 			if not size_above_threshold (self, context.view) then
	-- 				return;
	-- 			end if;
			
			new_sub_path (context.cr); -- required to suppress an initial line

			cairo.arc (
				context.cr,
				xc		=> convert_x (get_x (circle.center)),
				yc		=> shift_y (get_y (circle.center), height),
				radius	=> type_view_coordinate (circle.radius),

				-- it must be a full circle starting at 0 degree and ending at 360 degree:
				angle1	=> 0.0,
				angle2	=> type_view_coordinate (2 * pi)				
				);

			case filled is
				when YES => 
					fill (context.cr);

					-- A filled circle has always line width of zero:
					cairo.set_line_width (context.cr, type_view_coordinate (zero));
					stroke (context.cr);

					-- Restore line width as it was before this procedure:
					cairo.set_line_width (context.cr, line_width_before);
					
				when NO =>
					stroke (context.cr);
			end case;

		end if;
	end draw_circle;


	
	procedure draw_polygon (
		area	: in type_rectangle;
		context	: in type_draw_context;
		polygon	: in type_polygon;
		filled	: in type_filled;
		width	: in type_distance_positive;
		-- CS fill style

		--height	: in pac_shapes.pac_geometry_1.type_distance;
		height	: in type_float_internal_positive;
		drawn	: in out boolean)
	is
		-- compute the boundaries (greatest/smallest x/y) of the given polygon:
		boundaries : constant type_boundaries := get_boundaries (polygon, width);

		-- compute the bounding box of the given contour
		bounding_box : constant type_rectangle := make_bounding_box (height, boundaries);

		-- backup previous line width
		line_width_before : constant type_view_coordinate := get_line_width (context.cr);

		use pac_edges;
		

		procedure query_edge (c : in pac_edges.cursor) is begin

		-- CS query area vs bounding box ?
		
				-- start point
				line_to (
					context.cr,
					convert_x (get_x (element (c).start_point)),
					shift_y (get_y (element (c).start_point), height)
					);
					
				-- end point
				line_to (
					context.cr,
					convert_x (get_x (element (c).end_point)),
					shift_y (get_y (element (c).end_point), height)
					);

		end query_edge;


		-- CS: round caps on all edges ! currently only start and end of path have round caps.
		
	begin -- draw_polygon

		-- We draw the polygon if:
		--  - no area given or
		--  - if the bounding box of the contour intersects the given area
		if (area = no_rectangle
			or else intersects (area, bounding_box)) 
		then
			
	-- CS test size 
	-- 			if not size_above_threshold (self, context.view) then
	-- 				return;
	-- 			end if;

			new_sub_path (context.cr); -- required to suppress an initial line

			-- draw the edges
			polygon.edges.iterate (query_edge'access);

			
			case filled is
				when YES => 
					fill (context.cr);
					set_line_width (context.cr, type_view_coordinate (zero));
					
				when NO =>
					-- The ends of the line are round:
					set_line_cap (context.cr, cairo_line_cap_round);

			end case;

			stroke (context.cr);

			-- Restore line width as it was before this procedure:
			set_line_width (context.cr, line_width_before);
			
			-- The polygon has been drawn:
			drawn := true;			
		else
			-- The polygon has not been drawn:
			drawn := false;
		end if;
		
	end draw_polygon;
	

	
	procedure draw_contour (
		area	: in type_rectangle;
		context	: in type_draw_context;
		contour	: in type_contour'class;
		filled	: in type_filled;
		width	: in type_distance_positive;
		-- CS fill style

		--height	: in pac_shapes.pac_geometry_1.type_distance;
		height	: in type_float_internal_positive;
		drawn	: in out boolean)
	is
		-- compute the boundaries (greatest/smallest x/y) of the given contour:
		boundaries : constant type_boundaries := get_boundaries (contour, width);

		-- compute the bounding box of the given contour
		bounding_box : constant type_rectangle := make_bounding_box (height, boundaries);

		-- backup previous line width
		line_width_before : constant type_view_coordinate := get_line_width (context.cr);

		use pac_contour_segments;
		
		-- For cairo, en arc must be expressed by start and end arc:
		arc_temp : type_arc_angles;

		-- If the contour is not to be filled, then its contours must be drawn 
		-- with a line widht that depends on the current scale:
		scale : type_scale;

		-- The line style, or the dash pattern, will be calculated 
		-- according to the current scale:
		dash_on, dash_off	: gdouble;
		dash_pattern		: dash_array (1 .. 2);

		procedure query_segment (c : in pac_contour_segments.cursor) is begin

			-- CS query area vs bounding box ?
			
			case element (c).shape is
				
				when LINE =>

					-- start point
					line_to (
						context.cr,
						convert_x (get_x (element (c).segment_line.start_point)),
						shift_y (get_y (element (c).segment_line.start_point), height)
						);
						
					-- end point
					line_to (
						context.cr,
						convert_x (get_x (element (c).segment_line.end_point)),
						shift_y (get_y (element (c).segment_line.end_point), height)
						);

					
				when ARC =>
					-- Convert the segment to a type that uses start and end angles.
					-- The angles of arc_temp will later be inverted for cairo draw operation:
					arc_temp := to_arc_angles (element (c).segment_arc);
					
					if element (c).segment_arc.direction = CW then
						--put_line ("CW");
						
						cairo.arc (
							context.cr,
							xc		=> convert_x (get_x (arc_temp.center)),
							yc		=> shift_y (get_y (arc_temp.center), height),
							radius	=> type_view_coordinate (arc_temp.radius),
							angle1	=> - type_view_coordinate (to_radians (arc_temp.angle_start)),
							angle2	=> - type_view_coordinate (to_radians (arc_temp.angle_end))
							);

					else -- CCW
						--put_line ("CCW start" & to_string (element (ca).start_point) & " angle" 
						--		  & to_string (arc_temp.angle_start));

						--put_line ("CCW end  " & to_string (element (ca).end_point) & " angle" 
						--		  & to_string (arc_temp.angle_end));
						
						cairo.arc_negative (
							context.cr,
							xc		=> convert_x (get_x (arc_temp.center)),
							yc		=> shift_y (get_y (arc_temp.center), height),
							radius	=> type_view_coordinate (arc_temp.radius),
							angle1	=> - type_view_coordinate (to_radians (arc_temp.angle_start)),
							angle2	=> - type_view_coordinate (to_radians (arc_temp.angle_end))
							);
					end if;

			end case;
		end query_segment;

		
	begin -- draw_contour

		-- We draw the contour if:
		--  - no area given or
		--  - if the bounding box of the contour intersects the given area
		if (area = no_rectangle
			or else intersects (area, bounding_box)) 
		then
			
	-- CS test size 
	-- 			if not size_above_threshold (self, context.view) then
	-- 				return;
	-- 			end if;

			new_sub_path (context.cr); -- required to suppress an initial line

			if contour.contour.circular then

				-- Draw the single circle that forms the contour:

				if filled = YES then
					fill (context.cr);
				end if;
				
				-- CS: The intersection between circle and other segments
				-- is still visible as a very thin line.

				-- CS: All segments of the contour must have the same color.
				-- Where the circle overlaps the contour the brightness increases.

				cairo.arc (
					context.cr,
					xc		=> convert_x (get_x (contour.contour.circle.center)),
					yc		=> shift_y (get_y (contour.contour.circle.center), height),
					radius	=> type_view_coordinate (contour.contour.circle.radius),

					-- it must be a full circle starting at 0 degree and ending at 360 degree:
					angle1	=> 0.0,
					angle2	=> type_view_coordinate (2 * pi)				
					);


				
			else
				-- move lines and arcs:
				contour.contour.segments.iterate (query_segment'access);
			end if;

			
			case filled is
				when YES => 
					fill (context.cr);
					set_line_width (context.cr, type_view_coordinate (zero));
					
				when NO =>
					-- Calculate the line width of the contours:
					scale := get_scale (canvas);
					set_line_width (context.cr, type_view_coordinate (0.01 + 1.0 / scale));
					
					-- The ends of the line are round:
					set_line_cap (context.cr, cairo_line_cap_round);

					--dash_on := 0.2 + 1.0 / scale;
					--dash_off := 0.1 + 1.0 / scale;
					dash_on := 20.0 / scale;
					dash_off := 15.0 / scale;

					dash_pattern (1) := dash_on;
					dash_pattern (2) := dash_off;
					set_dash (context.cr, dash_pattern, 0.0);
			end case;

			stroke (context.cr);

			-- Restore line width as it was before this procedure:
			set_line_width (context.cr, line_width_before);

			-- Disable line dashes:
			set_dash (context.cr, no_dashes, 0.0);

			
			-- The contour has been drawn:
			drawn := true;			
		else
			-- The contour has not been drawn:
			drawn := false;
		end if;
		
	end draw_contour;


	procedure draw_contour_with_circular_cutout (
		area			: in type_rectangle;
		context			: in type_draw_context;
		outer_border	: in type_contour'class;
		inner_border	: in type_circle'class;
		--height			: in pac_shapes.pac_geometry_1.type_distance)
		height	: in type_float_internal_positive)
	is 
		drawn : boolean := false;
	begin
		-- Since this is about filled areas, the line width must be zero:
		set_line_width (context.cr, type_view_coordinate (zero));
		
		-- draw outer contour with outer border
		draw_contour (area, context, outer_border, YES, zero, height, drawn);

		-- the cutout area must clear out the outer area:
		set_operator (context.cr, CAIRO_OPERATOR_CLEAR);
		
		-- draw inner area to be taken out:
		draw_circle (area, context, inner_border, YES, zero, height);

		-- restore default compositing operator:
		set_operator (context.cr, CAIRO_OPERATOR_OVER);		
	end draw_contour_with_circular_cutout;


	procedure draw_contour_with_arbitrary_cutout (
		area			: in type_rectangle;
		context			: in type_draw_context;
		outer_border	: in type_contour'class;
		inner_border	: in type_contour'class;
		--height			: in pac_shapes.pac_geometry_1.type_distance)
		height	: in type_float_internal_positive)
	is 
		drawn : boolean := false;
	begin
		-- Since this is about filled areas, the line width must be zero:
		set_line_width (context.cr, type_view_coordinate (zero));
		
		-- draw outer contour with outer border
		draw_contour (area, context, outer_border, YES, zero, height, drawn);

		-- the cutout area must clear out the outer area:
		set_operator (context.cr, CAIRO_OPERATOR_CLEAR);
		
		-- draw inner contour - the area to be taken out:
		draw_contour (area, context, inner_border, YES, zero, height, drawn);
		
		-- restore default compositing operator:
		set_operator (context.cr, CAIRO_OPERATOR_OVER);		
	end draw_contour_with_arbitrary_cutout;

	
	procedure draw_rectangle (
		area			: in type_rectangle;
		context			: in type_draw_context;
		position		: in type_point'class; -- the lower left corner
		--width			: in pac_shapes.pac_geometry_1.type_distance;
		width			: in type_float_internal_positive;
		--height			: in pac_shapes.pac_geometry_1.type_distance;
		height			: in type_float_internal_positive;
		--frame_height	: in pac_shapes.pac_geometry_1.type_distance;
		frame_height	: in type_float_internal_positive;
		extend_boundaries	: in boolean := false;
		boundaries_to_add	: in type_boundaries := boundaries_default) 
	is
		-- compute the boundaries (greatest/smallest x/y) of the given arc:
		boundaries : type_boundaries := (
			smallest_x	=> type_float_internal (get_x (position)),
			greatest_x	=> type_float_internal (get_x (position)) + width,
			smallest_y	=> type_float_internal (get_y (position)),
			greatest_y	=> type_float_internal (get_y (position)) + height,
			others		=> <>);

		-- compute the bounding box of the given arc
		bounding_box : type_rectangle := make_bounding_box (frame_height, boundaries);

	begin
		if extend_boundaries then
			add (boundaries, boundaries_to_add);
		end if;

		-- We draw the rectangle if:
		--  - no area given or
		--  - if the bounding box of the segment intersects the given area
		if (area = no_rectangle
			or else intersects (area, bounding_box)) 
		then
			-- We draw the four lines of the rectangle starting at
			-- the lower left corner. proceed counterclockwise:

			-- CS use rectangle instead if filling required.

			-- The ends of the lines are round:
			set_line_cap (context.cr, cairo_line_cap_round);

			
			-- LINE 1:
			
			-- start point
			move_to (
				context.cr,
				convert_x (get_x (position)),
				shift_y (get_y (position), frame_height));

			-- end point
			line_to (
				context.cr,
				convert_x (get_x (position) + type_distance (width)),
				shift_y (get_y (position), frame_height));

			-- LINE 2:
			
			-- start point
			move_to (
				context.cr,
				convert_x (get_x (position) + type_distance (width)),
				shift_y (get_y (position), frame_height));

			-- end point
			line_to (
				context.cr,
				convert_x (get_x (position) + type_distance (width)),
				shift_y (get_y (position) + type_distance (height), frame_height));

			-- LINE 3:
			
			-- start point
			move_to (
				context.cr,
				convert_x (get_x (position) + type_distance (width)),
				shift_y (get_y (position) + type_distance (height), frame_height));

			-- end point
			line_to (
				context.cr,
				convert_x (get_x (position)),
				shift_y (get_y (position) + type_distance (height), frame_height));

			-- LINE 4:
			
			-- start point
			move_to (
				context.cr,
				convert_x (get_x (position)),
				shift_y (get_y (position) + type_distance (height), frame_height));

			-- end point
			line_to (
				context.cr,
				convert_x (get_x (position)),
				shift_y (get_y (position), frame_height));

			stroke (context.cr);
			
		end if;
		
	end draw_rectangle;


-- TEXT
	function to_points (size : in pac_text.type_text_size)
		return gdouble is
	begin
		return gdouble (size) * conversion_factor_mm_to_pt;
	end to_points;

	
	procedure draw_origin (
		context		: in type_draw_context;
		position	: in type_view_point) is

		use cairo;
	begin
	-- The text origin (or anchor point) is drawn directly on the context,
	-- means the y-axis increases downwards.
	-- The origin is never rotated.
		
		set_line_width (context.cr, type_view_coordinate (origin_line_width));
		
		-- horizontal line from left to right
		move_to (
			context.cr,
			position.x - type_view_coordinate (origin_half_size),
			position.y
			);

		line_to (
			context.cr,
			position.x + type_view_coordinate (origin_half_size),
			position.y
			);

		-- vertical line downwards
		-- upper end of line
		move_to (
			context.cr,
			position.x,
			position.y - type_view_coordinate (origin_half_size) -- y increases downwards.
			);

		-- lower end of line
		line_to (
			context.cr,
			position.x,
			position.y + type_view_coordinate (origin_half_size)
			);

		stroke (context.cr);
	end draw_origin;

	
	function start_point (
		width		: in type_view_coordinate;
		height		: in type_view_coordinate;
		alignment	: in type_text_alignment;
		origin		: in type_view_point)
		return type_view_point is

		sp : type_view_point; -- to be returned
	begin
		case alignment.horizontal is
			when LEFT => 
				sp.x := origin.x;

			when CENTER =>
				sp.x := origin.x - width/2.0;

			when RIGHT =>
				sp.x := origin.x - width;
		end case;

		case alignment.vertical is
			when BOTTOM => 
				sp.y := origin.y;

			when CENTER =>
				sp.y := origin.y + height/2.0;

			when TOP =>
				sp.y := origin.y + height;
		end case;

		return sp;
	end start_point;

	
	procedure draw_text (
		context		: in type_draw_context;
		content		: in pac_text_content.bounded_string;
		size		: in pac_text.type_text_size;
		font		: in et_text.type_font;
		x,y			: in gdouble;
		origin		: in boolean;
		rotation	: in pac_geometry_1.type_rotation;
		alignment	: in type_text_alignment) 
	is
		-- Here we will store the extents of the given text:
		text_area : aliased cairo_text_extents;

		-- Convert the given content and store it in variable text:
		--use interfaces.c.strings;
		--text : constant interfaces.c.strings.chars_ptr := new_string (to_string (content));

		use gtkada.types;
		text : constant chars_ptr := new_string (to_string (content));

		-- The point where we will start drawing the text:
		sp : type_view_point;
		
	begin
		save (context.cr);

		if origin then
			draw_origin (context, (x, y));
		end if;

		select_font_face (
			context.cr, 
			family	=> to_string (font.family),
			slant	=> font.slant,
			weight	=> font.weight);
	
		set_font_size (context.cr, (to_points (size)));

		-- Get the extents of the text to be displayed and store it in text_area:
		text_extents (cr => context.cr, utf8 => text, extents => text_area'access);
		
		-- depending on alignment compute position to start drawing the text:
		sp := start_point (
				width		=> text_area.width,
				height		=> text_area.height,
				alignment	=> alignment,
				origin		=> (x, y));

		-- Rotate the text around the origin.
		-- In cairo all angles increase in clockwise direction.
		-- Since our angles increase in counterclockwise direction (mathematically)
		-- the angle must change the sign.		
		translate (context.cr, x, y);
		rotate (context.cr, gdouble (to_radians (- rotation)));
		translate (context.cr, -x, -y);
		
		-- draw the text. start at calculated start position
		move_to (context.cr, sp.x, sp.y);

		show_text (context.cr, to_string (content));
		restore (context.cr);
	end draw_text;

	
	function get_text_extents (
		context		: in type_draw_context;
		content		: in pac_text_content.bounded_string;
		size		: in pac_text.type_text_size;
		font		: in et_text.type_font)
		return cairo_text_extents 
	is
		result : aliased cairo_text_extents; -- to be returned

		--use interfaces.c.strings;
		--text : interfaces.c.strings.chars_ptr := new_string (to_string (content));

		use gtkada.types;
		text : constant chars_ptr := new_string (to_string (content));
	begin
		select_font_face (context.cr, to_string (font.family), font.slant, font.weight);
		set_font_size (context.cr, (to_points (size)));
		text_extents (cr => context.cr, utf8 => text, extents => result'access);
		return result;
	end get_text_extents;

	
	procedure draw_text (
		area		: in type_rectangle;
		context		: in type_draw_context;
		content		: in pac_text_content.bounded_string;
		size		: in pac_text.type_text_size;
		font		: in et_text.type_font;
		position	: in type_point; -- anchor point in the drawing, the origin
		origin		: in boolean;		
		rotation	: in pac_geometry_1.type_rotation;
		alignment	: in type_text_alignment;
		--height		: in pac_shapes.pac_geometry_1.type_distance)  -- the height of the drawing frame
		height		: in type_float_internal_positive)
	is
		text_area : cairo_text_extents;

		-- the bounding box of the given text
		bounding_box : type_rectangle;

		-- The point where we will start drawing the text:
		sp : type_view_point;

		-- The position of the origin:
		ox : constant type_view_coordinate := convert_x (get_x (position));
		oy : constant type_view_coordinate := shift_y (get_y (position), height);

	begin
		select_font_face (
			context.cr, 
			family	=> to_string (font.family),
			slant	=> font.slant,
			weight	=> font.weight);
		
		set_font_size (context.cr, (to_points (size)));

		text_area := get_text_extents (
			context		=> context,
			content		=> content,
			size		=> size,
			font		=> font);
		
-- 		put_line ("length " & gdouble'image (abs (text_area.width)));
-- 		put_line ("height " & gdouble'image (abs (text_area.height)));

		-- Depending on alignment compute position to start drawing the text.
		-- NOTE: The start point is the lower right corner of the text.
		sp := start_point (
				width		=> text_area.width,
				height		=> text_area.height,
				alignment	=> alignment,
				origin		=> (ox, oy)
				);

		-- Now we build the bounding box of the text. The bounding box
		-- is the text enclosing rectangle that exists in the model plane.
		-- In the model plane the y-axis increases downwards.
		-- The bounding box position is where it has its upper left corner.
		-- To keep things simple, we assume the largest possible bonding box
		-- for the text. This way the text will be inside the box regardless
		-- of alignment and rotation:
		bounding_box.x := type_float_internal (ox - (text_area.width));
		bounding_box.y := type_float_internal (oy - (text_area.width));
		bounding_box.width	:= type_float_internal (2.0 * text_area.width);
		bounding_box.height	:= type_float_internal (2.0 * text_area.width);
		
		-- We draw the text if:
		--  - no area given or
		--  - if the bounding box of the text intersects the given area
		if (area = no_rectangle
			or else intersects (area, bounding_box)) 
		then
			save (context.cr);
			
	-- CS test size 
	-- 			if not size_above_threshold (self, context.view) then
	-- 				return;
	-- 			end if;

			if origin then 
				draw_origin (context, (ox, oy));
			end if;

			-- In cairo all angles increase in clockwise direction.
			-- Since our angles increase in counterclockwise direction (mathematically)
			-- the angle must change the sign.		
			translate (context.cr, ox, oy);
			rotate (context.cr, gdouble (to_radians (- rotation)));
			translate (context.cr, -ox, -oy);
			
			-- draw the text. start at calculated start position
			move_to (context.cr, sp.x, sp.y);

			show_text (context.cr, to_string (content));

			restore (context.cr);
		end if;

	end draw_text;

	
	procedure draw_vector_text (
		area	: in type_rectangle;
		context	: in type_draw_context;
		text	: in type_vector_text;
		width	: in type_distance_positive;
		--height	: in pac_shapes.pac_geometry_1.type_distance)
		height	: in type_float_internal_positive)
	is
		bounding_box_text : constant type_rectangle := 
			make_bounding_box (height, get_boundaries (text));
		
		use pac_vector_text_lines;
		
		procedure query_line (c : in pac_vector_text_lines.cursor) is 

			-- compute the boundaries (greatest/smallest x/y) of the given line:
			b : type_boundaries := get_boundaries (element (c), width);

			-- compute the bounding box of the given line
			bounding_box : constant type_rectangle := 
				make_bounding_box (height, b);

		begin
			-- We draw the segment if:
			--  - no area given or
			--  - if the bounding box of the segment intersects the given area
			if (area = no_rectangle
				or else intersects (area, bounding_box)) 
			then
				-- CS test size 
				-- 			if not size_above_threshold (self, context.view) then
				-- 				return;
				-- 			end if;
				
				-- start point
				move_to (
					context.cr,
					convert_x (get_x (element (c).start_point)),
					shift_y (get_y (element (c).start_point), height)
					);

				-- end point
				line_to (
					context.cr,
					convert_x (get_x (element (c).end_point)),
					shift_y (get_y (element (c).end_point), height)
					);

			end if;
			
		end query_line;
		
	begin
		-- We draw the text if:
		--  - no area given or
		--  - if the bounding box of the text intersects the given area
		if (area = no_rectangle
			or else intersects (area, bounding_box_text)) 
		then
			
			-- The ends of the line are round:
			set_line_cap (context.cr, cairo_line_cap_round);
			
			-- set_line_join (context.cr, cairo_line_join_miter); -- CS
			iterate (text, query_line'access);

			stroke (context.cr);
		end if;
	end draw_vector_text;

	
end pac_draw;

end et_canvas_primitive_draw_ops;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

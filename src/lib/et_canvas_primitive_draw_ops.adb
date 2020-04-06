------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                          PRIMITIVE DRAW OPS                              --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2020 Mario Blunk, Blunk electronic          --
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
with Interfaces.C.Strings;

package body et_canvas_primitive_draw_ops is
	
package body pac_draw is

	-- This function converts a x-value from the drawing to a x-value in the view.
	function convert_x (x : in pac_shapes.geometry.type_distance) 
		return type_view_coordinate is 
	begin
		return type_view_coordinate (x);
	end;

	-- This function converts a y-value from the drawing to a y-value in the view.
	function shift_y (
		y		: in pac_shapes.geometry.type_distance;
		height	: in pac_shapes.geometry.type_distance)
		return type_view_coordinate is
	begin
		return type_view_coordinate (height - y);
	end;

	function shift_y (
		y		: in pac_shapes.geometry.type_distance;
		height	: in pac_shapes.geometry.type_distance)
		return pac_shapes.geometry.type_distance is
	begin
		return (height - y);
	end;

	function make_bounding_box (
		height		: in pac_shapes.geometry.type_distance;
		boundaries	: in type_boundaries)
		return type_rectangle is
	begin
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
		area			: in type_rectangle;
		context			: in type_draw_context;
		line			: in type_line'class;
		height			: in pac_shapes.geometry.type_distance)
	is
		-- compute the boundaries (greatest/smallest x/y) of the given line:
		boundaries : type_boundaries := pac_shapes.boundaries (line);

		-- compute the bounding box of the given line
		bounding_box : type_rectangle := make_bounding_box (height, boundaries);
	begin
		-- We draw the segment if:
		--  - no area given or
		--  - if the bounding box of the segment intersects the given area
		if (area = no_rectangle
			or else intersects (area, bounding_box)) 
		then
-- 			put_line (to_string (boundaries));
-- 			put_line (to_string (bounding_box));
	-- CS test size 
	-- 			if not size_above_threshold (self, context.view) then
	-- 				return;
	-- 			end if;
			
			-- start point
			move_to (
				context.cr,
				convert_x (line.start_point.x),
				shift_y (line.start_point.y, height)
				);

			-- end point
			line_to (
				context.cr,
				convert_x (line.end_point.x),
				shift_y (line.end_point.y, height)
				);

		end if;
	end draw_line;

	procedure draw_arc (
		area	: in type_rectangle;
		context	: in type_draw_context;
		arc		: in type_arc'class;
		height	: in pac_shapes.geometry.type_distance) is

		-- compute the boundaries (greatest/smallest x/y) of the given arc:
		boundaries : type_boundaries := pac_shapes.boundaries (arc);

		-- compute the bounding box of the given arc
		bounding_box : type_rectangle := make_bounding_box (height, boundaries);

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

			if arc.direction = CW then
				
				cairo.arc (
					context.cr,
					xc		=> convert_x (arc_temp.center.x),
					yc		=> shift_y (arc_temp.center.y, height),
					radius	=> type_view_coordinate (arc_temp.radius),
					angle1	=> type_view_coordinate (to_radians (arc_temp.angle_start)),
					angle2	=> type_view_coordinate (to_radians (arc_temp.angle_end))
					);

			else
				
				cairo.arc_negative (
					context.cr,
					xc		=> convert_x (arc_temp.center.x),
					yc		=> shift_y (arc_temp.center.y, height),
					radius	=> type_view_coordinate (arc_temp.radius),
					angle1	=> type_view_coordinate (to_radians (arc_temp.angle_start)),
					angle2	=> type_view_coordinate (to_radians (arc_temp.angle_end))
					);
			end if;

		end if;
	end draw_arc;

	procedure draw_circle (
		area	: in type_rectangle;
		context	: in type_draw_context;
		circle	: in type_circle'class;
		filled	: in type_filled;		
		height	: in pac_shapes.geometry.type_distance) is

		-- compute the boundaries (greatest/smallest x/y) of the given circle:
		boundaries : type_boundaries := pac_shapes.boundaries (circle);

		-- compute the bounding box of the given arc
		bounding_box : type_rectangle := make_bounding_box (height, boundaries);
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

			cairo.arc (
				context.cr,
				xc		=> convert_x (circle.center.x),
				yc		=> shift_y (circle.center.y, height),
				radius	=> type_view_coordinate (circle.radius),

				-- it must be a full circle starting at 0 degree and ending at 360 degree:
				angle1	=> 0.0,
				angle2	=> type_view_coordinate (2 * pi)				
				);

			case filled is
				when YES => fill_preserve (context.cr);
				when NO => null;
			end case;
			
		end if;
	end draw_circle;

	procedure draw_rectangle (
		area			: in type_rectangle;
		context			: in type_draw_context;
		position		: in type_point'class; -- the lower left corner
		width			: in pac_shapes.geometry.type_distance;
		height			: in pac_shapes.geometry.type_distance;
		frame_height	: in pac_shapes.geometry.type_distance;
		extend_boundaries	: in boolean := false;
		boundaries_to_add	: in type_boundaries := boundaries_default) is

		-- compute the boundaries (greatest/smallest x/y) of the given arc:
		boundaries : type_boundaries := (
			smallest_x	=> position.x,
			greatest_x	=> position.x + width,
			smallest_y	=> position.y,
			greatest_y	=> position.y + height,
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

			-- LINE 1:
			
			-- start point
			move_to (
				context.cr,
				convert_x (position.x),
				shift_y (position.y, frame_height));

			-- end point
			line_to (
				context.cr,
				convert_x (position.x + width),
				shift_y (position.y, frame_height));

			-- LINE 2:
			
			-- start point
			move_to (
				context.cr,
				convert_x (position.x + width),
				shift_y (position.y, frame_height));

			-- end point
			line_to (
				context.cr,
				convert_x (position.x + width),
				shift_y (position.y + height, frame_height));

			-- LINE 3:
			
			-- start point
			move_to (
				context.cr,
				convert_x (position.x + width),
				shift_y (position.y + height, frame_height));

			-- end point
			line_to (
				context.cr,
				convert_x (position.x),
				shift_y (position.y + height, frame_height));

			-- LINE 4:
			
			-- start point
			move_to (
				context.cr,
				convert_x (position.x),
				shift_y (position.y + height, frame_height));

			-- end point
			line_to (
				context.cr,
				convert_x (position.x),
				shift_y (position.y, frame_height));
			
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
		content		: in type_text_content.bounded_string;
		size		: in pac_text.type_text_size;
		font		: in et_text.type_font;
		x,y			: in gdouble;
		origin		: in boolean;
		rotation	: in pac_shapes.geometry.type_rotation;
		alignment	: in type_text_alignment) is
		
		-- Here we will store the extents of the given text:
		text_area : aliased cairo_text_extents;

		-- Convert the given content and store it in variable text:
		use interfaces.c.strings;
		text : constant interfaces.c.strings.chars_ptr := new_string (to_string (content));

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
		content		: in type_text_content.bounded_string;
		size		: in pac_text.type_text_size;
		font		: in et_text.type_font)
		return cairo_text_extents is

		result : aliased cairo_text_extents; -- to be returned

		use interfaces.c.strings;
		text : interfaces.c.strings.chars_ptr := new_string (to_string (content));
	begin
		select_font_face (context.cr, to_string (font.family), font.slant, font.weight);
		set_font_size (context.cr, (to_points (size)));
		text_extents (cr => context.cr, utf8 => text, extents => result'access);
		return result;
	end get_text_extents;
	
	procedure draw_text (
		area		: in type_rectangle;
		context		: in type_draw_context;
		content		: in type_text_content.bounded_string;
		size		: in pac_text.type_text_size;
		font		: in et_text.type_font;
		position	: in type_point; -- anchor point in the drawing, the origin
		origin		: in boolean;		
		rotation	: in type_rotation;
		alignment	: in type_text_alignment;
		height		: in pac_shapes.geometry.type_distance)  -- the height of the drawing frame
	is
		text_area : cairo_text_extents;

		-- the bounding box of the given text
		bounding_box : type_rectangle;

		-- The point where we will start drawing the text:
		sp : type_view_point;

		-- The position of the origin:
		ox : constant type_view_coordinate := convert_x (x (position));
		oy : constant type_view_coordinate := shift_y (y (position), height);

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
		bounding_box.x := type_distance (ox - (text_area.width));
		bounding_box.y := type_distance (oy - (text_area.width));
		bounding_box.width	:= type_distance (2.0 * text_area.width);
		bounding_box.height	:= type_distance (2.0 * text_area.width);
		
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

	
end pac_draw;

end et_canvas_primitive_draw_ops;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

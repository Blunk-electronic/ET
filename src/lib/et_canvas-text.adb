------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                             CANVAS TEXT                                  --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2025                                                --
-- Mario Blunk / Blunk electronic                                           --
-- Buchfinkenweg 3 / 99097 Erfurt / Germany                                 --
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


with gtkada.types;



package body et_canvas.text is


	function to_points (size : in pac_text.type_text_size)
		return type_logical_pixels
	is 
		conversion_factor_mm_to_pt : constant := 1.53; -- CS use exact factor
	begin
		return to_distance (size) * conversion_factor_mm_to_pt;
	end to_points;


	function to_cairo_angle (
		angle : in type_rotation)
		return glib.gdouble
	is 
		use pac_geometry_1;
		use glib;
	begin
		-- In cairo all angles increase in clockwise direction.
		-- Since our angles increase in counterclockwise direction (mathematically)
		-- the angle must change the sign.		
		return gdouble (to_radians (- type_angle (angle)));
	end to_cairo_angle;

	
	
	function get_text_extents (
		content		: in et_text.pac_text_content.bounded_string;
		size		: in pac_text.type_text_size;
		font		: in type_font)
		return cairo.cairo_text_extents 
	is
		use cairo;
		use et_text;
		
		result : aliased cairo_text_extents; -- to be returned

		--use interfaces.c.strings;
		--text : interfaces.c.strings.chars_ptr := new_string (to_string (content));

		use gtkada.types;
		text : constant gtkada.types.chars_ptr := new_string (to_string (content));
	begin
		select_font_face (context, to_string (font.family), font.slant, font.weight);
		set_font_size (context, to_gdouble (to_points (size)));
		text_extents (cr => context, utf8 => text, extents => result'access);
		return result;
	end get_text_extents;
	

	
	
	function to_area (
		extents : in cairo.cairo_text_extents)
		return type_area
	is
		a : type_area;
	begin
		a.width  := to_distance (to_lp (extents.width));
		a.height := to_distance (to_lp (extents.height));		
		return a;
	end to_area;

	




	
	function get_text_start_point (
		extents		: in cairo.cairo_text_extents;
		alignment	: in type_text_alignment;
		anchor		: in type_logical_pixels_vector;
		mode_v		: in type_align_mode_vertical;
		size		: in pac_text.type_text_size)
		return type_logical_pixels_vector
	is
		use et_text;
		
		sp : type_logical_pixels_vector; -- to be returned

		-- The x_bearing is the horizontal distance between the origin
		-- of the text and the leftmost part of the text.
		-- It causes a small gap between origin and text.
		-- It is positive if the text is entirely right of the origin:
		x_bearing : constant type_logical_pixels := to_lp (extents.x_bearing);

		-- The y_bearing is the vertical distance between the origin
		-- of the text and the topmost part of the text. It is usually
		-- negative because the topmost part is above the baseline
		-- (canvas y-coordinates decrease upward):
		y_bearing : constant type_logical_pixels := to_lp (extents.y_bearing);

		-- The area occupied by the text has a width and a height:
		width  : constant type_logical_pixels := to_lp (extents.width);
		height : constant type_logical_pixels := to_lp (extents.height);
		
	begin
		--put_line ("x_bearing " & to_string (x_bearing));
		--put_line ("y_bearing " & to_string (y_bearing));

		
		-- HORIZONTAL ALIGNMENT:
		case alignment.horizontal is
			when ALIGN_LEFT => 
				sp.x := anchor.x;

			when ALIGN_CENTER =>
				sp.x := anchor.x - width / 2.0;

			when ALIGN_RIGHT =>
				sp.x := anchor.x - width;
		end case;


		-- VERTICAL ALIGNMENT:
		case alignment.vertical is
			when ALIGN_BOTTOM =>
				case mode_v is
					when MODE_ALIGN_BY_USED_SPACE =>
						sp.y := anchor.y - y_bearing - height;

					when MODE_ALIGN_RELATIVE_TO_BASELINE =>
						sp.y := anchor.y;
				end case;

				
			when ALIGN_CENTER =>
				case mode_v is
					when MODE_ALIGN_BY_USED_SPACE =>
						sp.y := anchor.y - y_bearing - height / 2.0;

					when MODE_ALIGN_RELATIVE_TO_BASELINE =>
						sp.y := anchor.y + to_distance (size) / 2.0;
				end case;

				
			when ALIGN_TOP =>
				case mode_v is
					when MODE_ALIGN_BY_USED_SPACE =>
						sp.y := anchor.y - y_bearing;

					when MODE_ALIGN_RELATIVE_TO_BASELINE =>
						sp.y := anchor.y + to_distance (size);
				end case;
		end case;


		-- Shift the start point to the left by the x_bearing:
		sp.x := sp.x - x_bearing;

		return sp;
	end get_text_start_point;





	procedure draw_text (
		content		: in et_text.pac_text_content.bounded_string;
		size		: in pac_text.type_text_size;
		font		: in type_font;
		anchor		: in type_vector_model;
		origin		: in boolean;		
		rotation	: in type_rotation;
		alignment	: in type_text_alignment)
	is
		use glib;
		use cairo;
		use et_text;
		use pac_text;

		-- The extents of the text on the canvas:
		text_area : aliased cairo_text_extents;
		
		use gtkada.types;
		text : constant gtkada.types.chars_ptr := new_string (to_string (content));
		
		-- The bounding-box of the given text (in the model domain):
		b : type_area;
  
		-- The diagonal of the bounding-box of the text:
		d : type_distance_positive;
		
		-- The start point where we will start drawing the text
		-- on the canvas:
		sp : type_logical_pixels_vector;
		
		-- The given anchor point of the text is in the model domain.
		-- Convert it to a canvas point (according to current zoom-factor): 
		anchor_canvas : constant type_logical_pixels_vector := 
			real_to_canvas (anchor, S);

		-- This flag indicates that rotation of the text is required.
		-- Using this flag some computing time can be saved. Mostly
		-- there is no rotation required. For this reason its
		-- default is false:
		rotation_required : boolean := false;
		
	begin
		-- Draw the origin (or the anchor point) of the 
		-- text if requested by the caller:
		if origin then
			-- The origin is never rotated. For this reason
			-- an angle of 0 degrees is passed here:
			draw_origin ((anchor, 0.0));
		end if;
		
		
		-- Set the font:
		select_font_face (context, to_string (font.family), font.slant, font.weight);

		-- Set the size:
		set_font_size (context, to_gdouble (to_points (size)));

		-- Set the text extents:
		text_extents (cr => context, utf8 => text, extents => text_area'access);
		
		-- put_line ("length " & gdouble'image (abs (text_area.width)));
		-- put_line ("height " & gdouble'image (abs (text_area.height)));

		-- Depending on alignment, compute the start position
		-- of the text:
		sp := get_text_start_point (
				extents		=> text_area,
				alignment	=> alignment,
				anchor		=> anchor_canvas,
				mode_v		=> MODE_ALIGN_RELATIVE_TO_BASELINE,
				size		=> size);
		
		-- Build a bounding-box of the text using the 
		-- canvas area (in logical pixels) occupied by the text:
		b.width  := to_distance (to_lp (text_area.width));
		b.height := to_distance (to_lp (text_area.height));

		-- put_line ("b " & to_string (b));
		
		-- Since the text can be rotated and aligned in various
		-- ways, we extend the bounding-box so that it encloses the
		-- text in any case. Because we need a fast and coarse solution,
		-- we assume a circular area that covers all kinds and
		-- combinations of rotation and alignment.

		-- Get the diagnoal of the bounding-box. This forms the widest
		-- possible range around the anchor point:
		d := get_diagonal (b);

		-- The bounding-box lower-left corner is then:
		b.position.x := anchor.x - d;
		b.position.y := anchor.y - d;

		-- Extend the bounding-box so that it encloses the circular
		-- area around the anchor point:
		b.width  := 2.0 * d;
		b.height := 2.0 * d;

		-- put_line ("b " & to_string (b));
		
		-- Do the area check. If the bounding-box of the text
		-- is inside the visible area then draw the text. Otherwise
		-- nothing will be drawn:
		if areas_overlap (visible_area, b) and then

			-- Do the size check. If the bounding-box is greater
			-- (either in width or heigth) than the visiblity threshold
			-- then draw the text. Otherwise nothing will be drawn:
			above_visibility_threshold (b) then

			-- Use this debug message to test the bounding-box
			-- of the text:
			-- put_line (to_string (content));

			-- Figure out whether rotation is required:
			if rotation /= 0.0 then
				rotation_required := true;
			end if;
			

			-- If rotation is required, then 
			-- the current context must
			-- be saved, translated by the anchor coordinates, rotated,
			-- moved back by the anchor coordinates and finally restored.
			-- This consumes time and can be skipped if no rotation
			-- is required:

			if rotation_required then
				save (context);			
				translate (context, 
					to_gdouble (anchor_canvas.x), to_gdouble (anchor_canvas.y));
				
				rotate (context, to_cairo_angle (rotation));
				
				translate (context, 
					- to_gdouble (anchor_canvas.x), - to_gdouble (anchor_canvas.y));
			end if;

			
			-- draw the text. start at calculated start position
			move_to (context, to_gdouble (sp.x), to_gdouble (sp.y));
			show_text (context, to_string (content));

			
			if rotation_required then
				restore (context);
			end if;
		end if;

	end draw_text;






	
	procedure draw_vector_text (
		text	: in pac_text.type_text_fab_with_content'class;
		pos		: in pac_geometry.type_position := origin_zero_rotation;
		mirror	: in type_mirror := MIRROR_NO)
	is
		use pac_text;
		use pac_character_lines;

		-- Drawing a vector-text is just a matter of
		-- drawing many lines. So we iterate the given
		-- lines of the text and draw them one by one.
		
		procedure query_line (
			c : in pac_character_lines.cursor)
		is
			-- The line of a character must now be
			-- converted to a type_line:
			lf : type_character_line renames element (c);
			lc : type_line;
		begin
			lc.A := to_point (lf.A);
			lc.B := to_point (lf.B);

			draw_line (
				line	=> lc,
				width	=> 0.0); -- don't care

		end query_line;
		

		vectors	: pac_text.type_vector_text;

		pos_final : type_position := text.position;
		
	begin
		-- Add the text position and the position of the
		-- parent object:
		add (position => pos_final, offset => pos, mirror => mirror);

		-- If the text is being moved, then pos_final will
		-- be overwritten by the tool position:
		if is_moving (text) then
			pos_final.place := get_object_tool_position;
		end if;

		-- Draw the origin of the text:
		draw_origin ((pos_final.place, zero_rotation));
		-- CS draw the origin rotated by 45 degrees 
		-- if the text is locked ?

		if not is_empty (text.content) then
			
			vectors := vectorize_text (
				content		=> text.content,
				size		=> text.size,
				rotation	=> get_rotation (pos_final),
				position	=> pos_final.place,
				mirror		=> mirror,
				line_width	=> text.line_width,
				alignment	=> text.alignment); -- right, bottom

			
			-- set_line_join (context.cr, cairo_line_join_miter); -- CS

			-- The linewidth applies to all character lines:
			set_linewidth (text.line_width);
			
			iterate (vectors, query_line'access);

			-- Do a final stroke:
			stroke;
		end if;
	end draw_vector_text;



	
end et_canvas.text;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16


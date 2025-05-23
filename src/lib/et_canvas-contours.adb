------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           CANVAS CONTOURS                                --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2024                                                --
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

-- with ada.strings.bounded;
-- with ada.strings;
-- with ada.strings.fixed;
-- 

with glib;

with ada.numerics;


package body et_canvas.contours is

	use glib;
	use cairo;

	
	procedure draw_contour (
		contour	: in type_contour'class;
		pos 	: in type_position := origin_zero_rotation;
		offset	: in type_position := origin_zero_rotation;
		style	: in type_line_style := CONTINUOUS;
		filled	: in type_filled;
		width	: in type_distance_positive;
		mirror	: in type_mirror := MIRROR_NO)
		-- CS fill style
	is
		-- The line style, or the dash pattern, will be calculated 
		-- according to the current scale:
		dash_on, dash_off	: gdouble;
		dash_pattern		: dash_array (1 .. 2);

		offset_tmp : type_vector_model := offset.place;
		-- CS currently the rotation of the contour about itself
		-- is ignored.

		-- This is the final position of the contour
		-- due to the position of the parent object:
		pos_end : type_position := pos;
		
		use pac_segments;


		-- This flag indicates how to draw the
		-- contour.
		-- If the contour is closed, then it is allowed
		-- to fill it. Otherwise filling is not allowed.
		-- If the contour is closed, then the segments are
		-- drawn as a polyline (except the first segment).
		-- If the contour is not closed, then each segment
		-- will be drawn as an independend object:
		contour_is_closed : constant boolean := not is_open (contour);

		-- In case the contour is closed, then this
		-- flag is set when the first segment is drawn:
		start_point_set : boolean := false;

		
		-- This procedure draws a segment of the given contour:
		procedure query_segment (
			c : in pac_segments.cursor) 
		is 
			-- Take a copy of the given segment:
			segment : type_segment := element (c);

			-- In case the segment is being moved
			-- by the operator, then we store the tool
			-- position here:
			pointer : type_vector_model;
		begin			
			case segment.shape is				
				when LINE =>
					--put_line ("draw_segment (line)");
					-- put_line (" line" & to_string (type_line (segment.segment_line)));

					if contour_is_closed then

						-- Draw a polyline:
						if not start_point_set then
							start_point_set := true;
						
							draw_line (
								line	=> segment.segment_line,
								pos		=> pos_end,		  
								width	=> zero,  -- don't care. see specs of draw_line.
								mirror	=> mirror,
								style	=> style);
						else
							draw_line (
								line		=> segment.segment_line,
								pos			=> pos_end,		  
								width		=> zero,  -- don't care. see specs of draw_line.
								mirror		=> mirror,
								style		=> style,
								polyline	=> true);

						end if;

					else
						-- If the contour is not closed, then it can not be filled.
						-- Each line is drawn as a single independend segment:
						draw_line (
							line		=> segment.segment_line,
							pos			=> pos_end,		  
							width		=> zero,  -- don't care. see specs of draw_line.
							mirror		=> mirror,
							style		=> style,
							polyline	=> false);
					end if;

					
				when ARC =>

					-- CS ?
					draw_arc (
						arc		=> segment.segment_arc,
						pos		=> pos_end,		 
						width	=> zero,
						mirror	=> mirror,
						style	=> style);

			end case;
		end query_segment;

		
	begin -- draw_contour
		-- put_line ("draw_contour");
		-- put_line (" pos" & to_string (pos));
		-- put_line (" off" & to_string (offset));

		-- Rotate by rotation of parent object:
		rotate_by (offset_tmp, get_rotation (pos));
		
		case mirror is
			when MIRROR_ALONG_X_AXIS =>
				mirror_point (offset_tmp, MIRROR_ALONG_X_AXIS);

			when MIRROR_ALONG_Y_AXIS =>
				mirror_point (offset_tmp, MIRROR_ALONG_X_AXIS);

			when others => null;
		end case;
		
		add (pos_end.place, offset_tmp);



		-- The calls of primitive draw operations in this 
		-- procedure are without explicit strokes. For this 
		-- reason the linewidth must be set first:
		if width > zero then
			set_line_width (context, 
				to_gdouble_positive (to_distance (width)));
  
		else
			-- If linewidth is zero then a mimimum
			-- must be ensured:
			set_line_width (context, to_gdouble (minimal_linewidth));
		end if;


		-- Draw the contour.
		-- If it is just a circle then draw a circle.
		-- If the contour is a collection of segments (arc and lines),
		-- then iterate through the segments:
		if contour.contour.circular then
			
			-- CS: The intersection between circle and other segments
			-- is still visible as a very thin line.

			-- Draw the single circle that forms the contour:
			draw_circle (
				circle		=> contour.contour.circle,
				pos			=> pos_end,			
				filled		=> filled,
				mirror		=> mirror,
				width		=> zero,   
				-- The linewidth is ignored (see specs of draw_circle)
				-- because no stroke will be carried out.
				-- The linewidth has been set already and
				-- there is a final stroke in this procedure.
				style		=> style);
			
		else
			new_sub_path (context); -- required to suppress an initial line
			contour.contour.segments.iterate (query_segment'access);
		end if;

		
		-- Fill the contour if requested by the caller
		-- AND if it is closed:
		if filled = YES and contour_is_closed then
			cairo.fill (context);
		end if;


		-- Set the line style as requested by the caller:
		-- NOTE: This is a makeshift. The handling of the style
		-- should be done by draw_line, draw_arc and draw_circle instead.
		case style is
			when CONTINUOUS => null;
			
			when DASHED =>
				dash_on  := 1.5 * to_gdouble (to_distance (width));
				dash_off := 1.5 * to_gdouble (to_distance (width));
				
				dash_pattern (1) := dash_on;
				dash_pattern (2) := dash_off;
				set_dash (context, dash_pattern, 0.0);
		end case;


		-- Do the final stroke on everything that
		-- has been drawn:
		stroke (context);


		-- Disable line dashes:
		if style /= CONTINUOUS then
			set_dash (context, no_dashes, 0.0);
		end if;
		
	end draw_contour;




	procedure draw_contour_with_circular_cutout (
		outer_border	: in type_contour'class;
		inner_border	: in type_circle;
		pos 			: in type_position := origin_zero_rotation; -- includes x,y, rotation
		offset			: in type_position := origin_zero_rotation;
		mirror			: in type_mirror := MIRROR_NO)
	is begin
		-- draw outer contour:
		draw_contour (outer_border, pos, offset, CONTINUOUS, YES, zero, mirror);
		-- CS dash pattern ? currently set to CONTINUOUS

		-- the cutout area must clear out the outer area:
		set_operator (context, CAIRO_OPERATOR_CLEAR);
		
		-- draw inner area to be taken out:
		draw_circle (inner_border, pos, filled => YES, 
					 width => zero, mirror => mirror, do_stroke => true);

		-- restore default compositing operator:
		set_operator (context, CAIRO_OPERATOR_OVER);		
	end draw_contour_with_circular_cutout;



	procedure draw_contour_with_arbitrary_cutout (
		outer_border	: in type_contour'class;
		inner_border	: in type_contour'class;
		pos 			: in type_position := origin_zero_rotation; -- includes x,y, rotation
		offset			: in type_position := origin_zero_rotation;
		mirror			: in type_mirror := MIRROR_NO)
	is begin
		-- draw outer contour:
		draw_contour (outer_border, pos, offset, CONTINUOUS, YES, zero, mirror);
		-- CS dash pattern ? currently set to CONTINUOUS
		
		-- the cutout area must clear out the outer area:
		set_operator (context, CAIRO_OPERATOR_CLEAR);
		
		-- draw inner contour - the area to be taken out:
		draw_contour (inner_border, pos, offset, CONTINUOUS, YES, zero, mirror);
		-- CS dash pattern ? currently set to CONTINUOUS
		
		-- restore default compositing operator:
		set_operator (context, CAIRO_OPERATOR_OVER);		
	end draw_contour_with_arbitrary_cutout;

	
	
end et_canvas.contours;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

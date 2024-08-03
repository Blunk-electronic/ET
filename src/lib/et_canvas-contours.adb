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
		style	: in type_line_style := CONTINUOUS;
		filled	: in type_filled;
		width	: in type_distance_positive)
		-- CS fill style
	is
		-- The line style, or the dash pattern, will be calculated 
		-- according to the current scale:
		dash_on, dash_off	: gdouble;
		dash_pattern		: dash_array (1 .. 2);

		
		use pac_segments;
		
		procedure query_segment (
			c : in pac_segments.cursor) 
		is 
			segment : type_segment renames element (c);
		begin
			case segment.shape is
				
				when LINE =>
					draw_line (
						line	=> type_line (segment.segment_line),
						width	=> zero);  -- don't care. see specs of draw_line.

					
				when ARC =>
					draw_arc (
						arc		=> type_arc (segment.segment_arc),
						width	=> zero);   -- don't care. see specs of draw_line.

			end case;
		end query_segment;

		
	begin -- draw_contour

		new_sub_path (context); -- required to suppress an initial line

		if contour.contour.circular then
			-- Draw the single circle that forms the contour:
			
			-- CS: The intersection between circle and other segments
			-- is still visible as a very thin line.

			draw_circle (
				circle		=> contour.contour.circle,
				filled		=> filled,
				width		=> zero);   -- don't care. see specs of draw_line.
			
		else
			contour.contour.segments.iterate (query_segment'access);
		end if;


		if filled = YES then
			set_linewidth (zero);
			cairo.fill (context);
		else
			set_linewidth (width);
			
			case style is
				when CONTINUOUS => null;
				
				when DASHED =>
					--dash_on := 0.2 + 1.0 / scale;
					--dash_off := 0.1 + 1.0 / scale;
					dash_on := 20.0 / gdouble (S);
					dash_off := 15.0 / gdouble (S);

					dash_pattern (1) := dash_on;
					dash_pattern (2) := dash_off;
					set_dash (context, dash_pattern, 0.0);
			end case;
		end if;

		stroke (context);


		-- Disable line dashes:
		if filled = NO then
			set_dash (context, no_dashes, 0.0);
		end if;
		
	end draw_contour;




	procedure draw_contour_with_circular_cutout (
		outer_border	: in type_contour'class;
		inner_border	: in type_circle)
	is begin
		-- draw outer contour:
		draw_contour (outer_border, CONTINUOUS, YES, zero);
		-- CS dash pattern ? currently set to CONTINUOUS

		-- the cutout area must clear out the outer area:
		set_operator (context, CAIRO_OPERATOR_CLEAR);
		
		-- draw inner area to be taken out:
		draw_circle (inner_border, filled => YES, 
					 width => zero, do_stroke => true);

		-- restore default compositing operator:
		set_operator (context, CAIRO_OPERATOR_OVER);		
	end draw_contour_with_circular_cutout;



	procedure draw_contour_with_arbitrary_cutout (
		outer_border	: in type_contour'class;
		inner_border	: in type_contour'class)
	is begin
		-- draw outer contour:
		draw_contour (outer_border, CONTINUOUS, YES, zero);
		-- CS dash pattern ? currently set to CONTINUOUS
		
		-- the cutout area must clear out the outer area:
		set_operator (context, CAIRO_OPERATOR_CLEAR);
		
		-- draw inner contour - the area to be taken out:
		draw_contour (inner_border, CONTINUOUS, YES, zero);
		-- CS dash pattern ? currently set to CONTINUOUS
		
		-- restore default compositing operator:
		set_operator (context, CAIRO_OPERATOR_OVER);		
	end draw_contour_with_arbitrary_cutout;

	
	
end et_canvas.contours;

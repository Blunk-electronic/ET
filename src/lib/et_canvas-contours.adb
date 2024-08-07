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
		pos 	: in type_position := origin_zero_rotation; -- includes x,y, rotation
		offset	: in type_vector_model := origin;
		style	: in type_line_style := CONTINUOUS;
		filled	: in type_filled;
		width	: in type_distance_positive;
		mirror	: in type_mirror_style := mirror_style_default)
		-- CS fill style
	is
		-- The line style, or the dash pattern, will be calculated 
		-- according to the current scale:
		dash_on, dash_off	: gdouble;
		dash_pattern		: dash_array (1 .. 2);

		offset_tmp : type_vector_model := offset;
		pos_end : type_position := pos;
		
		use pac_segments;

		
		procedure query_segment (
			c : in pac_segments.cursor) 
		is 
			segment : type_segment renames element (c);
		begin
			case segment.shape is
				
				when LINE =>
					put_line ("draw_segment (line)");
					put_line (" line" & to_string (type_line (segment.segment_line)));
					
					draw_line (
						line	=> type_line (segment.segment_line),
						pos		=> pos_end,		  
						--width	=> zero);  -- don't care. see specs of draw_line.
						width 	=> 0.1,
						mirror	=> mirror,
						do_stroke => true);
						-- CS mirror

					
				when ARC =>
					draw_arc (
						arc		=> type_arc (segment.segment_arc),
						width	=> zero);   -- don't care. see specs of draw_line.

			end case;
		end query_segment;

		
	begin -- draw_contour
		put_line ("draw_contour");
		put_line (" pos" & to_string (pos));


		-- Rotate by rotation of parent object:
		rotate_by (offset_tmp, get_rotation (pos));
		
		case mirror is
			when MIRROR_X =>
				pac_geometry.mirror (offset_tmp, X);

			when MIRROR_Y =>
				pac_geometry.mirror (offset_tmp, Y);

			when others => null;
		end case;
		
		add (pos_end.place, offset_tmp);

		
		
		-- new_sub_path (context); -- required to suppress an initial line

		
		-- if width > zero then
		-- 	set_line_width (context, 
		-- 		to_gdouble_positive (to_distance (width)));
  -- 
		-- else
		-- 	-- If linewidth is zero then a mimimum
		-- 	-- must be ensured:
		-- 	set_line_width (context, to_gdouble (minimal_linewidth));
		-- end if;

		
		if contour.contour.circular then
			-- Draw the single circle that forms the contour:
			
			-- CS: The intersection between circle and other segments
			-- is still visible as a very thin line.

			draw_circle (
				circle		=> contour.contour.circle,
				pos			=> pos,			
				filled		=> filled,
				mirror		=> mirror,
				width		=> zero);   
				-- The linewidth is don't care (see specs of draw_line)
				-- because the linewidth is has been set initially and
				-- there is a final stroke in this procedure.
		else
			contour.contour.segments.iterate (query_segment'access);
		end if;

		-- cairo.fill (context);
		
		
		-- if filled = YES then
		-- 	cairo.fill (context);
		-- end if;

		
-- 		case style is
-- 			when CONTINUOUS => null;
-- 			
-- 			when DASHED =>
-- 				--dash_on := 0.2 + 1.0 / scale;
-- 				--dash_off := 0.1 + 1.0 / scale;
-- 				dash_on := 20.0 / gdouble (S);
-- 				dash_off := 15.0 / gdouble (S);
-- 
-- 				dash_pattern (1) := dash_on;
-- 				dash_pattern (2) := dash_off;
-- 				set_dash (context, dash_pattern, 0.0);
-- 		end case;

		-- stroke (context);


		-- Disable line dashes:
		-- set_dash (context, no_dashes, 0.0);

		
	end draw_contour;




	procedure draw_contour_with_circular_cutout (
		outer_border	: in type_contour'class;
		inner_border	: in type_circle;
		pos 			: in type_position := origin_zero_rotation; -- includes x,y, rotation
		offset			: in type_vector_model := origin;
		mirror			: in type_mirror_style := mirror_style_default)
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
		offset			: in type_vector_model := origin;
		mirror			: in type_mirror_style := mirror_style_default)
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

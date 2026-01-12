------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                             BOARD WRITE                                  --
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

--   For correct displaying set tab with in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with ada.characters.handling;	use ada.characters.handling;
with ada.strings; 				use ada.strings;
with ada.text_io;				use ada.text_io;

with et_coordinates_formatting;		use et_coordinates_formatting;
with et_keywords;					use et_keywords;
with et_directions;					use et_directions;


package body et_board_write is


	
	procedure write_line (line : in type_line'class) is begin
		write (keyword => keyword_start, parameters => to_string (get_A (line), FORMAT_2));
		write (keyword => keyword_end  , parameters => to_string (get_B (line), FORMAT_2));
	end write_line;

	
	procedure write_arc (arc : in type_arc'class) is begin
		write (keyword => keyword_center, parameters => to_string (get_center (arc), FORMAT_2));
		write (keyword => keyword_start, parameters => to_string (get_A (arc), FORMAT_2));
		write (keyword => keyword_end, parameters => to_string (get_B (arc), FORMAT_2));
		write (keyword => keyword_direction, parameters => to_string (get_direction (arc)));
	end write_arc;

	
	procedure write_circle (circle : in type_circle'class) is begin
		write (keyword => keyword_center, parameters => to_string (get_center (circle), FORMAT_2));
		write (keyword => keyword_radius, parameters => to_string (get_radius (circle)));
	end write_circle;

		
	
	
	
	procedure write_polygon_segments (
		polygon : in type_contour'class)
	is
		use pac_segments;
		
		procedure query_segment (c : in pac_segments.cursor) is begin
			case element (c).shape is
				
				when LINE =>
					line_begin;
					write_line (element (c).segment_line);
					line_end;

				when ARC =>
					arc_begin;
					write_arc (element (c).segment_arc);
					arc_end;

			end case;
		end query_segment;		

		contours : type_segments := get_segments (polygon);
		
	begin				
		if contours.circular then

			circle_begin;
			write_circle (contours.circle);
			circle_end;

		else
			contours.segments.iterate (query_segment'access);
		end if;
		
	end write_polygon_segments;
	


	
	

	
	
	procedure fill_zone_begin is begin section_mark (section_zone, HEADER); end;
	procedure fill_zone_end   is begin section_mark (section_zone, FOOTER); end;
	procedure cutout_zone_begin is begin section_mark (section_cutout_zone, HEADER); end;
	procedure cutout_zone_end   is begin section_mark (section_cutout_zone, FOOTER); end;
	procedure contours_begin is begin section_mark (section_contours, HEADER); end;
	procedure contours_end   is begin section_mark (section_contours, FOOTER); end;

	
	
end et_board_write;

------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      GEOMETRY 2 / FILE READ / WRITE                      --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2026                                                --
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

--   For correct displaying set tab width in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with ada.text_io;					use ada.text_io;
with ada.strings;					use ada.strings;
with ada.strings.fixed;				use ada.strings.fixed;
-- with ada.strings.unbounded;
-- with ada.characters.latin_1;
-- with ada.characters.handling;	use ada.characters.handling;
-- 
with et_coordinates_formatting;		use et_coordinates_formatting;
with et_keywords;					use et_keywords;
with et_file_write;					use et_file_write;
with et_package_sections;			use et_package_sections;
with et_directions;					use et_directions;

	
package body et_geometry_2_file_rw is


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
					section_mark (section_line, HEADER);
					write_line (element (c).segment_line);
					section_mark (section_line, FOOTER);

				when ARC =>
					section_mark (section_arc, HEADER);
					write_arc (element (c).segment_arc);
					section_mark (section_arc, FOOTER);

			end case;
		end query_segment;		

		contours : type_segments := get_segments (polygon);
		
	begin				
		if contours.circular then

			section_mark (section_circle, HEADER);
			write_circle (contours.circle);
			section_mark (section_circle, FOOTER);

		else
			contours.segments.iterate (query_segment'access);
		end if;
		
	end write_polygon_segments;

	
	
end et_geometry_2_file_rw;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

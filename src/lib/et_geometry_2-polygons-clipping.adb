------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      GEOMETRY 2 / POLYGONS / CLIPPING                    --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2022 Mario Blunk, Blunk electronic          --
--                                                                          --
--    This program is free software: you can redistribute it and/or modify  --
--    it under the terms of the GNU General Public License as published by  --
--    the Free Software Foundation, either version 3 of the License, or     --
--    (at your option) any later version.                                   --
--                                                                          --
--    This program is distributed in the hope that it will be useful,       --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of        --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         --
--    GNU General Public License for more details.                          --
--                                                                          --
--    You should have received a copy of the GNU General Public License     --
--    along with this program.  If not, see <http://www.gnu.org/licenses/>. --
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


with ada.strings;				use ada.strings;
with ada.strings.fixed;			use ada.strings.fixed;
with ada.strings.unbounded;
with ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;

with et_exceptions;				use et_exceptions;


package body et_geometry_2.polygons.clipping is

	use pac_polygon_segments;


	function to_string (intersection : in type_intersection)
		return string
	is begin
		return to_string (intersection.point) 
			& " " & type_direction'image (intersection.direction);
	end to_string;

	

	function clip (
		polygon_A	: in type_polygon'class;
		polygon_B	: in type_polygon'class)
		return pac_clipped.list
	is
		result : pac_clipped.list;

		intersections : pac_intersections.list;
		
		
		procedure query_A_segment (a : in pac_polygon_segments.cursor) is

			procedure query_B_segment (b : in pac_polygon_segments.cursor) is
				I2L : type_intersection_of_two_lines := get_intersection (
				   element (a).segment_line, element (b).segment_line);

				p : type_point;
				Q : type_inside_polygon_query_result;
				IAB : type_intersection;
			begin
				if I2L.status = EXISTS then
					p := to_point (I2L.intersection.vector);

					Q := in_polygon_status (polygon_A, element (b).segment_line.start_point);
					
					if Q.status = INSIDE then
						IAB := (P, EXITING);
					else
						IAB := (P, ENTERING);
					end if;

					intersections.append (IAB);
					put_line (to_string (IAB));
				end if;
			end query_B_segment;
			
		begin
			polygon_B.contours.segments.iterate (query_B_segment'access);
		end query_A_segment;

		
	begin


		polygon_A.contours.segments.iterate (query_A_segment'access);
		return result;
	end clip;

	
end et_geometry_2.polygons.clipping;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

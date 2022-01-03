------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                     GEOMETRY 2 / POLYGONS / OFFSETTING                   --
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



package body et_geometry_2.polygons.offsetting is

	use pac_polygon_segments;



	procedure offset_polygon (
		polygon		: in out type_polygon_base'class;
		offset		: in type_distance) 
	is

		function offset_line (
			line : in type_line)
			return type_line_vector
		is
			line_new : type_line := line;
			center : type_point := get_center (line);
			line_direction : type_rotation := get_direction (line);
			dir_scratch : type_rotation;			
			test_point : type_point;
			tp_status : type_inside_polygon_query_result;

		begin
			dir_scratch := add (line_direction, +90.0);
			test_point := type_point (move (center, dir_scratch, type_distance'small));
			--put_line ("tp " & to_string (test_point));
			tp_status := in_polygon_status (polygon, test_point);

			if tp_status.status = INSIDE then
				--put_line ("inside");
				move_by (line_new, add (line_direction, -90.0), offset);
			else
				--put_line ("outside");
				move_by (line_new, add (line_direction, +90.0), offset);
			end if;

			return to_line_vector (line_new);
		end offset_line;
	

		package pac_line_vectors is new doubly_linked_lists (type_line_vector);
		use pac_line_vectors;
		line_vectors : pac_line_vectors.list;
		
		
		procedure do_segment (c : in pac_polygon_segments.cursor) is
			lv_tmp : type_line_vector;			
		begin
			case element (c).shape is
				
				when LINE =>
					lv_tmp := offset_line (element (c).segment_line);
					--put_line ("lv " & to_string (lv_tmp));
					line_vectors.append (lv_tmp);

				when ARC =>
					null; -- CS

			end case;
		end do_segment;


		polygon_segments_new : type_polygon_segments := (circular => false, others => <>);
		
		INIT, LS, LE : type_point;
		I : type_intersection_of_two_lines := (status => EXISTS, others => <>);

		
		procedure query_line (cp : in pac_line_vectors.cursor) is
			-- cp is the primary cursor that points to the current line.
			
			-- The secondary cursor that points to the line that is
			-- before the candidate line:
			cs : pac_line_vectors.cursor;

		begin
			--put_line ("lv " & to_string (element (cp)));

			if cp = line_vectors.first then
				cs := line_vectors.last;
				I := get_intersection (element (cp), element (cs));

				LS := to_point (I.intersection.vector);
				INIT := LS;

				
			else
				cs := previous (cp);
				I := get_intersection (element (cp), element (cs));

				LE := to_point (I.intersection.vector);

				-- line complete. append to new segments:
				polygon_segments_new.segments.append (
					(shape => LINE, segment_line => (LS, LE)));


				if cp = line_vectors.last then

					polygon_segments_new.segments.append (
						(shape => LINE, segment_line => (LE, INIT)));

				end if;

				
				-- The end point of this line will be the 
				-- start point of the next line (irrelevant for last line vector):
				LS := LE;
			end if;
		end query_line;

		
	begin -- offset_polygon

		if polygon.contours.circular then

			-- scale the single circle that forms the polygon:
			-- CS change radius of  polygon.contours.circle
			null;
		else
			polygon.contours.segments.iterate (do_segment'access);
		end if;

		-- Compute the intersections of the line_vectors.
		-- The intersections become the start and end points
		-- of the new line-segments:
		line_vectors.iterate (query_line'access);

		polygon.contours := polygon_segments_new;

		if not is_closed (polygon).closed then
			raise constraint_error with "Polygon NOT closed !";
		end if;
	end offset_polygon;

	
end et_geometry_2.polygons.offsetting;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

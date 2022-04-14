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


with et_exceptions;				use et_exceptions;



package body et_geometry_2.polygons.offsetting is


	procedure offset_polygon (
		polygon		: in out type_polygon;
		offset		: in type_distance) 
	is
		-- Mode tells whether we are shrinking, expanding
		-- or whether there is nothing to do:
		mode : constant type_mode := to_mode (offset);	
		
		
		function offset_line (
			line : in type_line)
			return type_line_vector
		is
			line_new : type_line := line;
			center : type_vector := get_center (line);
			
			line_direction : type_rotation := get_direction (line);
			dir_scratch : type_rotation;			
			test_point : type_point;

			-- These two procedures move the line_new to the right or
			-- to the left (seen relative to the line direction):
			procedure move_right is begin
				move_by (line_new, add (line_direction, -90.0), abs (offset));
			end move_right;

			procedure move_left is begin
				move_by (line_new, add (line_direction, +90.0), abs (offset));
			end move_left;
			
		begin
			-- Set a test point that is very close to the center of the line.
			-- The point is located in direction dir_scratch away from the center:
			dir_scratch := add (line_direction, +90.0);
			test_point := type_point (move (to_point (center), dir_scratch, type_distance'small));
			--put_line ("tp " & to_string (test_point));

			-- Depending on the location of the test point, means inside or outside
			-- the polygon and the mode we move the line to the right or to the left:
			declare
				tp_status : constant type_point_to_polygon_status :=
					get_point_to_polygon_status (polygon, to_vector (test_point));
			begin
				case mode is
					when EXPAND =>
					-- move line toward outside. Polygon area becomes greater:
						if tp_status.location = INSIDE then
							move_right;
						else
							move_left;
						end if;

					when SHRINK =>
					-- move the line toward inside. Polygon area becomes smaller:
						if tp_status.location = INSIDE then
							move_left;
						else
							move_right;							
						end if;

					when NOTHING =>
						raise constraint_error; -- should never happen
				end case;
					
			end;
			
			return to_line_vector (line_new);
		end offset_line;
	

		package pac_line_vectors is new doubly_linked_lists (type_line_vector);
		use pac_line_vectors;
		line_vectors : pac_line_vectors.list;
		
		
		procedure do_segment (c : in pac_edges.cursor) is
			lv_tmp : type_line_vector;			
		begin
			lv_tmp := offset_line (element (c));
			--put_line ("lv " & to_string (lv_tmp));
			line_vectors.append (lv_tmp);
		end do_segment;


		polygon_segments_new : pac_edges.list;
		
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
				--polygon_segments_new.segments.append (
					--(shape => LINE, segment_line => (LS, LE)));

				polygon_segments_new.append ((LS, LE));

				
				if cp = line_vectors.last then

					--polygon_segments_new.segments.append (
						--(shape => LINE, segment_line => (LE, INIT)));
					polygon_segments_new.append ((LE, INIT));
					
				end if;

				
				-- The end point of this line will be the 
				-- start point of the next line (irrelevant for last line vector):
				LS := LE;
			end if;
		end query_line;

		
	begin -- offset_polygon

		if mode /= NOTHING then
			
			polygon.edges.iterate (do_segment'access);

			-- Compute the intersections of the line_vectors.
			-- The intersections become the start and end points
			-- of the new line-segments:
			line_vectors.iterate (query_line'access);

			polygon.edges := polygon_segments_new;

			--if not is_closed (polygon).closed then
				--raise constraint_error with "Polygon NOT closed !";
			--end if;
		end if;
		
	end offset_polygon;



	function offset_polygons (
		polygons	: in pac_polygons.list;
		offset		: in type_distance)
		return pac_polygons.list
	is
		use pac_polygons;
		result : pac_polygons.list;

		-- Iterate the given list of polygons. 
		-- Offset each of them and append it to the result:
		
		procedure query_polygon (c : in pac_polygons.cursor) is
			p : type_polygon := element (c);
		begin
			offset_polygon (p, offset);
			result.append (p);
		end query_polygon;
		
	begin
		polygons.iterate (query_polygon'access);
		return result;
	end offset_polygons;


	procedure offset_polygons (
		polygons	: in out pac_polygons.list;
		offset		: in type_distance)
	is begin
		polygons := offset_polygons (polygons, offset);
	end offset_polygons;


	

	function to_mode (
		offset : in type_distance)
	return type_mode is 
		result : type_mode := NOTHING;
	begin
		if offset > 0.0 then
			result := EXPAND;
		elsif offset < 0.0 then
			result := SHRINK;
		else
			result := NOTHING;
		end if;

		return result;
	end to_mode;

	
end et_geometry_2.polygons.offsetting;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

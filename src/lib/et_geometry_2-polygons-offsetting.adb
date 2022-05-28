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
with ada.exceptions;			use ada.exceptions;


package body et_geometry_2.polygons.offsetting is


	function to_string (oe : in type_offset_edge) return string is begin
		return to_string (oe.edge);
			--& " line: " & to_string (oe.line);
	end to_string;


	function to_string (oe : in pac_offset_edges.cursor) return string is begin
		return to_string (element (oe));
	end to_string;


	function get_next_direct_intersection (
		start	: in pac_offset_edges.cursor;
		first	: in pac_offset_edges.cursor;
		debug	: in boolean := false)
		return type_next_direct_intersection
	is
		result : type_next_direct_intersection;

		c : pac_offset_edges.cursor := next (start);
	begin
		--put_line ("start " & to_string (start));

		--goto skip;
		
		--while c /= start loop
		while c /= pac_offset_edges.no_element loop

			--if c = start then
				--exit;
			--end if;
			
			--put_line ("probe " & to_string (c));
			
			declare
				I : constant type_intersection_of_two_lines := get_intersection (
					edge_1 => element (start).edge,
					edge_2 => element (c).edge);
			begin
				if I.status = EXISTS then
					--put_line ("EXISTS");
					
					result.cursor := c;
					result.place := I.intersection.vector;

					--put_line ("intersects " 
						--& to_string (result.cursor) 
						--& " at " & to_string (result.place));
					
					exit;
				end if;
			end;
			
			next (c);

			--if c = pac_offset_edges.no_element then
				--c := first;
			--end if;
		end loop;

	<<skip>>

	--goto skip2;
		
		--if c = pac_offset_edges.no_element then
		if result.cursor = pac_offset_edges.no_element then
			--put_line ("nothing");
			
			c := first;

			while c /= previous (start) loop
				
				declare
					I : constant type_intersection_of_two_lines := get_intersection (
						edge_1 => element (start).edge,
						edge_2 => element (c).edge);
				begin
					if I.status = EXISTS then
						--put_line ("EXISTS");
						
						result.cursor := c;
						result.place := I.intersection.vector;
						--result.restarted := true;

						--put_line ("intersects " 
							--& to_string (result.cursor) 
							--& " at " & to_string (result.place));
						
						exit;
					end if;
				end;
				
				next (c);
			end loop;
		end if;

	<<skip2>>
		
		return result;

		exception when event: others =>
			put_line (exception_information (event));
			--put_line (exception_occurrence (event));
			raise;
		
	end get_next_direct_intersection;
	

	function get_relevant (
		intersection : in pac_edge_intersections.cursor) 
		return type_vector
	is 
		use pac_edge_intersections;
	begin
		if element (intersection).direct_available then
			return element (intersection).place_direct;
		else
			return element (intersection).place_indirect;
		end if;
	end get_relevant;

	

	procedure offset_polygon (
		polygon		: in out type_polygon;
		offset		: in type_distance;
		debug		: in boolean := false) 
	is
		-- Mode tells whether we are shrinking, expanding
		-- or whether there is nothing to do:
		mode : constant type_mode := to_mode (offset);	

		
		-- This procedure computes an "offset edge" from an original edge.
		-- An "offset edge" is a composite of an edge and an infinite long line.
		-- See specification of type_offset_edge:
		function offset_edge (
			edge : in type_edge)
			return type_offset_edge
		is
			edge_new : type_edge := edge;
			center : type_vector := get_center (edge);
			
			edge_direction : type_angle := get_direction (edge);
			dir_scratch : type_angle;			
			test_point : type_vector;

			-- These two procedures move the edge_new to the right or
			-- to the left (seen relative to the edge direction):
			procedure move_right is begin
				move_by (edge_new, add (edge_direction, -90.0), abs (offset));
			end move_right;

			procedure move_left is begin
				move_by (edge_new, add (edge_direction, +90.0), abs (offset));
			end move_left;
			
		begin
			--put_line ("edge direction:" & to_string (edge_direction));
			
			-- Set a test point that is very close to the center of the edge.
			-- The point is located in direction dir_scratch away from the center:
			dir_scratch := add (edge_direction, +90.0);

			-- CS:
			test_point := move_by (center, dir_scratch, type_float_internal (type_distance'small));
			--test_point := move_by (center, dir_scratch, 100.0 * type_float_internal'small);

			--put_line ("tp " & to_string (test_point));

			-- Depending on the location of the test point, means inside or outside
			-- the polygon and the mode we move the edge to the right or to the left:
			declare
				tp_status : constant type_point_to_polygon_status :=
					get_point_to_polygon_status (polygon, test_point);
			begin
				case mode is
					when EXPAND =>
					-- move edge toward outside. Polygon area becomes greater:
						if tp_status.location = INSIDE then
							move_right;
						else
							move_left;
						end if;

					when SHRINK =>
					-- move the edge toward inside. Polygon area becomes smaller:
						if tp_status.location = INSIDE then
							move_left;
						else
							move_right;							
						end if;

					when NOTHING =>
						raise constraint_error; -- should never happen
				end case;
					
			end;

			return (
				edge => edge_new,
				line => to_line_vector (edge_new));
		end offset_edge;
	

		-- After preprocessing the offset edges are collected here:
		offset_edges : pac_offset_edges.list;
		

		-- This procedure computes an "offset edge" from the cursor to 
		-- an original edge and stores the offset edge in list "offset_edges":
		procedure preprocess_edge (c : in pac_edges.cursor) is
			OE : type_offset_edge;
		begin
			--put_line ("original edge: " & to_string (element (c)));
			OE := offset_edge (element (c));
			offset_edges.append (OE); -- CS pass OE inline
		end preprocess_edge;


		-- The edges of the final polygon. They will overwrite
		-- the original edges:
		polygon_segments_new : pac_edges.list;

		INIT : type_vector;		
		LE : type_vector;
		LS : type_vector;
		I : type_intersection_of_two_lines := (status => EXISTS, others => <>);


		-- This procedure takes a cursor to an "offset edge" and 
		-- computes the intersection of its infinite line with the 
		-- previous infinite line.
		-- The intersection becomes the end point of a new edge. The start point
		-- of that edge has been computed in the previous call of this procedure.
		-- The new edge will then be appended to polygon_segments_new:
		procedure get_intersection_with_previous_edge (
			cp : in pac_offset_edges.cursor) 
		is
			-- cp is the primary cursor that points to the current line.
			
			-- The secondary cursor that points to the line that is
			-- before the candidate line:
			cs : pac_offset_edges.cursor;

		begin
			--put_line ("lv " & to_string (element (cp)));

			-- In case the candidate edge is the first, then the intersection
			-- with the last edge among the offset_edges must be computed:
			if cp = offset_edges.first then
				cs := offset_edges.last;
				I := get_intersection (element (cp).line, element (cs).line);

				-- The intersection forms the start point of the first new edge:
				LS := I.intersection.vector;

				-- Preparation for the last edge:
				INIT := LS;
				
			else
			-- In case the candidate edge is not the first, set the secondary
			-- cursor to the previous edge and compute the intersection:
				cs := previous (cp);
				I := get_intersection (element (cp).line, element (cs).line);

				-- The intersection forms the end point of the new edge.
				LE := I.intersection.vector;

				-- The start point of the new edge has been computed during
				-- the previous execution of this procedure:

				-- The new edge is now complete. Append to new segments:
				polygon_segments_new.append ((LS, LE));

				-- In case the candidate edge is the last one,
				-- use INIT as the end point:
				if cp = offset_edges.last then
					polygon_segments_new.append ((LE, INIT));
				end if;
				
				-- The end point of this line will be the 
				-- start point of the next line (irrelevant for last line vector):
				LS := LE;
			end if;
		end get_intersection_with_previous_edge;


		function get_intersection_with_next_edge (
			cp : in pac_offset_edges.cursor) 
			return type_vector
		is
			-- cp is the primary cursor that points to the current edge.
			-- The secondary cursor that points to the edge that comes
			-- after the candidate edge:
			cs : pac_offset_edges.cursor;
		begin
			-- In case the candidate edge is the last, then the intersection
			-- with the first edge among the offset_edges must be computed:
			if cp = offset_edges.last then
				cs := offset_edges.first;
			else
			-- In case the candidate edge is not the last, set the secondary
			-- cursor to the next edge and compute the intersection:
				cs := next (cp);
			end if;

			return get_intersection (element (cp).line, element (cs).line).intersection.vector;
		end get_intersection_with_next_edge;

		
		E : pac_offset_edges.cursor;
		N : type_next_direct_intersection;

		use pac_edge_intersections;
		intersections : pac_edge_intersections.list;
		
		
		procedure query_intersection (c : in pac_edge_intersections.cursor) is
			i : type_vector;
		begin
			--new_line;
			i := get_relevant (c);
			--put_line (" indirect " & to_string (element (c).place_indirect));

			--if element (c).direct_available then
				--put_line (" direct   " & to_string (element (c).place_direct));
			--end if;

			put_line (" " & to_string (i));
			--if c = intersections.first then
				--INIT := i;
			--else
				--LE := v;
		end query_intersection;

		
		--procedure connect_edges is
			--c : pac_edge_intersections.cursor := intersections.first;
			--i : type_vector;
		--begin
			--if debug then
				--put_line ("intersections:");
			--end if;

			--while c /= pac_edge_intersections.no_element loop
				----i := get_relevant (c);
				--if element (c).direct_available then
					

				--if debug then
					--put_line (" " & to_string (i));
				--end if;

				--next (c);
			--end loop;
		--end connect_edges;

		
	begin -- offset_polygon

		if mode /= NOTHING then

			-- Preprocessing the polygon edges.
			-- For each edge an "offset edge" is computed and stored
			-- in list offset_edges:
			polygon.edges.iterate (preprocess_edge'access);

			-- Traverse through the offset_edges and
			-- find intersections between them.
			E := offset_edges.first;
			while E /= pac_offset_edges.no_element loop
				if debug then
					new_line;
					put_line ("EDGE: " & to_string (element (E).edge));
				end if;

				-- Get the intersection of the candidate edge with the previous edge
				-- and form a new edge:
				-- NOTE: The edges in list offset_edge are orderd counter clockwise.
				--get_intersection_with_previous_edge (E);
				--indirect_intersection := get_intersection_with_next_edge (E);
				--intersections.append (get_intersection_with_next_edge (E));

				-- Get the next direct intersection with an "offset edge" in
				-- counter-clockwise direction after the candidate "offset edge":
				N := get_next_direct_intersection (E, offset_edges.first, debug);

				-- If there is a direct intersection then fast-forward to the
				-- corresponding "offset edge":
				--if N.cursor /= pac_offset_edges.no_element then
				if N.cursor = pac_offset_edges.no_element then
					if debug then
						put_line (" no direct intersection found");
					end if;

					intersections.append ((
						direct_available	=> FALSE,
						place_indirect		=> get_intersection_with_next_edge (E)));
						

					-- The end point of the new edge is where
					-- the direct intersection is:
					--LE := N.place;

					--polygon_segments_new.append ((LS, LE));

					-- This point of intersection will also serve as start point
					-- of the next edge:
					--LS := LE;

					--if N.restarted then
						--exit;
					--end if;
					
					-- Fast-forward to the intersected edge:
					--E := N.cursor;
				else
					if debug then
						put_line (" next direct intersection: " & to_string (N.place));
					end if;

					intersections.append ((
						direct_available	=> TRUE,
						place_direct		=> N.place,
						place_indirect		=> get_intersection_with_next_edge (E)));
				end if;
				
				next (E);
			end loop;

			intersections.iterate (query_intersection'access);

			--connect_edges;
			

			-- Overwrite the old edges of the given polygon with the new edges:
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

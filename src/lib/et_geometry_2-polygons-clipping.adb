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
	use pac_points;
	use pac_vertices;
	

	function to_string (intersection : in type_intersection)
		return string
	is begin
		return to_string (intersection.position) 
			& " " & type_direction'image (intersection.direction);
	end to_string;


	
	function to_string (vertex : in type_vertex)
		return string
	is begin
		return to_string (vertex.position) 
			& " " & type_direction'image (vertex.direction);
	end to_string;

	

	function to_string (vertices : in pac_vertices.list) return string is
		use ada.strings.unbounded;
		
		result : unbounded_string;
		
		procedure query_vertex (v : in pac_vertices.cursor) is begin
			result := result & " " 
				& trim (to_string (get_x (element (v).position)), left)
				& "/"
				& trim (to_string (get_y (element (v).position)), left)
				& " " & type_category'image (element (v).category);

			if element (v).category = INTERSECTION then
				result := result & " " & type_direction'image (element (v).direction);
			end if;

			result := result & ".";
		end query_vertex;
			
	begin
		vertices.iterate (query_vertex'access);
		return to_string (result);
	end to_string;


	
	function is_entering (v : pac_vertices.cursor) return boolean is begin
		if element (v).category = INTERSECTION then
			if element (v).direction = ENTERING then
				return true;
			else
				return false;
			end if;
		else
			return false;
		end if;
	end is_entering;

	
	function is_leaving (v : pac_vertices.cursor) return boolean is begin
		if element (v).category = INTERSECTION then
			if element (v).direction = LEAVING then
				return true;
			else
				return false;
			end if;
		else
			return false;
		end if;
	end is_leaving;


	
	procedure sort_by_distance (
		vertices	: in out pac_vertices.list;
		reference	: in type_point)
	is
		type type_item is record
			-- We will be sorting intersections only (no regular vertices):
			vertex		: type_vertex (category => INTERSECTION);
			distance	: type_distance_positive;
		end record;

		
		function "<" (left, right : in type_item) return boolean is begin
			if left.distance < right.distance then
				return true;
			else
				return false;
			end if;
		end;
	
			
		package pac_items is new doubly_linked_lists (type_item);
		use pac_items;
		
		items : pac_items.list;

		
		procedure query_vertex (v : in pac_vertices.cursor) is 
			d : type_distance_polar;
		begin
			d := get_distance (type_point (reference), type_point (element (v).position));
			
			items.append (new_item => (
				vertex		=> element (v),
				distance	=> get_absolute (d)));
		end query_vertex;

		

		package pac_sorting is new pac_items.generic_sorting;
		use pac_sorting;
		

		procedure query_item (i : in pac_items.cursor) is begin
			vertices.append (element (i).vertex);
		end query_item;
		
		
	begin
		-- Collect vertices and their distance to the reference
		-- in list "items":
		vertices.iterate (query_vertex'access);

		-- Sort items by distance to reference:
		sort (items);

		-- The old vertices are no longer required:
		vertices.clear;
		-- New vertices will be appended here.
		

		-- Traverse items and append them one by one to the
		-- list of vertices:
		items.iterate (query_item'access);
	end sort_by_distance;



	function to_polygon (vertices : in pac_vertices.list)
		return type_polygon
	is
		result : type_polygon;		
		
		procedure query_vertex (v : in pac_vertices.cursor) is 
			edge : type_line;
		begin
			-- The candidate vertex becomes the end of 
			-- the edge:
			edge.end_point := element (v).position;

			-- The vertex before the candidate vertex 
			-- will be the start of the edge:
			if v = vertices.first then
				edge.start_point := element (vertices.last).position;
			else
				edge.start_point := element (previous (v)).position;
			end if;
			
			append (result.contours.segments, (LINE, edge));
		end query_vertex;
		
	begin
		-- Convert the list of vertices to a list of lines (or edges):
		vertices.iterate (query_vertex'access);
		return result;
	end to_polygon;

	

	function clip (
		polygon_A	: in type_polygon'class;
		polygon_B	: in type_polygon'class)
		return pac_clipped.list
	is
		result : pac_clipped.list;

		
		intersections : pac_intersections.list;
		
		-- Seaches for intersections of the given two polygons
		-- and stores them in container "intersection".
		-- An intersection is where the edge of polygon A intersects
		-- an edge of polygon B. The intersection has direction "entering"
		-- if the start point of the edge (of polygon A) is outside of polygon B.
		-- In other words, if polygon A enters polygon B.
		-- If there are no intersections then "intersections" stays empty:
		procedure find_intersections is
			
			procedure query_A_segment (a : in pac_polygon_segments.cursor) is

				procedure query_B_segment (b : in pac_polygon_segments.cursor) is
					-- Compute the point of intersection:
					I2L : type_intersection_of_two_lines := get_intersection (
						element (a).segment_line, element (b).segment_line);

					p : type_point;
					Q : type_inside_polygon_query_result;

					-- The fully specified intersection of edge A and B:
					IAB : type_intersection;
				begin
					-- There is nothing to do if
					-- - the two edges do not intersect each other
					-- - they run parallel and overlap each other
					if I2L.status = EXISTS then
						-- The two edges do intersect at point p:
						p := to_point (I2L.intersection.vector);

						-- Depending on the inside/outside status of the start point of edge A
						-- we set the direction of the intersection.
						-- As supportive information the affected edges are also stored in IAB.
						-- This information serves later to create two separate lists of vertices
						-- for the A and the B polygon:
						Q := in_polygon_status (polygon_B, element (a).segment_line.start_point);
						
						if Q.status = INSIDE then
							IAB := (P, LEAVING, element (a).segment_line, element (b).segment_line);
						else
							IAB := (P, ENTERING, element (a).segment_line, element (b).segment_line);
						end if;

						intersections.append (IAB);
						--put_line (to_string (IAB));
					end if;
				end query_B_segment;
				
			begin			
				-- Traverse the edges of polygon B:
				polygon_B.contours.segments.iterate (query_B_segment'access);
			end query_A_segment;

		begin
			-- Traverse the edges of polygon A:
			polygon_A.contours.segments.iterate (query_A_segment'access);
		end find_intersections;


		-- Recursively changes the direction of an 
		-- intersection (in list "intersection") to "leaving".
		-- Identifies the targeted intersection by the position of the given
		-- vertex. 
		-- CS: Assumes the position of the intersection is unique. Means there is
		-- no other intersection having the same x/y coordinates.
		procedure update_intersection (v : in pac_vertices.cursor) is

			procedure change_direction (i : in out type_intersection) is begin
				i.direction := LEAVING;
			end;
		
			c : pac_intersections.cursor := intersections.first;
		begin
			while c /= pac_intersections.no_element loop
				if element (c).position = element (v).position then
					intersections.update_element (c, change_direction'access);
					exit;
				end if;
				next (c);
			end loop;
		end update_intersection;

		
		-- Returns the intersection points on a given edge.
		-- Searches in list "intersections" using the supportive information
		-- of affected edges (see specs of type_intersection and proc find_intersections).
		-- Orders the points by their distance to the start point of 
		-- the edge (nearest first).
		-- The parameter AB determines whether to look for intersections
		-- on edges of the A or the B polygon:
		function get_intersections_on_edge (
			edge	: in type_line;
			AB		: in type_AB_polygon)
			return pac_vertices.list
		is 
			result : pac_vertices.list;
			
			procedure query_intersection (i : in pac_intersections.cursor) is begin
				case AB is
					when A =>
						if element (i).edge_A = edge then
							result.append (new_item => (
								position	=> element (i).position,
								category	=> INTERSECTION,
								direction	=> element (i).direction));
						end if;

					when B =>
						if element (i).edge_B = edge then
							result.append (new_item => (
								position	=> element (i).position,
								category	=> INTERSECTION,
								direction	=> element (i).direction));

						end if;
				end case;
			end query_intersection;


			-- Iterates the vertices in "result" and detect successive
			-- entering points. Changes the direction of the intersection
			-- to "leaving". Changes direction of the affected intersection
			-- in the list "intersection".
			procedure render_successive_enterings is
				c : pac_vertices.cursor := result.first;

				procedure change_direction (v : in out type_vertex) is begin
					v.direction := LEAVING;
				end;
				
			begin
				-- Iterate all elements of "result":
				while c /= pac_vertices.no_element loop

					if c /= result.first then
						if element (c).direction = ENTERING
						and element (previous (c)).direction = ENTERING then
							result.update_element (c, change_direction'access);
							update_intersection (c);
						end if;
					end if;
					
					next (c);
				end loop;
			end render_successive_enterings;
			
			
		begin
			intersections.iterate (query_intersection'access);
			sort_by_distance (result, edge.start_point);			

			-- After sorting the intersections there may two 
			-- successive entering intersections. This special case arises
			-- in case of the STC. See package specification header.
			-- Two successive entering intersections are not correct. So the second
			-- intersection must be rendered to "leaving".
			if AB = A then
				render_successive_enterings;
			end if;
			
			return result;
		end get_intersections_on_edge;


		
		-- These are the lists of vertices and intersections
		-- in counter-clockwise order for polygon A and B:
		vertices_A, vertices_B : pac_vertices.list;
		

		-- Fills container vertices_A counter-clockwise with the vertices of polygon A
		-- and the intersections (leaving or entering) with polygon B.
		-- MUST BE CALLED BEFORE procedure make_vertices_B because here the the
		-- list "intersections" is updated in order to handle the STC (see
		-- comments in header of package specs):
		procedure make_vertices_A is

			procedure query_segment (s : in pac_polygon_segments.cursor) is
				v_list : pac_vertices.list;
			begin
				-- The start point of the candidate polygon segment is
				-- always a regular vertex:
				vertices_A.append (new_item => (
					category	=> REGULAR,
					position	=> element (s).segment_line.start_point));

				-- Get the vertices (on the current edge) that follow
				-- after the start point:
				v_list := get_intersections_on_edge (element (s).segment_line, A);
				-- Function get_intersections_on_edge has updated the "intersections".

				-- Append the vertices to container vertices_A:
				splice (target => vertices_A, before => pac_vertices.no_element, source => v_list);
			end query_segment;
			
		begin
			polygon_A.contours.segments.iterate (query_segment'access);
		end make_vertices_A;
		

		-- Fills container vertices_B counter-clockwise with the vertices of polygon B
		-- and the intersections (leaving or entering) with polygon A:
		procedure make_vertices_B is

			procedure query_segment (s : in pac_polygon_segments.cursor) is
				v_list : pac_vertices.list;
			begin
				-- The start point of the candidate polygon segment is
				-- always a regular vertex:
				vertices_B.append (new_item => (
					category	=> REGULAR,
					position	=> element (s).segment_line.start_point));
				
				-- Get the vertices (on the current edge) that follow
				-- after the start point:
				v_list := get_intersections_on_edge (element (s).segment_line, B);

				-- Append the vertices to container vertices_B:
				splice (target => vertices_B, before => pac_vertices.no_element, source => v_list);
			end query_segment;
			
		begin
			polygon_B.contours.segments.iterate (query_segment'access);
		end make_vertices_B;


		
		vertice_A_cursor : pac_vertices.cursor;

		-- Returns a cursor to the first entering intersection in
		-- vertices_A.
		-- If no entering intersection found, returns no_element:
		function get_first_entering return pac_vertices.cursor is
			v : pac_vertices.cursor := vertices_A.first;
		begin
			while v /= pac_vertices.no_element loop
				if is_entering (v) then
					exit;
				end if;
				next (v);
			end loop;

			return v;
		end get_first_entering;


		-- Returns the vertices (in vertices_A) from the entering vertex 
		-- to the next leaving vertex. The vertices are removed from
		-- vertices_A so that they won't be visited again:
		function get_until_leaving (entering : in pac_vertices.cursor)
			return pac_vertices.list 
		is
			v : pac_vertices.cursor;
			result : pac_vertices.list;

			procedure collect is begin
				while v /= pac_vertices.no_element loop
					result.append (element (v));
					
					if is_leaving (v) then
						exit;
					end if;

					next (v);
				end loop;
			end collect;
			
		begin
			-- Preset cursor v to the given entering vertex:
			v := entering;

			-- Collect vertices from entering vertex to
			-- the next leaving vertex:
			collect;

			-- If no leaving vertex found (until end of list),
			-- restart the search from the top of the list:
			if v = pac_vertices.no_element then
				v := vertices_A.first;
				collect;
			end if;
			
			-- Remove the vertices from vertices_A:
			v := entering;
			vertices_A.delete (position => v, count => length (result));

			-- The first the result is not required because
			-- this is where we have started:
			result.delete_first;
			
			return result;
		end get_until_leaving;
		

		-- Returns the vertices (in vertices_B) between the leaving vertex 
		-- and the next entering vertex. The vertices are removed from
		-- vertices_B so that they won't be visited again:
		function get_until_entering (leaving : in pac_vertices.cursor)
			return pac_vertices.list 
		is
			v : pac_vertices.cursor;
			result : pac_vertices.list;

			procedure collect is begin
				while v /= pac_vertices.no_element loop
					result.append (element (v));
					
					if is_entering (v) then
						exit;
					end if;

					next (v);
				end loop;
			end collect;
			
		begin
			-- Preset cursor v to the given leaving vertex:
			v := leaving;

			-- Collect vertices from leaving vertex to
			-- the next entering vertex:
			collect;

			-- If no entering vertex found (until end of list),
			-- restart the search from the top of the list:
			if v = pac_vertices.no_element then
				v := vertices_B.first;
				collect;
			end if;
			
			-- Remove the collected vertices from vertices_B:
			v := leaving;
			vertices_B.delete (position => v, count => length (result));

			-- The first the result is not required because
			-- this is where we have started:
			result.delete_first;
			
			return result;
		end get_until_entering;

		
		-- Temporarily collections of vertices and intersections:
		vertices_tmp_1 : pac_vertices.list; -- primary collection
		vertices_tmp_2 : pac_vertices.list; -- secondary collection
		
		-- The start point when walking along the vertices_A is
		-- always an intersection:
		v_start : type_vertex (category => INTERSECTION);


		-- This is a safety measure to prevent indefinite looping.
		-- CS: Increase upper limit if required:
		subtype type_safety_counter is natural range 0 .. 100;
		safety_counter : type_safety_counter := 0;

		
	begin -- clip

		-- Find intersections of the given two polygons:
		find_intersections;

		-- If there are intersections then the actual work begins.
		-- Otherwise do nothing and return an empty list of polygons:
		if not is_empty (intersections) then
		
			make_vertices_A; -- MUST be called BEFORE make_vertices_B !
			--put_line ("A: " & to_string (vertices_A));

			make_vertices_B;
			--put_line ("B: " & to_string (vertices_B));


			-- Go to the first entering intersection in vertices_A:
			vertice_A_cursor := get_first_entering;
			--put_line ("first entering " & to_string (element (vertice_A_cursor)));

			-- Traverse vertices_A until no more entering vertex
			-- can be found:
			while vertice_A_cursor /= pac_vertices.no_element loop

				-- A sub-polygon starts at v_start. When walking along the
				-- edges of polygon A or B we will eventually get back to 
				-- the start point v_start. The current sub-polygon is then complete.
				v_start := element (vertice_A_cursor);

				-- Walk along the vertices (and intersections) of polygon A until
				-- a leaving intersection:
				vertices_tmp_1 := get_until_leaving (vertice_A_cursor);
				-- Now we have the intersections and vertices from after the start point 
				-- to (and including) the leaving intersection L.

				-- Find the very leaving intersection L in polygon B and walk
				-- along the vertices (and intersections) of polygon B until
				-- an entering intersection:
				vertices_tmp_2 := get_until_entering (vertices_B.find (vertices_tmp_1.last_element));

				
				loop
					-- safety measure to prevent forever-looping:
					safety_counter := safety_counter + 1;
					if safety_counter = type_safety_counter'last then
						raise constraint_error with "safety counter overrun !";
					end if;
					

					-- Splice the intersections and vertices of A and B.
					-- Collect everything in the primary collection:
					splice (
						target	=> vertices_tmp_1, -- primary
						before	=> pac_vertices.no_element, 
						source 	=> vertices_tmp_2); -- will be emptied

					-- If we have reached the start point then the sub-polygon
					-- is complete.
					if last_element (vertices_tmp_1) = v_start then
						exit;
					else
						-- In order to handle the STC (see header of the package specification)
						-- this stuff is required
						-- as an extension of the Weiler-Atherton algorithm:
						-- If sub-polygon is not complete, then again go to the first
						-- entering intersection of polygon A:
						vertice_A_cursor := get_first_entering;

						-- Get the intersections and vertices until
						-- the a leaving intersection in polygon A:
						vertices_tmp_2 := get_until_leaving (vertice_A_cursor);

						-- Append the intersection (and vertices) to the primary
						-- collection:
						splice (
							target	=> vertices_tmp_1, -- primary
							before	=> pac_vertices.no_element, 
							source 	=> vertices_tmp_2); -- will be emtied

						-- Switch to polygon B and get intersections
						-- until (and including) an entering intersection
						-- into the secondary collection. The secondary collection
						-- will be appended to the primary one once this loop
						-- starts again:
						vertices_tmp_2 := get_until_entering (vertices_B.find (vertices_tmp_1.last_element));
					end if;
				end loop;
					

				-- Append the sub-polygon to the result:
				result.append (to_polygon (vertices_tmp_1));
				--put_line (to_string (to_polygon (vertices_tmp_1)));

				-- Get the next entering vertex from vertices_A.
				-- In case there is no entering vertex any more, then this
				-- loop will be the last:
				vertice_A_cursor := get_first_entering;
			end loop;

		end if;
		
		return result;
	end clip;

	
end et_geometry_2.polygons.clipping;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

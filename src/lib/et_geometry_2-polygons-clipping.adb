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

with ada.exceptions; 			use ada.exceptions;
with gnat.source_info;

with et_exceptions;				use et_exceptions;


package body et_geometry_2.polygons.clipping is

	use pac_polygon_segments;
	use pac_points;
	use pac_vertices;

	
	function to_string (vertex : in type_vertex)
		return string
	is begin
		return to_string (vertex.position) 
			& " " & type_intersection_direction'image (vertex.direction);
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
				result := result & " " & type_intersection_direction'image (element (v).direction);
			end if;

			result := result & ".";
		end query_vertex;
			
	begin
		vertices.iterate (query_vertex'access);
		return to_string (result);
	end to_string;



	function same_position (
		intersection_1, intersection_2 : in pac_intersections.cursor)
		return boolean
	is
		result : boolean := false;
	begin
		if element (intersection_1).position = element (intersection_2).position then
			result := true;
		end if;

		return result;
	end same_position;

	

	function get_real_intersections (
		intersections	: in pac_intersections.list)
		return pac_intersections.list
	is
		result : pac_intersections.list;

		procedure compare_position_and_direction (
			i1, i2 : in pac_intersections.cursor)
		is begin
			-- The two intersections in question must have
			-- same x/y position and differing direction, means
			-- one is an entering and the other is a leaving one.
			-- On this x/y position we have no real intersection
			-- but just a touch point:
			if same_position (i1, i2)
			and element (i1).direction /= element (i2).direction
			then 
				null; -- A touches B -> skip this intersection
			else
				-- is real -> collect this intersection
				result.append (element (i1)); 
				--put_line ("real intersection:" & to_string (element (i1)));
			end if;
		end compare_position_and_direction;
				
		procedure query_intersection (
			c : in pac_intersections.cursor) 
		is begin
			-- CS: We assume that leaving and entering points of
			-- a touch-point follow each other in the given 
			-- list of intersection. So we always look at the 
			-- predecessor of the candidate intersection (indicated by 
			-- cursor c):
			if c = intersections.first then
				compare_position_and_direction (c, intersections.last);
			else
				compare_position_and_direction (c, previous (c));
			end if;
		end query_intersection;
		
	begin
		intersections.iterate (query_intersection'access);
		return result;
	end get_real_intersections;


	
	function are_redundant (
		i1, i2 : in pac_intersections.cursor)
		return boolean
	is
		result : boolean := false;
	begin
		if element (i1).position = element (i2).position then
			if element (i1).direction = element (i2).direction then
				result := true;
			end if;
		end if;

		return result;
	end are_redundant;


	
	function count (
		intersections	: in pac_intersections.list;
		intersection	: in type_intersection)
		return type_redundant_intersection_count
	is
		result : type_redundant_intersection_count := 0;

		procedure query_intersection (c : in pac_intersections.cursor) is begin
			if element (c).position = intersection.position
			and element (c).direction = intersection.direction then
				result := result + 1;
			end if;
		end query_intersection;
		
	begin
		intersections.iterate (query_intersection'access);
		return result;
	end count;
	
	
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


	function is_regular (v : pac_vertices.cursor) return boolean is begin
		if element (v).category = REGULAR then
			return true;
		else
			return false;
		end if;
	end is_regular;

	

	function same_position (
		vertex_1, vertex_2 : in pac_vertices.cursor)
		return boolean
	is 
		result : boolean := false;
	begin
		if not is_regular (vertex_1) and is_regular (vertex_2) then
			
			if element (vertex_1).position = element (vertex_2).position then
				result := true;
			end if;
		end if;
			
		return result;
	end same_position;
	
	
	
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
		polygon_B	: in type_polygon'class;
		debug		: in boolean := false)
		return pac_clipped.list
	is
		result : pac_clipped.list;

		
		intersections : pac_intersections.list;

		
		-- Removes successive redundant intersections from 
		-- list "intersections" so that only one of them is left:
		procedure remove_redundant_intersections is
			c : pac_intersections.cursor := intersections.first;
			i_list_new : pac_intersections.list;
		begin
			--put_line ("removing redundant intersections ...");
			while c /= pac_intersections.no_element loop
				--put_line (to_string (element (c)));
				
				if c /= intersections.last then
					if not are_redundant (next (c), c) then
						i_list_new.append (element (c));
					end if;
				else
					if not are_redundant (intersections.first, c) then
						i_list_new.append (element (c));
					end if;
				end if;
				
				next (c);
			end loop;			

			intersections := i_list_new;
		end remove_redundant_intersections;

		
		-- Seaches for intersections of the given two polygons
		-- and stores them in container "intersection".
		-- An intersection is where the edge of polygon A intersects
		-- an edge of polygon B. The intersection has direction "entering"
		-- if the start point of the edge (of polygon A) is outside of polygon B.
		-- In other words, if polygon A enters polygon B.
		-- If there are no intersections then "intersections" stays empty:
		procedure find_intersections is
			
			procedure query_A_segment (a : in pac_polygon_segments.cursor) is

				-- The status of the A-edge relative to polygon B:
				LPS : constant type_line_to_polygon_status := 
					get_line_to_polygon_status (polygon_B, element (a).segment_line);

				
				procedure collect_intersections is 

					procedure query_intersection (i : in pac_line_edge_intersections.cursor) is
						use pac_line_edge_intersections;
						b_edge : pac_polygon_segments.cursor := element (i).edge;
					begin
						-- Ignore intersection if edge A runs "parallel" to edge B:
						if not lines_overlap (element (a).segment_line, element (b_edge).segment_line) then
							intersections.append ((
								type_intersection_base (element (i)) with
								edge_A => element (a).segment_line,
								edge_B => element (b_edge).segment_line));

							if debug then
								put_line ("intersection: " & to_string (intersections.last_element));
							end if;
						end if;
					end query_intersection;

				begin
					LPS.intersections.iterate (query_intersection'access);
				end collect_intersections;


				
				procedure use_start_point_as_intersection is 
					-- The intersection of edge A and B:
					IAB : type_intersection;
				begin
					IAB.position := element (a).segment_line.start_point;
					IAB.edge_A := element (a).segment_line;
					
					case LPS.start_point.location is
						when ON_EDGE =>
							IAB.direction := LPS.start_point.direction_on_edge;

							-- Get the touched B-edge at the start point:
							IAB.edge_B := element (LPS.start_point.edge).segment_line; 

							-- Ignore intersection if edge A runs "parallel" to edge B:
							if not lines_overlap (element (a).segment_line, IAB.edge_B) then
								-- collect intersection:
								intersections.append (IAB);

								if debug then
									put_line ("intersection (start on edge): " & to_string (intersections.last_element));
								end if;
							end if;
							
						when ON_VERTEX =>
							IAB.direction := LPS.start_point.direction_on_vertex;

							-- Get the touched B-edge that starts at the start point:
							IAB.edge_B := element (LPS.start_point.edges.edge_2).segment_line;

							-- Ignore intersection if edge A runs "parallel" to edge B:
							if not lines_overlap (element (a).segment_line, IAB.edge_B) then
								-- collect intersection:
								intersections.append (IAB);

								if debug then
									put_line ("intersection (start on vertex): " & to_string (intersections.last_element));
								end if;
							end if;

							
						when others => raise constraint_error; -- CS should never happen
					end case;
				end use_start_point_as_intersection;
				

				procedure use_end_point_as_intersection is 
					-- The intersection of edge A and B:
					IAB : type_intersection;
				begin
					IAB.position := element (a).segment_line.end_point;
					IAB.edge_A := element (a).segment_line;
					
					case LPS.end_point.location is
						when ON_EDGE =>
							IAB.direction := LPS.end_point.direction_on_edge;

							-- Get the touched B-edge at the end point:
							IAB.edge_B := element (LPS.end_point.edge).segment_line; 

							-- Ignore intersection if edge A runs "parallel" to edge B:
							if not lines_overlap (element (a).segment_line, IAB.edge_B) then
								-- collect intersection:
								intersections.append (IAB);

								if debug then
									put_line ("intersection (end on edge): " & to_string (intersections.last_element));
								end if;
							end if;
							
						when ON_VERTEX =>
							IAB.direction := LPS.end_point.direction_on_vertex;

							-- Get the touched B-edge that starts at the end point:
							--IAB.edge_B := element (LPS.end_point.edges.edge_2).segment_line;

							-- Get the touched B-edge that ends at the end point:
							IAB.edge_B := element (LPS.end_point.edges.edge_1).segment_line;

							-- Ignore intersection if edge A runs "parallel" to edge B:
							if not lines_overlap (element (a).segment_line, IAB.edge_B) then
								-- collect intersection:
								intersections.append (IAB);

								if debug then
									put_line ("intersection (end on vertex): " & to_string (intersections.last_element));
								end if;
							end if;

						when others => raise constraint_error; -- CS should never happen
					end case;
				end use_end_point_as_intersection;
			
				
			begin -- query_A_segment

				case LPS.start_point.location is
					when OUTSIDE =>
						case LPS.end_point.location is
							when OUTSIDE =>
								if LPS.intersections.is_empty then
									-- edge passes the polygon
									null;
								else
									-- edge runs through the polygon
									collect_intersections;
								end if;
								
							when INSIDE =>
								if LPS.intersections.is_empty then
									-- CS: should never happen
									raise constraint_error;
								else
									-- edge runs from outside to inside
									collect_intersections;
								end if;

								
							when ON_EDGE =>
								use_end_point_as_intersection;
								
								if LPS.intersections.is_empty then
									-- edge comes from outside, does not
									-- cross any edge of the polygon and ends 
									-- on edge of polygon:
									null;
								else
									-- edge comes from outside, 
									-- crosses one or more edges of the polygon and ends 
									-- on edge of polygon:
									collect_intersections;
								end if;

								
							when ON_VERTEX =>
								use_end_point_as_intersection;
								
								if LPS.intersections.is_empty then
									-- edge comes from outside, does not
									-- cross any edge of the polygon and ends 
									-- on a vertex of polygon:
									null;
								else
									-- edge comes from outside, 
									-- crosses one or more edges of the polygon and ends 
									-- on a vertex of polygon:
									collect_intersections;
								end if;
							
						end case;

						
					when INSIDE =>
						case LPS.end_point.location is
							when OUTSIDE =>
								if LPS.intersections.is_empty then
									-- CS: should never happen
									raise constraint_error;
								else
									-- edge runs from inside to outside of the polygon:
									collect_intersections;
								end if;

							when INSIDE =>
								if LPS.intersections.is_empty then
									-- edge is completely inside the polygon
									-- without touching the polygon edges or vertices:
									null;
								else
									-- edge starts and ends inside 
									collect_intersections;
								end if;

								
							when ON_EDGE =>
								use_end_point_as_intersection;
								
								if LPS.intersections.is_empty then
									-- edge starts inside and ends on an edge of the polygon
									null;
								else
									-- edge starts inside, crosses one or more edges of the polygon
									-- and ends on edge
									collect_intersections;
								end if;

								
							when ON_VERTEX =>
								use_end_point_as_intersection;
								
								if LPS.intersections.is_empty then
									-- edge starts inside and ends on a vertex of the polygon
									null; 
								else
									-- edge starts inside, crosses one or more edges of the polygon
									-- and ends on a vertex
									collect_intersections;
								end if;

						end case;

						
					when ON_EDGE =>
						use_start_point_as_intersection;
						
						case LPS.end_point.location is
							when OUTSIDE =>
								if LPS.intersections.is_empty then
									-- edge starts on edge and ends outside
									-- without crossing any edges or vertices
									null;
								else
									-- edge starts on edge, crosses one or more edges of the polygon
									-- and ends outside
									collect_intersections;
								end if;
								
							when INSIDE =>
								if LPS.intersections.is_empty then
									-- edge starts on edge and ends inside
									-- without crossing any edges or verices
									null;
								else
									-- edge starts on edge, crosses one or more edges of the polygon
									-- and ends inside
									collect_intersections;
								end if;

								
							when ON_EDGE =>
								use_end_point_as_intersection;
								
								if LPS.intersections.is_empty then
									-- edge starts on edge and ends on edge
									-- without crossing any edges or verices
									null;
								else
									-- edge starts on edge, crosses one or more edges of the polygon
									-- and ends on edge
									collect_intersections;
								end if;

								
							when ON_VERTEX =>
								use_end_point_as_intersection;
								
								if LPS.intersections.is_empty then
									-- edge starts on edge and ends on vertex
									-- without crossing any edges or verices
									null;
								else
									-- edge starts on edge, crosses one or more edges of the polygon
									-- and ends on vertex
									collect_intersections;
								end if;
						end case;

						
					when ON_VERTEX =>
						use_start_point_as_intersection;
						
						case LPS.end_point.location is
							when OUTSIDE =>
								if LPS.intersections.is_empty then
									-- edge starts on vertex and ends outside
									-- without crossing any edges or vertices
									null;
								else
									-- edge starts on vertex, crosses one or more edges of the polygon
									-- and ends outside
									collect_intersections;
								end if;
								
							when INSIDE =>
								if LPS.intersections.is_empty then
									-- edge starts on vertex and ends inside
									-- without crossing any edges or verices
									null;
								else
									-- edge starts on vertex, crosses one or more edges of the polygon
									-- and ends inside
									collect_intersections;
								end if;

								
							when ON_EDGE =>
								use_end_point_as_intersection;
								
								if LPS.intersections.is_empty then
									-- edge starts on vertex and ends on edge
									-- without crossing any edges or verices
									null;
								else
									-- edge starts on vertex, crosses one or more edges of the polygon
									-- and ends on edge
									collect_intersections;
								end if;

								
							when ON_VERTEX =>
								use_end_point_as_intersection;
								
								if LPS.intersections.is_empty then
									-- edge starts on vertex and ends on vertex
									-- without crossing any edges or verices
									null;
								else
									-- edge starts on vertex, crosses one or more edges of the polygon
									-- and ends on vertex
									collect_intersections;
								end if;
								
						end case;

				end case;
				
			end query_A_segment;

		begin
			-- Traverse the edges of polygon A:
			polygon_A.contours.segments.iterate (query_A_segment'access);

			remove_redundant_intersections;
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
		-- Intersection points can lie on the start or end point of the given line.
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

			
		begin
			intersections.iterate (query_intersection'access);
			sort_by_distance (result, edge.start_point);			
			
			return result;
		end get_intersections_on_edge;


		
		-- These are the lists of vertices and intersections
		-- in counter-clockwise order for polygon A and B:
		vertices_A, vertices_B : pac_vertices.list;
		


		-- When the start point of an edge lies on an edge of the other polygon
		-- then we got a regular vertex right AFTER an intersection. Both have 
		-- the same x/y-position. The regular vertex must be deleted 
		-- so that just the intersection is left:
		procedure delete_regular_after_intersection (
			vertices : in out pac_vertices.list)
		is
			c : pac_vertices.cursor := vertices.first;
		begin
			while c /= pac_vertices.no_element loop

				if c = vertices.first then
					if same_position (vertices.last, c) then
						vertices.delete (c);
					end if;
				else
					if same_position (previous (c), c) then
						vertices.delete (c);
					end if;
				end if;
				
				next (c);
			end loop;
		end delete_regular_after_intersection;


		-- When the start point of an edge lies on an edge of the other polygon
		-- then we got a regular vertex right BEFORE an intersection. Both have 
		-- the same x/y-position. The regular vertex must be deleted 
		-- so that just the intersection is left:
		procedure delete_regular_before_intersection (
			vertices : in out pac_vertices.list)
		is
			c : pac_vertices.cursor := vertices.first;
			c2 : pac_vertices.cursor;
		begin
			while c /= pac_vertices.no_element loop

				if c = vertices.first then
					if same_position (c, vertices.last) then
						vertices.delete_last;
					end if;
				else
					if same_position (c, previous (c)) then
						c2 := previous (c);
						vertices.delete (c2);
					end if;
				end if;
				
				next (c);
			end loop;
		end delete_regular_before_intersection;
		
		
		-- Fills container vertices_A counter-clockwise with the vertices of polygon A
		-- and the intersections (leaving or entering) with polygon B:
		procedure make_vertices_A is

			procedure query_segment (s : in pac_polygon_segments.cursor) is
				v_list : pac_vertices.list;
			begin
				-- Get the intersections (on the current edge) that follow
				-- after the start point:
				v_list := get_intersections_on_edge (element (s).segment_line, A);
				-- Function get_intersections_on_edge has updated the "intersections".

				vertices_A.append (new_item => (
					category	=> REGULAR,
					position	=> element (s).segment_line.start_point));
				
				-- Append the vertices to container vertices_A:
				splice (target => vertices_A, before => pac_vertices.no_element, source => v_list);
			end query_segment;
			
		begin
			polygon_A.contours.segments.iterate (query_segment'access);
			delete_regular_after_intersection (vertices_A);
			delete_regular_before_intersection (vertices_A);
		end make_vertices_A;
		

		-- Fills container vertices_B counter-clockwise with the vertices of polygon B
		-- and the intersections (leaving or entering) with polygon A:
		procedure make_vertices_B is

			procedure query_segment (s : in pac_polygon_segments.cursor) is
				v_list : pac_vertices.list;
			begin
				-- Get the intersections (on the current edge) that follow
				-- after the start point:
				v_list := get_intersections_on_edge (element (s).segment_line, B);

				vertices_B.append (new_item => (
					category	=> REGULAR,
					position	=> element (s).segment_line.start_point));

				-- Append the vertices to container vertices_B:
				splice (target => vertices_B, before => pac_vertices.no_element, source => v_list);
			end query_segment;
			
		begin
			polygon_B.contours.segments.iterate (query_segment'access);
			delete_regular_after_intersection (vertices_B);
			delete_regular_before_intersection (vertices_B);
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



		

		type type_overlap_status is (
			A_INSIDE_B,
			A_CLIPPED_BY_B,
			A_OUTSIDE_B);

		overlap_status : type_overlap_status;


		procedure set_overlap_status is
			real_intersections : constant pac_intersections.list := 
				get_real_intersections (intersections);
		begin
			case real_intersections.length is
				when 0 => -- no real intersections at all
					-- A is either completely inside or outside B:
					if all_vertices_of_A_inside_B (polygon_A, polygon_B) then
						overlap_status := A_INSIDE_B;
					else
						overlap_status := A_OUTSIDE_B;
					end if;
					
				when 1 => raise constraint_error; -- CS should never happen

				when others =>
					overlap_status := A_CLIPPED_BY_B;

			end case;

			if debug then
				put_line ("overlap status: " & type_overlap_status'image (overlap_status));
			end if;
		end set_overlap_status;

		

		procedure do_clipping is 

			-- This is a safety measure to prevent indefinite looping.
			-- CS: Increase upper limit if required:
			subtype type_safety_counter is natural range 0 .. 100;
			safety_counter : type_safety_counter := 0;

		begin
			-- If there are intersections then the actual work begins.
			-- Otherwise do nothing and return an empty list of polygons:
			if not is_empty (intersections) then
			
				make_vertices_A; -- MUST be called BEFORE make_vertices_B !
				
				if debug then
					new_line;
					put_line ("vertices A: " & to_string (vertices_A));
				end if;
				
				make_vertices_B;

				if debug then
					new_line;
					put_line ("vertices B: " & to_string (vertices_B));
				end if;

				-- Go to the first entering intersection in vertices_A:
				vertice_A_cursor := get_first_entering;

				if debug then
					new_line;
					put_line ("first entering: " & to_string (element (vertice_A_cursor)));
				end if;

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
							--  CS: This comment is obsolete ? Rework !
							
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
		end do_clipping;
		
		
		
	begin -- clip

		-- Both polygons must have vertices and edges.
		-- Otherwise raise exception:
		if get_segments_total (polygon_A) = 0 then
			raise constraint_error with "Polygon A has no vertices !";
		end if;

		if get_segments_total (polygon_B) = 0 then
			raise constraint_error with "Polygon B has no vertices !";
		end if;

		
		-- Find intersections of the given two polygons:
		find_intersections;


		set_overlap_status;
		case overlap_status is
			when A_OUTSIDE_B => 
				-- Nothing to do. Return an empty list:
				null; 

			when A_INSIDE_B => 
				-- Polygon A is completely inside B. So the result
				-- is just polygon A:
				result.append (type_polygon (polygon_A));

			when A_CLIPPED_BY_B => 
				-- Do the actual clipping work:
				do_clipping;
		end case;
	
	
		return result;



		--exception
			----when event: operator_error =>
				----put_line(exception_message(event));
			--when event: constraint_error =>
				----put_line ("Constraint error occured !");
				--put_line (exception_information (event));
				--put_line (exception_message (event));

				--put_line (gnat.source_info.file & " :" & integer'image (gnat.source_info.line));
				
				--raise;
				
			--when others =>
				--put_line ("Other error occured !");
				--raise;
		
	end clip;

	
end et_geometry_2.polygons.clipping;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

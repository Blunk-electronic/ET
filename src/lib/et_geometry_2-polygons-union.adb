------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      GEOMETRY 2 / POLYGONS / UNION                       --
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


with ada.exceptions; 			use ada.exceptions;
with gnat.source_info;

with et_exceptions;				use et_exceptions;


package body et_geometry_2.polygons.union is


	function get_greatest (
		polygons	: in pac_polygons.list)
		return pac_polygons.cursor
	is 
		primary : pac_polygons.cursor := polygons.first;

		select_next_primary : boolean := false;


		procedure query_secondary (secondary : pac_polygons.cursor) is 
			intersections : pac_intersections.list;
			overlap_status : type_overlap_status;			
		begin
			if primary /= secondary then
				--new_line;
				--put_line ("secondary: " & to_string (element (secondary)));
				
				intersections := get_intersections (element (primary), element (secondary));
				overlap_status := get_overlap_status (element (primary), element (secondary), intersections);

				case overlap_status is
					when CONGRUENT =>
						null; -- CS ?
						
					when A_DOES_NOT_OVERLAP_B => 
						raise constraint_error; -- CS more details ?

					when A_INSIDE_B => 
						--put_line ("A inside");
						-- Primary polygon is completely inside secondary.
						select_next_primary := true;

					when B_INSIDE_A =>
						--put_line ("B inside");
						-- Secondary polygon is completely inside primary.
						null;

					when A_OVERLAPS_B => 
						raise constraint_error; -- CS more details ?
				end case;

			end if;
		end query_secondary;

		
	begin
		while primary /= pac_polygons.no_element loop

			--new_line;
			--put_line ("primary: " & to_string (element (primary)));

			polygons.iterate (query_secondary'access);

			if select_next_primary then
				next (primary);
				select_next_primary := false;
			else
				exit;
			end if;
		end loop;
		
		--greatest := polygons.first;
		return primary;
	end get_greatest;

	
	
	function union (
		polygon_A	: in type_polygon;
		polygon_B	: in type_polygon;
		debug		: in boolean := false)
		return type_union
	is
		result_exists : boolean := true;
		result_polygon : type_polygon;

		-- The list of intersecting A and B edges:
		intersections : pac_intersections.list;

		
		-- This procedure does the actual union work:
		procedure do_union is

			-- These are the lists of vertices and intersections
			-- in counter-clockwise order for polygon A and B:
			vertices_A, vertices_B : pac_vertices.list;
			
			vertice_A_cursor : pac_vertices.cursor;
			outside_vertices_A : pac_vertices.list;
			
			-- Temporarily collections of vertices and intersections:
			vertices_tmp_1 : pac_vertices.list; -- primary collection
			vertices_tmp_2 : pac_vertices.list; -- secondary collection
			
			-- Appends the intersections and vertices of the secondary
			-- collection to the primary collection.
			-- The secondary collection will be emptied.
			procedure append_secondary is begin
				splice (
					target	=> vertices_tmp_1, -- primary
					before	=> pac_vertices.no_element, 
					source 	=> vertices_tmp_2); -- will be emptied

			end append_secondary;

			-- The start point when walking along the vertices_A.
			-- In the course of the union process we eventually get back
			-- to this point. The process is then complete:
			start_point : type_vector;

			-- This flag goes true as soon as the start point is
			-- approaching:
			expect_return_to_start : boolean := false;

			v_cursor_tmp : pac_vertices.cursor;
			
			-- There are two methods to do the union process.
			-- The method is choosen by the fact that polygon A
			-- has or does not has a vertex which is outside polygon B.
			outside_vertex_found : boolean;

			-- This is a safety measure to prevent indefinite looping.
			-- CS: Increase upper limit if required:
			safety_counter_limit : constant natural := 100;
			safety_counter : natural := 0;

			-- If there is at least one outside vertex then this method is applied.
			-- In general the search starts at one selected outside vertex. But
			-- there can be many outside vertices. It can NOT be assumed that the outcome
			-- will be the same whatever outside vertex is choosen as starting point.
			-- So we will be collecting the results in a list of candidate polygons
			-- and select later the greatest among them, that is the one that encloses
			-- all the other candidates:
			procedure walk_1 is 

				candidate_polygons : pac_polygons.list;
				
				procedure query_outside_vertex (ov : pac_vertices.cursor) is begin
					-- Set the start point.
					-- When walking along the
					-- edges of polygon A we will eventually get back to 
					-- the start point. The polygon is then complete.
					start_point := element (ov).position;
					vertice_A_cursor := vertices_A.find (element (ov));

					safety_counter := 0;
					vertices_tmp_1.clear;
					vertices_tmp_2.clear;
					expect_return_to_start := false;
					
					WALK_METHOD_1:
					loop
						-- safety measure to prevent forever-looping:
						increment_safety_counter (safety_counter, safety_counter_limit);
						
						-- Walk along the vertices (and intersections) of polygon A until
						-- the next entering intersection:
						vertices_tmp_2 := get_until (
							vertices					=> vertices_A,
							start_vertex				=> vertice_A_cursor,
							direction_of_intersection	=> ENTERING,
							direction_of_search			=> CCW,
							delete_visited				=> false);

						-- If this loop is executed the first time we do not expect
						-- the start point and just append the secondary collection to
						-- the primary collection.
						-- For following passes we iterate the secondary collection
						-- and copy the vertices into the primary collection:
						if expect_return_to_start then
							v_cursor_tmp := vertices_tmp_2.first;
							while v_cursor_tmp /= pac_vertices.no_element loop

								-- Copy vertex to primary vertices collection:
								vertices_tmp_1.append (element (v_cursor_tmp));

								-- Terminate the outer loop "walk" once the start
								-- point has been detected:
								if element (v_cursor_tmp).position = start_point then								
									exit WALK_METHOD_1;
								end if;
								
								next (v_cursor_tmp);
							end loop;

						else
							append_secondary;
						end if;				
						
						-- Walk along the vertices (and intersections) of polygon B until
						-- the next leaving intersection:
						vertices_tmp_2 := get_until (
							vertices					=> vertices_B,
							start_vertex				=> vertices_B.find (vertices_tmp_1.last_element),
							direction_of_intersection	=> LEAVING,
							direction_of_search			=> CCW,
							delete_visited				=> false);

						append_secondary;

						-- Set the vertice_A_cursor to the last vertex of the primary collection:
						vertice_A_cursor := vertices_A.find (vertices_tmp_1.last_element);

						expect_return_to_start := true;
					end loop WALK_METHOD_1;
				
					-- Make a polygon from the primary collection of vertices
					-- and append it to the candidate list of polygons:
					candidate_polygons.append (type_polygon (to_polygon (vertices_tmp_1)));

				end query_outside_vertex;
				
			begin
				-- Union a union for each candidate outside vertex:
				outside_vertices_A.iterate (query_outside_vertex'access);

				-- Get the greatest polygon from the resulting candidate polygons:
				result_polygon := element (get_greatest (candidate_polygons));
			end walk_1;

			
			-- If there are no outside vertices then this method is applied:
			procedure walk_2 is begin

				-- Set the start point.
				-- When walking along the
				-- edges of polygon A we will eventually get back to 
				-- the start point. The polygon is then complete.
				start_point := element (vertice_A_cursor).position;
				
				WALK_METHOD_2:
				loop
					-- safety measure to prevent forever-looping:
					increment_safety_counter (safety_counter, safety_counter_limit);
					
					-- Walk along the vertices (and intersections) of polygon A until
					-- the next entering intersection:
					vertices_tmp_2 := get_until (
						vertices					=> vertices_A,
						start_vertex				=> vertice_A_cursor,
						direction_of_intersection	=> ENTERING,
						direction_of_search			=> CCW,
						delete_visited				=> false);

					append_secondary;
					
					-- Walk along the vertices (and intersections) of polygon B until
					-- the next leaving intersection:
					vertices_tmp_2 := get_until (
						vertices					=> vertices_B,
						start_vertex				=> vertices_B.find (vertices_tmp_1.last_element),
						direction_of_intersection	=> LEAVING,
						direction_of_search			=> CCW,
						delete_visited				=> false);

					append_secondary;

					-- Set the vertice_A_cursor to the last vertex of the primary collection:
					vertice_A_cursor := vertices_A.find (vertices_tmp_1.last_element);

					-- If the last element of the primary collection is the start point
					-- then the resulting polygon is complete:
					if element (vertice_A_cursor).position = start_point then
						-- Make a polygon from the primary collection of vertices:
						result_polygon := type_polygon (to_polygon (vertices_tmp_1));
												
						exit WALK_METHOD_2; -- Terminate this loop:
					end if;
					
				end loop WALK_METHOD_2;
			end walk_2;

			
		begin
			-- Make the vertices and intersection nodes of polygon A:
			vertices_A := get_vertices (polygon_A, polygon_B, intersections, A);
			
			if debug then
				new_line;
				put_line ("vertices A: " & to_string (vertices_A));
			end if;

			-- Make the vertices and intersection nodes of polygon B:
			vertices_B := get_vertices (polygon_B, polygon_A, intersections, B);

			if debug then
				new_line;
				put_line ("vertices B: " & to_string (vertices_B));
			end if;

			-- Search for the first OUTSIDE vertex in vertices_A:
			vertice_A_cursor := get_first (OUTSIDE, vertices_A);

			-- Here we decide which method to should be applied in order to
			-- do the union process.
			-- The flag outside_vertex_found is set or cleared:
			if vertice_A_cursor = pac_vertices.no_element then

				-- No outside vertex found.
				outside_vertex_found := false;
				
				if debug then
					new_line;
					put_line ("No vertex of A outside B found.");
				end if;

				-- Find the first leaving vertex instead:
				vertice_A_cursor := get_first (LEAVING, vertices_A);

				if debug then
					new_line;
					put_line ("First leaving vertex of A: " & to_string (element (vertice_A_cursor)));
				end if;

			else
				-- At least one outside vertex exists:
				outside_vertex_found := true;
				
				outside_vertices_A := get_vertices (OUTSIDE, vertices_A);
				
				if debug then
					new_line;
					put_line ("first outside: " & to_string (element (vertice_A_cursor)));
					--put_line ("outside vertices: " & to_string (outside_vertices_A));
				end if;

			end if;


			if outside_vertex_found then
				walk_1;
			else
				walk_2;
			end if;
			
		end do_union;

		
		overlap_status : type_overlap_status;
		
		
	begin -- clip

		-- Both polygons must have vertices and edges.
		-- Otherwise raise exception:
		if get_edges_total (polygon_A) = 0 then
			raise constraint_error with "Polygon A has no vertices !";
		end if;

		if get_edges_total (polygon_B) = 0 then
			raise constraint_error with "Polygon B has no vertices !";
		end if;

		
		-- Find intersections of the given two polygons:
		intersections := get_intersections (polygon_A, polygon_B, debug);


		overlap_status := get_overlap_status (polygon_A, polygon_B, intersections);
		
		case overlap_status is
			when CONGRUENT =>
				put_line ("CONGRUENT");
				-- The result is just polygon A:
				result_polygon := type_polygon (polygon_A);
				
			when A_DOES_NOT_OVERLAP_B => 
				-- No union possible.
				result_exists := false;

			when A_INSIDE_B => 
				-- Polygon A is completely inside B. So the result
				-- is just polygon B:
				result_polygon := type_polygon (polygon_B);

			when B_INSIDE_A =>
				-- Polygon B is completely inside A. So the result
				-- is just polygon A:
				result_polygon := type_polygon (polygon_A);

			when A_OVERLAPS_B => 
				-- Do the actual union work:
				do_union;
		end case;
	

		if result_exists then
			return (exists => true, union => result_polygon);

		else
			return (exists => false);
		end if;


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
		
	end union;

	
end et_geometry_2.polygons.union;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
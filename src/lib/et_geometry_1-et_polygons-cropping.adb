------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      GEOMETRY 2 / POLYGONS / CROPPING                    --
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


with ada.strings.unbounded;
with ada.characters.latin_1;

with ada.exceptions; 			use ada.exceptions;
with gnat.source_info;

with et_exceptions;				use et_exceptions;


package body et_geometry_1.et_polygons.cropping is


	function "=" (
		left, right : in type_crop)
		return boolean
	is
		result : boolean := true;

		procedure search_left_in_right (c_left : pac_polygon_list.cursor) is
			c_right : pac_polygon_list.cursor := right.fragments.first;
			found : boolean := false;
		begin
			while c_right /= pac_polygon_list.no_element loop
				
				if are_congruent (element (c_left), element (c_right)) then
					found := true;
					exit;
				end if;
				
				next (c_right);
			end loop;

			if not found then
				result := false;
				-- CS signal the iterator to abort
			end if;			
		end search_left_in_right;

		
	begin
		--put_line ("CROP COMPARE");

		if not left.exists and not right.exists then
			goto none_exists;
		end if;
		
		
		-- Test whether both operands provide a cropped area:
		if left.exists and right.exists then

			if left.status = right.status then

				-- Compare the number of cropped areas:
				if left.count = right.count then
				
					-- Double check: Compare the number of cropped areas:
					if length (left.fragments) = length (right.fragments) then

						-- Search the left cropped areas in the right:
						left.fragments.iterate (search_left_in_right'access);
						-- CS abort iteration as soon as the first left 
						-- has not been found in right.
					else
						result := false;
					end if;

				else
					result := false;
				end if;
				
			else
				result := false;
			end if;
			
		else
			result := false;
		end if;			

		<<none_exists>>
		
		return result;
	end "=";



	function to_string (cr : in type_crop) return string is
		use ada.strings.unbounded;
		use ada.characters.latin_1;
		
		result : unbounded_string;

		procedure query_fragment (f : in pac_polygon_list.cursor) is begin
			result := result & LF & to_string (element (f));
		end query_fragment;
		
	begin
		result := result & type_overlap_status'image (cr.status);

		if cr.exists then
			result := result & LF & "fragment count:" & count_type'image (cr.count);
			cr.fragments.iterate (query_fragment'access);
		end if;

		return to_string (result);
	end to_string;

	
	
	function crop (
		polygon_A	: in type_polygon;
		polygon_B	: in type_polygon;
		debug		: in boolean := false)
		return type_crop
	is
		result_exists : boolean := false;
		result_crop : pac_polygon_list.list;

		-- The list of intersecting A and B edges:		
		intersections : pac_intersections.list;
		
		
		-- This procedure does the actual cropping work:
		procedure do_cropping is 

			-- These are the lists of vertices and intersections
			-- in counter-clockwise order for polygon A and B:
			vertices_A, vertices_B : pac_vertices.list;
					
			vertice_B_cursor : pac_vertices.cursor;

			-- Temporarily collections of vertices and intersections:
			vertices_tmp_1 : pac_vertices.list; -- primary collection
			vertices_tmp_2 : pac_vertices.list; -- secondary collection
			
			-- The start point when walking along the vertices_B is
			-- always a LEAVING intersection:
			v_start : type_vertex (category => INTERSECTION);
			
			-- This is a safety measure to prevent indefinite looping.
			-- CS: Increase upper limit if required:
			safety_counter_limit : constant type_safety_count := 1000;
			safety_counter : type_safety_count := 0;

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

			-- Go to the first leaving intersection in vertices_B:
			vertice_B_cursor := get_first (LEAVING, vertices_B);

			if debug then
				new_line;
				put_line ("First leaving vertex of B: " & to_string (element (vertice_B_cursor)));
			end if;

			-- Traverse vertices_B until no more leaving vertex
			-- can be found:
			while vertice_B_cursor /= pac_vertices.no_element loop
				--put_line ("A");
				
				-- A sub-polygon starts at v_start. When walking along the
				-- edges of polygon A or B we will eventually get back to 
				-- the start point v_start. The current sub-polygon is then complete.
				v_start := element (vertice_B_cursor);
				
				-- Walk along the vertices (and intersections) of polygon A until
				-- an entering intersection:
				vertices_tmp_1 := get_until (
					vertices					=> vertices_A,
					start_vertex				=> vertices_A.find (element (vertice_B_cursor)),
					direction_of_intersection	=> ENTERING,
					direction_of_search			=> CW,
					delete_visited				=> false);

				-- Now we have the intersections and vertices from after the start point 
				-- to (and including) the entering intersection E.

				--put_line ("B");
				
				-- Find the very entering intersection E in polygon B and walk
				-- along the vertices (and intersections) of polygon B until
				-- a leaving intersection:
				vertices_tmp_2 := get_until (
					vertices					=> vertices_B,
					start_vertex				=> vertices_B.find (vertices_tmp_1.last_element),
					direction_of_intersection	=> LEAVING,
					direction_of_search			=> CCW);

				--put_line ("C");
				
				loop
					--put_line ("C1");
					
					-- safety measure to prevent forever-looping:
					increment_safety_counter (safety_counter, safety_counter_limit);					

					--put_line ("C2");
					
					-- Splice the intersections and vertices of A and B.
					-- Collect everything in the primary collection:
					splice (
						target	=> vertices_tmp_1, -- primary
						before	=> pac_vertices.no_element, 
						source 	=> vertices_tmp_2); -- will be emptied

					--put_line ("C3");
					
					-- If we have reached the start point then the sub-polygon
					-- is complete.
					if last_element (vertices_tmp_1) = v_start then
						exit;
					else
						--put_line ("C4");
						
						-- If sub-polygon is not complete, then again go to the first
						-- leaving intersection of polygon B:
						--vertice_B_cursor := get_first (LEAVING, vertices_B);

						-- Get the intersections and vertices until
						-- an entering intersection in polygon A:
						vertices_tmp_2 := get_until (
							vertices					=> vertices_A,
							--start_vertex				=> vertices_A.find (element (vertice_B_cursor)),
							start_vertex				=> vertices_A.find (vertices_tmp_1.last_element),
							direction_of_intersection	=> ENTERING,
							direction_of_search			=> CW,
							delete_visited				=> false);

						--put_line ("C5");
						
						
						-- Append the intersection (and vertices) to the primary
						-- collection:
						splice (
							target	=> vertices_tmp_1, -- primary
							before	=> pac_vertices.no_element, 
							source 	=> vertices_tmp_2); -- will be emtied

						--put_line ("D");
						
						-- Switch to polygon B and get intersections
						-- until (and including) a leaving intersection
						-- into the secondary collection. The secondary collection
						-- will be appended to the primary one once this loop
						-- starts again:
						vertices_tmp_2 := get_until (
							vertices					=> vertices_B,
							start_vertex				=> vertices_B.find (vertices_tmp_1.last_element),
							direction_of_intersection	=> LEAVING,
							direction_of_search			=> CCW);

						--put_line ("E");
					end if;
				end loop;

				--put_line ("F");
				--put_line (to_string (to_polygon (vertices_tmp_1)));

				-- Iron out useless vertices and append the 
				-- sub-polygon to the result:
				result_crop.append (optimize_edges (to_polygon (vertices_tmp_1), debug));

				--put_line ("G");
				
				-- Get the next leaving vertex from vertices_B.
				-- In case there is no leaving vertex any more, then this
				-- pass will be the last (see head of this loop):
				vertice_B_cursor := get_first (LEAVING, vertices_B);

				--put_line ("H");
				
				-- We also abort the loop if there are no more entering
				-- vertices in vertices_B (happens in rare cases):
				if get_first (ENTERING, vertices_B) = pac_vertices.no_element then
					exit;
				end if;
				
			end loop;

		end do_cropping;
		
		

		overlap_status : type_overlap_status;

		procedure show_overlap_status is begin
			if debug then
				put_line ("OVERLAP STATUS: " & type_overlap_status'image (overlap_status));
			end if;
		end;

		
	begin -- crop
		--put_line ("crop");
		
		-- To speed things up, we do a simple pre-test.
		-- If the boundaries of the given polygons overlap each other
		-- then cropping makes sense.:
		if overlap (get_boundaries (polygon_A), get_boundaries (polygon_B)) then

			--put_line ("overlap");
			
			-- Find intersections of the given two polygons:
			intersections := get_intersections (polygon_A, polygon_B, debug);


			overlap_status := get_overlap_status (polygon_A, polygon_B, intersections);
			show_overlap_status;
			
			case overlap_status is
				when CONGRUENT =>
					-- Both polygons have the same outline. B is completely cropped to zero area.
					-- So the result is an empty list of polygons:
					result_exists := true;
					
				when A_DOES_NOT_OVERLAP_B => 
					-- Nothing to do. Polygon B is unchanged so it is the one and only polygon
					-- to be returned:
					result_exists := true;
					result_crop.append (polygon_B);

				when A_INSIDE_B => 
					-- Polygon A is completely inside B. A crop operation is
					-- not possible (The outcome would be a cutout area in polygon B.):
					result_exists := false;

				when B_INSIDE_A => 
					-- Polygon B is completely inside A. B is completey cropped to zero area
					-- So the result is an empty list of polygons:
					result_exists := true;
					
				when A_OVERLAPS_B => 
					result_exists := true;
					-- Do the actual cropping work:
					do_cropping;
			end case;

		else
			result_exists := true;
			overlap_status := A_DOES_NOT_OVERLAP_B;
			result_crop.append (polygon_B);
		end if;	

			
		
		if result_exists then
			return (
				exists		=> TRUE, 
				status		=> overlap_status, 
				fragments	=> result_crop, 
				count		=> result_crop.length);
			
		else
			return (
				exists	=> FALSE, 
				status	=> overlap_status);
		end if;


		
		-- Convert the polygon specific exception to a constraint error:
		exception when event: others =>
			--put_line (exception_name (event) & " " & exception_message (event));
		
			raise constraint_error with 
				exception_name (event) & " " & exception_message (event);
		
	end crop;


	

	function multi_crop_1 (
		polygon_B		: in type_polygon; -- the cropped polygon / zu bescheidendes Polygon
		polygon_A_list	: in pac_polygon_list.list; -- the cropping polygons
		debug			: in boolean := false)
		return pac_polygon_list.list
	is
		result : pac_polygon_list.list;

		P_init : type_polygon := polygon_B;
		
		
		procedure query_polygon_A (a : pac_polygon_list.cursor) is
			intersections : constant pac_intersections.list := get_intersections (
				polygon_A		=> element (a), 
				polygon_B		=> P_init,
				debug			=> false);
								
			ol_sts : constant type_overlap_status := get_overlap_status (
				polygon_A		=> element (a), -- polygon A is cropping !
				polygon_B		=> P_init,
				intersections	=> intersections);

		begin
			case ol_sts is
				when A_OVERLAPS_B =>
					declare
						cr : constant type_crop := crop (
							polygon_A => element (a), -- polygon A is cropping !
							polygon_B => P_init);
					begin
						if cr.count = 1 then
							P_init := cr.fragments.first_element;
						end if;
					end;

				when others => null;					
			end case;
		end query_polygon_A;


		P_list : pac_polygon_list.list;
		P_cursor : pac_polygon_list.cursor;

		L_list : pac_polygon_list.list renames polygon_A_list;
		L_cursor : pac_polygon_list.cursor;

		restart : boolean := false;
		
	begin -- multi_crop_1
		
		-- Process those A-polygons that DO NOT cause fragmentation
		-- of polygon B. The B-polygon simply looses area:
		polygon_A_list.iterate (query_polygon_A'access);

		-- Insert the B-polygon in the P_list:
		P_list.append (P_init);

		-- Process those A-polygons that DO cause fragmentation
		-- of polygon B:
		P_cursor := P_list.first;
		while P_cursor /= pac_polygon_list.no_element loop

			L_cursor := L_list.first;
			while L_cursor /= pac_polygon_list.no_element loop
				declare
					cr : type_crop := crop (element (L_cursor), element (P_cursor));
				begin
					if cr.status = A_OVERLAPS_B then
						if cr.count > 1 then -- fragments have developed

							splice (
								target	=> P_list,
								before	=> P_cursor,
								source	=> cr.fragments);

							P_list.delete (P_cursor);

							restart := true;
							exit;
						else
							-- no fragments
							restart := false;
						end if;
					end if;
				end;
				
				next (L_cursor);
			end loop;

			if restart then
				P_cursor := P_list.first;
			else
				next (P_cursor);
			end if;
			
		end loop;
		

		return P_list;
	end multi_crop_1;

	
	function multi_crop_2 (
		polygon_B_list	: in pac_polygon_list.list; -- the cropped polygons / zu bescheidende Polygone
		polygon_A_list	: in pac_polygon_list.list; -- the cropping polygons
		debug			: in boolean := false)
		return pac_polygon_list.list
	is
		result : pac_polygon_list.list;

		procedure query_polygon_B (polygon_B : in pac_polygon_list.cursor) is
			-- Crop the candidate B-polygon by the list of A-polygons:
			scratch : pac_polygon_list.list;
		begin
			if debug then
				put_line ("query B");
			end if;
			
			scratch := multi_crop_1 (
				polygon_B		=> element (polygon_B), 
				polygon_A_list	=> polygon_A_list,
				debug			=> debug);

			if debug then
				put_line ("scratch length" & count_type'image (scratch.length));
			end if;
			
			-- Append the resulting islands to the return value:
			splice (
				target	=> result,
				source	=> scratch,
				before	=> pac_polygon_list.no_element);

			if debug then
				put_line ("result length" & count_type'image (result.length));
			end if;
		end query_polygon_B;

		
	begin
		if debug then
			put_line ("multi_crop_2");
			put_line ("B ct:" & count_type'image (polygon_B_list.length));
			put_line ("A ct:" & count_type'image (polygon_A_list.length));
		end if;

		
		if not is_empty (polygon_A_list) then
		
			-- Iterate the B-polygons and crop each of them by all A-polygons.
			-- Append the resulting islands to the return value:
			polygon_B_list.iterate (query_polygon_B'access);
		else
			result := polygon_B_list;
		end if;
		
		return result;
	end multi_crop_2;
	

	
	
end et_geometry_1.et_polygons.cropping;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

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
	
	
	function clip (
		polygon_A	: in type_polygon'class;
		polygon_B	: in type_polygon'class;
		debug		: in boolean := false)
		return pac_clipped.list
	is
		result : pac_clipped.list;

		-- The list of intersecting A and B edges:		
		intersections : pac_intersections.list;

		
		-- These are the lists of vertices and intersections
		-- in counter-clockwise order for polygon A and B:
		vertices_A, vertices_B : pac_vertices.list;
		
		
		vertice_A_cursor : pac_vertices.cursor;


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



		
		

		procedure do_clipping is 

			-- This is a safety measure to prevent indefinite looping.
			-- CS: Increase upper limit if required:
			subtype type_safety_counter is natural range 0 .. 100;
			safety_counter : type_safety_counter := 0;

		begin
			-- If there are intersections then the actual work begins.
			-- Otherwise do nothing and return an empty list of polygons:
			if not is_empty (intersections) then
			
				vertices_A := get_vertices (polygon_A, intersections, A);
				
				if debug then
					new_line;
					put_line ("vertices A: " & to_string (vertices_A));
				end if;
				
				vertices_B := get_vertices (polygon_B, intersections, B);

				if debug then
					new_line;
					put_line ("vertices B: " & to_string (vertices_B));
				end if;

				-- Go to the first entering intersection in vertices_A:
				vertice_A_cursor := get_first (ENTERING, vertices_A);

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
							vertice_A_cursor := get_first (ENTERING, vertices_A);

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
					result.append (type_polygon (to_polygon (vertices_tmp_1)));
					--put_line (to_string (to_polygon (vertices_tmp_1)));

					-- Get the next entering vertex from vertices_A.
					-- In case there is no entering vertex any more, then this
					-- loop will be the last:
					vertice_A_cursor := get_first (ENTERING, vertices_A);
				end loop;

			end if;
		end do_clipping;
		
		

		overlap_status : type_overlap_status;
		
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
		intersections := get_intersections (polygon_A, polygon_B, debug);


		overlap_status := get_overlap_status (polygon_A, polygon_B, intersections);
		
		case overlap_status is
			when A_DOES_NOT_OVERLAP_B => 
				-- Nothing to do. Return an empty list:
				null; 

			when A_INSIDE_B => 
				-- Polygon A is completely inside B. So the result
				-- is just polygon A:
				result.append (type_polygon (polygon_A));

			when B_INSIDE_A => 
				-- Polygon B is completely inside A. So the result
				-- is just polygon B:
				result.append (type_polygon (polygon_B));
				
			when A_OVERLAPS_B => 
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

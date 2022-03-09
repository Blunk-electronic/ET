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


with ada.strings;				use ada.strings;
with ada.strings.fixed;			use ada.strings.fixed;
with ada.strings.unbounded;
with ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;

with ada.exceptions; 			use ada.exceptions;
with gnat.source_info;

with et_exceptions;				use et_exceptions;


package body et_geometry_2.polygons.union is

	use pac_polygon_segments;
	
	
	function union (
		polygon_A	: in type_polygon'class;
		polygon_B	: in type_polygon'class;
		debug		: in boolean := false)
		return type_union
	is
		result_exists : boolean := true;
		result_polygon : type_polygon;

		-- The list of intersecting A and B edges:
		intersections : pac_intersections.list;


		
		-- These are the lists of vertices and intersections
		-- in counter-clockwise order for polygon A and B:
		vertices_A, vertices_B : pac_vertices.list;
		


		
		vertice_A_cursor : pac_vertices.cursor;

		
		-- Temporarily collections of vertices and intersections:
		vertices_tmp_1 : pac_vertices.list; -- primary collection
		vertices_tmp_2 : pac_vertices.list; -- secondary collection
		
		-- The start point when walking along the vertices_A is
		-- a REGULAR vertex OUTSIDE polygon B:
		v_start : type_vertex (category => REGULAR);

		

		procedure do_union is 
			expect_return_to_start : boolean := false;
			v_cursor : pac_vertices.cursor;
		begin
			vertices_A := get_vertices (polygon_A, polygon_B, intersections, A);
			
			if debug then
				new_line;
				put_line ("vertices A: " & to_string (vertices_A));
			end if;
			
			vertices_B := get_vertices (polygon_B, polygon_A, intersections, B);

			if debug then
				new_line;
				put_line ("vertices B: " & to_string (vertices_B));
			end if;

			-- Go to the first OUTSIDE vertex in vertices_A:
			vertice_A_cursor := get_first (OUTSIDE, vertices_A);

			if vertice_A_cursor = pac_vertices.no_element then
				result_polygon := type_polygon (polygon_B);

				new_line;
				put_line ("no outside vertex found");
			else
								
				if debug then
					new_line;
					put_line ("first outside: " & to_string (element (vertice_A_cursor)));
				end if;

				-- When walking along the
				-- edges of polygon A we will eventually get back to 
				-- the start point v_start. The polygon is then complete.
				v_start := element (vertice_A_cursor);

				walk:
				loop -- CS safety counter
					--put_line ("W");
					
					-- Walk along the vertices (and intersections) of polygon A until
					-- an entering intersection:
					-- Now we have the intersections and vertices from after the start point 
					-- to (and including) the entering intersection E.
					vertices_tmp_2 := get_until (
						vertices					=> vertices_A,
						start_vertex				=> vertice_A_cursor,
						direction_of_intersection	=> ENTERING,
						direction_of_search			=> CCW,
						delete_visited				=> false);

					
					if expect_return_to_start then
											
						v_cursor := vertices_tmp_2.first;

						while v_cursor /= pac_vertices.no_element loop
							vertices_tmp_1.append (element (v_cursor));
							if element (v_cursor) = v_start then
								exit walk;
							end if;
							next (v_cursor);
						end loop;

					else
						-- Splice the intersections and vertices of A and B.
						-- Collect everything in the primary collection:
						splice (
							target	=> vertices_tmp_1, -- primary
							before	=> pac_vertices.no_element, 
							source 	=> vertices_tmp_2); -- will be emptied

					end if;				
					
					-- Find the very entering intersection E in polygon B and walk
					-- along the vertices (and intersections) of polygon B until
					-- a leaving intersection:
					vertices_tmp_2 := get_until (
						vertices					=> vertices_B,
						start_vertex				=> vertices_B.find (vertices_tmp_1.last_element),
						direction_of_intersection	=> LEAVING,
						direction_of_search			=> CCW,
						delete_visited				=> false);
						
					-- Splice the intersections and vertices of A and B.
					-- Collect everything in the primary collection:
					splice (
						target	=> vertices_tmp_1, -- primary
						before	=> pac_vertices.no_element, 
						source 	=> vertices_tmp_2); -- will be emptied

					vertice_A_cursor := vertices_A.find (vertices_tmp_1.last_element);

					expect_return_to_start := true;

				end loop walk;
			
				-- Make a polygon from the primary collection of vertices:
				result_polygon := type_polygon (to_polygon (vertices_tmp_1));
				--put_line ("Polygon C: " & to_string (result_polygon));

			end if;
			
		end do_union;

		
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

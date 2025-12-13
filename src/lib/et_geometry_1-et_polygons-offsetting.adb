------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                     GEOMETRY 1 / POLYGONS / OFFSETTING                   --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2025                                                --
-- Mario Blunk / Blunk electronic                                           --
-- Buchfinkenweg 3 / 99097 Erfurt / Germany                                 --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
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


package body et_geometry_1.et_polygons.offsetting is


	function to_string (oe : in type_offset_edge) return string is begin
		return to_string (oe.edge);
			--& " line: " & to_string (oe.line);
	end to_string;


	function to_string (oe : in pac_offset_edges.cursor) return string is begin
		return to_string (element (oe));
	end to_string;



	
	

	function offset_edge (
		edge	: in type_edge;
		offset	: in type_float_positive;
		mode	: in type_mode)
		return type_offset_edge
	is
		edge_new : type_edge := edge;
		edge_direction : constant type_angle := get_direction (edge);
	begin
		--put_line ("edge direction:" & to_string (edge_direction));
		
		case mode is
			when EXPAND =>
				-- Move edge to the right (outside):
				move_by (edge_new, add (edge_direction, -90.0), offset);
				
			when SHRINK =>
				-- Move the edge to the left (inside):
				move_by (edge_new, add (edge_direction, +90.0), offset);
				
			when NOTHING =>
				raise constraint_error; -- should never happen
		end case;


		return (
			edge => edge_new,
			line => to_line_vector (edge_new));
	end offset_edge;



	
	

	procedure offset_polygon (
		polygon			: in out type_polygon;
		offset			: in type_float;
		log_threshold	: in type_log_level)
	is
		offset_float : constant type_float_positive := abs (offset);
		
		-- Mode tells whether we are shrinking, expanding
		-- or whether there is nothing to do:
		mode : constant type_mode := to_mode (offset);	
	

		-- After preprocessing the offset edges are collected here:
		offset_edges : pac_offset_edges.list;
		



		-- Preprocessing the polygon edges.
		-- For each edge an "offset edge" is computed and stored
		-- in list offset_edges:
		procedure preprocess_edges is
			
			-- This procedure computes an "offset edge" from the cursor to 
			-- an original edge and stores the offset edge in list "offset_edges":
			procedure query_edge (c : in pac_edges.cursor) is
				edge : type_edge renames element (c);
				OE : type_offset_edge;
			begin
				--put_line ("original edge: " & to_string (element (c)));
				OE := offset_edge (edge, offset_float, mode);
				offset_edges.append (OE); -- CS pass OE inline
			end query_edge;
			
		begin
			log (text => "preproces edges", level => log_threshold + 1);
			log_indentation_up;
			polygon.edges.iterate (query_edge'access);
			log_indentation_down;
		end preprocess_edges;


		
		
		-- This procedure takes a cursor to an "offset edge" and 
		-- computes the intersection of its infinite line with the 
		-- next infinite line:
		function get_intersection_with_next_edge (
			cp : in pac_offset_edges.cursor) 
			return type_vector
		is
			result : type_vector; 

			-- cp is the primary cursor that points to the current edge.
			-- The secondary cursor that points to the edge that comes
			-- after the candidate edge:
			cs : pac_offset_edges.cursor;


			-- The assumption is that the two edges do
			-- intersect somewhere. Otherwise an exception will
			-- be raised:
			procedure compute_intersection is
				lp : type_line_vector renames element (cp).line;
				ls : type_line_vector renames element (cs).line;
			begin
				result := get_intersection (lp, ls);

				-- CS exception hanlder
-- 						log (text => "lines overlap", level => log_threshold + 2);
-- 						log (text => "lp: " & to_string (lp), level => log_threshold + 2);
-- 						log (text => "ls: " & to_string (ls), level => log_threshold + 2);
			end;

			
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

			compute_intersection;
			
			return result;
		end get_intersection_with_next_edge;



		
		-- Looks for a direct intersection after the edge "start"
		-- in counter-clockwise direction. If there is a direct
		-- intersection then the cursor to the corresponding "offset edge"
		-- along with the location vector of the actual intersection is returned. 
		-- If no direct intersection found, returns cursor no_element. The returned
		-- location vector is then irrelevant:
		function get_next_direct_intersection (
			start	: in pac_offset_edges.cursor;
			first	: in pac_offset_edges.cursor)
			return type_next_intersection
		is
			result : type_next_intersection;

			c : pac_offset_edges.cursor := next (start);

			premature_abort : boolean := false;
		begin
			--put_line ("start " & to_string (start));

			--goto skip;

			while c /= pac_offset_edges.no_element loop

				if start = offset_edges.first 
				and c = offset_edges.last
				then
					premature_abort := true;
					exit;
				end if;

				
				declare
					I : constant type_line_vector_intersection := get_intersection (
						line_1 => element (start).edge,
						line_2 => element (c).edge);
				begin
					if I.status = EXISTS then
						--put_line ("EXISTS");
						
						result.cursor := c;
						result.place := I.intersection;

						--put_line ("intersects " 
							--& to_string (result.cursor) 
							--& " at " & to_string (result.place));
						
						exit;
					end if;
				end;
				
				next (c);
			end loop;

		--<<skip>>

		if premature_abort then
			goto skip2;
		end if;
		
			
			--if c = pac_offset_edges.no_element then
			if result.cursor = pac_offset_edges.no_element then
				--put_line ("nothing");
				
				c := first;

				while c /= previous (start) loop
					
					declare
						I : constant type_line_vector_intersection := get_intersection (
							line_1 => element (start).edge,
							line_2 => element (c).edge);
					begin
						if I.status = EXISTS then
							--put_line ("EXISTS");
							
							result.cursor := c;
							result.place := I.intersection;
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
	


		-- Here we store the indirect and direct intersections as a result of STEP 2:
		use pac_edge_intersections;
		intersections : pac_edge_intersections.list;

		
		
		-- Traverse through the list "offset_edges" and finds intersections between them.
		-- Fills the list "intersections".
		-- For each "offset edge" the next indirect intersection is computed. An indirect
		-- intersection DOES ALWAYS exist. In contrast, a direct intersection MAY exist.
		procedure find_intersections is
			-- This cursor points to the candidate "offset_edge":
			OE : pac_offset_edges.cursor := offset_edges.first;

			-- This variable may point to the next direct intersection.
			-- If no direct intersection exists, then it points to no_element:
			N : type_next_intersection;
		begin	
			log (text => "find_intersections", level => log_threshold + 1);
			log_indentation_up;			
			
			while OE /= pac_offset_edges.no_element loop
				
				log (text => to_string (element (OE).edge), level => log_threshold + 2);


				-- Get the intersection of the candidate edge with the next edge:
				-- NOTE: The edges in list offset_edge are orderd counter clockwise.
				N := get_next_direct_intersection (OE, offset_edges.first);

				-- N.cursor now tells whether there is a direct intersection or not.
				-- If none exists, then we add to the list "intersections" only the next indirect
				-- intersection.
				-- If a direct intersection exists, then we add to "intersections" both
				-- the next indirect and the next direct intersection:
				
				if N.cursor = pac_offset_edges.no_element then
					log (text => "no direct intersection found",
						 level => log_threshold + 2);

					intersections.append ((
						direct_available	=> FALSE,
						indirect			=> (
							place	=> get_intersection_with_next_edge (OE),
							cursor	=> OE)));


				else
					log (text => "next direct intersection: " & to_string (N.place),
						level => log_threshold + 2);

					intersections.append ((
						direct_available	=> TRUE,
						direct				=> (
							place	=> N.place,
							cursor	=> N.cursor),
						
						indirect			=> (
							place	=> get_intersection_with_next_edge (OE),
							cursor	=> OE)));

				end if;
				
				next (OE);
			end loop;

			log_indentation_down;
		end find_intersections;



		
		-- The result of STEP 3 are the vertices of the final polygon:
		vertices : pac_vectors.list;


		
		-- Step 3:
		-- Traverses through list "intersections" and builds the list "vertices":
		procedure build_vertices is

			-- This counter prevents infinite looping.
			-- There can never be more vertices than intersections:
			safety_counter_limit : constant type_safety_count := 
				type_safety_count (intersections.length);
			
			safety_counter : type_safety_count := 0;

			
			-- This primary cursor points to an entry in list "intersections":
			c : pac_edge_intersections.cursor := intersections.first;

			-- A single vertex (taken from an indirect or a direct intersection):
			vertex : type_vector;
			

			-- This function searches in list "intersections" for a target entry that
			-- contains the given "offset edge" and returns the cursor to that entry:
			function fast_forward (target : in pac_offset_edges.cursor) 
				return pac_edge_intersections.cursor
			is 
				result : pac_edge_intersections.cursor;
				i : pac_edge_intersections.cursor := intersections.first;
			begin
				while i /= pac_edge_intersections.no_element loop
					if element (i).indirect.cursor = target then
						result := i;						
						exit;						
					end if;
					
					next (i);
				end loop;
				
				return result;
			end fast_forward;


			
			-- This secondary cursor is used to "look back" after fast forwarding.
			-- It is required to handle special case A (see details below):
			s : pac_edge_intersections.cursor;

			-- Used to handle special case A:
			ignore_next_direct : boolean := false;

			
		begin -- build_vertices
			log (text => "build_vertices", level => log_threshold + 1);
			log_indentation_up;
			
			log (text => "intersections:", level => log_threshold + 2);
			log_indentation_up;

			while c /= pac_edge_intersections.no_element loop

				-- Count loops and abort on safety counter overflow:
				increment_safety_counter_2 (safety_counter, safety_counter_limit);
				
				-- If a direct intersection with any following edge exists, then
				-- fast forward to that edge (thus skipping the edges inbetween).
				if element (c).direct_available 
				and not ignore_next_direct then

					-- Save the cursor of the inital edge,
					-- in order to handle special case A:
					s := c;

					-- Take the direct intersection as vertex:
					vertex := element (c).direct.place;

					-- Jump to the edge that intersects the candidate directly:
					c := fast_forward (element (c).direct.cursor);
					-- The primary cursor has now skipped some edges and is
					-- pointing to another edge.
				
					-- Special case A:
					-- Look ahead for another direct intersection.
					-- If the current edge (indicated by c) has a direct 
					-- intersection with the initial
					-- edge (indicated by s), then the next
					-- direct intersection is to be ignored and the next
					-- indirect intersection used instead.
					-- Otherwise we would end up with just a single vertex:
					if element (c).direct_available then
						if s = fast_forward (element (c).direct.cursor) then
							ignore_next_direct := true;
						end if;
					end if;

					
				-- If no direct intersection is available or if a certain
				-- direct intersection is to be ignored then take the current
				-- indirect intersection as vertex:
				else
					vertex := element (c).indirect.place;

					-- Reset the ignore-flag:
					ignore_next_direct := false;
					next (c);
				end if;


				-- If the just computed vertex has already been found as the very
				-- first vertex then the round trip along the edges is complete.
				-- Otherwise the just computed vertex is to be added to the list "vertices":
				if not vertices.is_empty and then vertices.first_element = vertex then
					exit;
				else
					log (text => to_string (vertex), level => log_threshold + 2);
					vertices.append (vertex);
				end if;

			end loop;

			log_indentation_down;
			log_indentation_down;
		end build_vertices;

		

		
	begin -- offset_polygon
		log (text => "offset_polygon: " & to_string (polygon), 
			 level => log_threshold);

		-- log (text => "log level " & to_string (log_threshold), level => log_threshold);
		
		log_indentation_up;

		
		if mode /= NOTHING then

		-- STEP 1:
			preprocess_edges;
		
			
		-- STEP 2:
			find_intersections;

			
		-- STEP 3:
			build_vertices;



		-- STEP 4:
			-- Convert the list "vertices" to a polygon.
			log (text => "convert vertices to polygon", level => log_threshold + 1);
			log_indentation_up;

			-- Overwrite the given polygon with a new one:
			polygon := to_polygon (vertices);

			--if not is_closed (polygon).closed then
				--raise constraint_error with "Polygon NOT closed !";
			--end if;
			log_indentation_down;
		end if;

		
		log_indentation_down;
		
		-- Convert the polygon specific exception to a constraint error:
		exception when event: others =>
			--put_line (exception_name (event) & " " & exception_message (event));

			log_indentation_down;
		
			raise constraint_error with 
				exception_name (event) & " " & exception_message (event);

	end offset_polygon;



	
	
	function offset_polygon (
		polygon			: in type_polygon;
		offset			: in type_float;
		log_threshold	: in type_log_level)
		return type_polygon
	is
		result : type_polygon := polygon;
	begin
		offset_polygon (result, offset, log_threshold);
		return result;
	end offset_polygon;


	

	
	function offset_polygons (
		polygons		: in pac_polygon_list.list;
		offset			: in type_float;
		log_threshold	: in type_log_level)
		return pac_polygon_list.list
	is
		use pac_polygon_list;
		result : pac_polygon_list.list;

		-- Iterate the given list of polygons. 
		-- Offset each of them and append it to the result:
		
		procedure query_polygon (c : in pac_polygon_list.cursor) is
			p : type_polygon := element (c);
		begin
			-- log (text => to_string (p), level => log_threshold + 1);
			-- log_indentation_up;
			offset_polygon (p, offset, log_threshold + 1);
			result.append (p);
			-- log_indentation_down;
		end query_polygon;
		
	begin
		log (text => "offset_polygons. count: " & get_count (polygons)
			 & " offset: " & to_string (offset),
			 level => log_threshold);
		
		log_indentation_up;
		
		polygons.iterate (query_polygon'access);

		log_indentation_down;
		
		return result;
	end offset_polygons;



	
	procedure offset_polygons (
		polygons		: in out pac_polygon_list.list;
		offset			: in type_float;
		log_threshold	: in type_log_level)
	is begin
		polygons := offset_polygons (polygons, offset, log_threshold);
	end offset_polygons;


	

	function to_mode (
		offset : in type_float)
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

	
end et_geometry_1.et_polygons.offsetting;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

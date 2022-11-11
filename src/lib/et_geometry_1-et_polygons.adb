------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      GEOMETRY 2 / POLYGONS                               --
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

with ada.exceptions;			use ada.exceptions;
with et_exceptions;				use et_exceptions;


package body et_geometry_1.et_polygons is

	
	function to_string (
		edge : in type_edge)
		return string
	is begin
		return "edge: S: " 
			--& to_string (edge.start_point.x)
		--& to_string (edge.start_point.y) 
			& to_string (edge.start_point)
			& " E: " 
			& to_string (edge.end_point);
			--& to_string (edge.end_point.y);
	end to_string;

	
	
	procedure iterate (
		edges	: in pac_edges.list;
		process	: not null access procedure (position : in pac_edges.cursor);
		proceed	: not null access boolean)
	is
		c : pac_edges.cursor := edges.first;
	begin
		while c /= pac_edges.no_element and proceed.all = TRUE loop
			process (c);
			next (c);
		end loop;
	end iterate;

	

	function to_edges (
		arc			: in type_arc;
		tolerance	: in type_float_internal_positive;
		mode		: in type_approximation_mode;
		debug		: in boolean := false)				  
		return pac_edges.list
	is
		-- This is the list of edges to be returned:
		result : pac_edges.list;
		
		
		-- The given arc is usually offset from the origin.
		-- This offset is later required to create the actual edges 
		-- where the given arc is:
		arc_offset : constant type_offset := to_offset (arc.center);

		-- Make a copy of the given arc with its center on the origin (0/0):
		arc_origin : constant type_arc := move_to (arc, null_vector);

		-- Get the start and end angles of the arc:
		arc_angles : constant type_arc_angles := to_arc_angles (arc_origin);

		-- Get the radius of the given arc:
		radius : constant type_float_internal := type_float_internal (arc_angles.radius);


		-- The tolerance must be adjusted according to the given radius.
		tolerance_dyn : type_float_internal_positive := tolerance;

		-- Adjusts the tolerance. 
		-- CS: Currently empirically. Need refinement.
		procedure compute_tolerance_dyn is begin
			if radius < 1.0 then
				tolerance_dyn := tolerance * radius;
			end if;
		end compute_tolerance_dyn;

		
		
		-- This is the total angle between start and end point of the given arc:
		span : type_angle;

		
		edge_ct_float : type_float_internal; -- number of edges
		edge_ct_final : positive; -- real number of edges
		-- CS subtype to limit the number of edges ?

		-- The theoretical and real angle between the vertices:
		angle_theo : type_float_internal;
		angle_real : type_float_internal;

		-- In case the mode is EXPAND then this is the
		-- start and end point of a virtual outer arc:
		p_start_outside, p_end_outside : type_vector;

		
		procedure make_edges (p_start : in type_vector) is
			-- The edges will be built on these location vectors:
			p_walk, p_walk_previous : type_vector;

			-- Rotates the given location vector by angle_real * m.
			-- m is a multipier according to the current edge being built:
			function rotate (
				p : in type_vector;
				m : in positive)
				return type_vector
			is 
				result : type_vector := p;
			begin
				case arc.direction is
					when CW =>
						rotate_by (result, - angle_real * type_float_internal (m)); 

					when CCW =>
						rotate_by (result, + angle_real * type_float_internal (m)); 
				end case;

				return result;
			end rotate;
			
		begin -- make_edges
			p_walk_previous := p_start;

			-- We rotate p_start as many times as edges are required:
			for e in 1 .. edge_ct_final loop

				-- For each pass the multipier increases:
				p_walk := rotate (p_start, e);

				-- Build an edge and append it to the result:
				result.append ((
					start_point => p_walk_previous,
					end_point	=> p_walk));
				
				p_walk_previous := p_walk;
			end loop;
		end make_edges;
		

		-- Moves the edges (in result) by the offset of the given arc.
		-- The resulting edges are the final edges that 
		-- shall approximate the given arc:
		procedure move_edges is 
			e : pac_edges.cursor := result.first;

			procedure do_it (edge : in out type_edge) is begin
				move_by (edge, arc_offset);

				if debug then
					put_line (" " & to_string (edge));
				end if;
			end do_it;
			
		begin
			while e /= pac_edges.no_element loop
				result.update_element (e, do_it'access);
				next (e);
			end loop;
		end move_edges;
		
		
	begin -- to_edges
		
		if debug then
			new_line;
			put_line ("approximate arc");
		end if;

		compute_tolerance_dyn;
		
		-- Get the span of the arc:
		span := get_span (arc_angles);

		-- Compute the theoretical angle required between the vertices:
		case mode is
			when SHRINK =>
				angle_theo := 2.0 * (90.0 - arcsin ((radius - tolerance_dyn) / radius, units_per_cycle));

			when EXPAND =>
				-- This fomula applies to the virtual outer arc (see spec of type_approximation_mode):
				angle_theo := 2.0 * (90.0 - arcsin (radius / (radius + tolerance_dyn), units_per_cycle));
		end case;
		
		-- Compute the number of edges required: 
		edge_ct_float := span / angle_theo;

		-- The result is not an integer. We need a natural number.
		-- So we must round up to the nearest integer:
		edge_ct_final := positive (type_float_internal'ceiling (edge_ct_float));
		
		-- The span divided by the natural number of edges
		-- gives us the real (practical) angle betweeen the vertices:
		angle_real := span / type_float_internal (edge_ct_final);

		
		if debug then
			new_line;
			put_line ("mode         : " & type_approximation_mode'image (mode));			
			put_line ("given arc    : " & to_string (arc_angles));
			put_line ("tolerance_dyn    : " & to_string (tolerance_dyn));
			put_line ("span         : " & to_string (span));
			put_line ("angle theo.  : " & to_string (angle_theo));
			--put_line ("edge ct float: " & to_string (edge_ct_float));
			put_line ("edge ct pre. : " & positive'image (edge_ct_final));
			put_line ("angle real   : " & to_string (angle_real));
		end if;


		
		if mode = EXPAND then
			-- The start point of the outer arc:
			p_start_outside := move_by (arc_origin.start_point, arc_angles.angle_start, tolerance_dyn);

			-- The end point of the outer arc:
			p_end_outside   := move_by (arc_origin.end_point, arc_angles.angle_end, tolerance_dyn);

			--if debug then
				--put_line ("start outside : " & to_string (p_start_outside));
				--put_line ("end   outside : " & to_string (p_end_outside));
			--end if;

			-- Compute the edges from p_start_outside to p_end_outside.
			-- (The first and last edge will later be replaced by two new edges.)
			make_edges (p_start_outside);

			declare
				I1, I2 : type_intersection_of_two_lines (EXISTS);
				N1, N2 : type_line_vector;
				
				center_to_start : constant type_line_vector := 
					(v_start => null_vector, v_direction => arc_origin.start_point);

				center_to_end : constant type_line_vector := 
					(v_start => null_vector, v_direction => arc_origin.end_point);

				E1, E2 : type_edge;
			begin
				-- Replace the first edge by two new edges:
				N1 := get_normal_vector (center_to_start, arc_origin.start_point);
				E1 := result.first_element;
				delete_first (result);
				I1 := get_intersection (N1, to_line_vector (E1));

				--if debug then
					----new_line;
					--put_line ("N1 : " & to_string (N1));
					--put_line ("I1 : " & to_string (I1.intersection.vector));
				--end if;

				E1.start_point := I1.intersection.vector;
				--put_line ("prepend B" & to_string (E1));
				result.prepend (E1);

				E1.end_point := E1.start_point;
				E1.start_point := arc_origin.start_point;
				--put_line ("prepend A" & to_string (E1));
				result.prepend (E1);


				-- Replace the last edge by two new edges:
				N2 := get_normal_vector (center_to_end, arc_origin.end_point);
				E2 := result.last_element;
				delete_last (result);
				I2 := get_intersection (N2, to_line_vector (E2));
				
				--if debug then
					--put_line ("N2 : " & to_string (N2));
					--put_line ("I2 : " & to_string (I2.intersection.vector));
				--end if;

				E2.end_point := I2.intersection.vector;
				--put_line ("append B" & to_string (E2));
				result.append (E2);

				E2.start_point := E2.end_point;
				E2.end_point := arc_origin.end_point;
				--put_line ("append A" & to_string (E2));
				result.append (E2);
			end;
			
		else -- SHRINK
			make_edges (arc_origin.start_point);
			-- CS snap the end of the last edge to p_end
		end if;

		-- Move the edges to the final position (of the given arc):
		move_edges;

		if debug then
			put_line ("edge ct fin. : " & count_type'image (result.length));
		end if;		
		
		return result;
	end to_edges;



	procedure update_boundaries (
		polygon	: in out type_polygon)
	is begin
		polygon.boundaries := get_boundaries (polygon);
	end update_boundaries;

	--pragma inline (update_boundaries);

	

	procedure rotate (
		polygon 	: in out type_polygon;
		direction	: in type_direction_of_rotation := CCW)
	is
		scratch : type_edge;
	begin
		case direction is
			when CCW =>
				scratch := polygon.edges.last_element;

				polygon.edges.insert (
					before		=> polygon.edges.first,
					new_item	=> scratch);

				polygon.edges.delete_last;
					
			when CW =>
				scratch := polygon.edges.first_element;

				polygon.edges.delete_first;
				polygon.edges.append (scratch);
				
		end case;
	end rotate;



	procedure optimize_edges (
		polygon : in out type_polygon;
		debug	: in boolean := false)
	is
		-- Here we collect the optimized edges. In the end this will
		-- overwrite the given polygon:
		result : type_polygon;

		-- A temporarily edge:
		scratch : type_edge;

		-- Tests the candidate edge of the given polygon against
		-- the direction of the scratch edge. If the candidate
		-- has the same direction as the scratch edge then the scratch
		-- edge will be extended (german: verlängert): 
		--  The end point of the scratch edge 
		--  assumes the end point of the candidate edge.
		-- If the candidate direction is different from the scratch edge
		-- then the scratch edge will be appended to the result. Afterward
		-- a new scratch edge is formed from the candidate edge.
		-- A special case may arise on the last candidate edge. See comments below.
		procedure query_edge (c : pac_edges.cursor) is
			candidate_edge : type_edge renames element (c);
			candidate_direction : constant type_angle := get_direction (candidate_edge);

			scratch_direction : constant type_angle := get_direction (scratch);
		begin
			if debug then
				put_line (to_string (candidate_edge));
			end if;

			-- Compare direction of scratch and candidate edge:
			if scratch_direction = candidate_direction then
				-- No change in direction.

				-- Extend scratch edge:
				scratch.end_point := candidate_edge.end_point;

				-- Special case on last candidate edge:
				-- The current scratch edge must be appended to the result somehow.
				-- But the whole optimization procedure uses as initial scratch the 
				-- last edge of the given polygon. If this scratch has been appended
				-- as first element to the result then is might running into the same
				-- direction as the current scratch edge. The current scratch must
				-- replace the erroneously first edge in the result:
				if c = polygon.edges.last then
					if scratch_direction = get_direction (result.edges.first_element) then
						if debug then
							put_line ("replace first");
						end if;
						
						result.edges.replace_element (result.edges.first, scratch);
					end if;
				end if;

			else
				-- Direction changed.				
				if debug then
					put_line ("direction change");
					put_line ("append " & to_string (scratch));
				end if;

				-- Append scratch edge to result:
				result.edges.append (scratch);
				
				-- Start a new scratch from the candidate edge:
				scratch := candidate_edge;	
			end if;
		end query_edge;

		
	begin
		if debug then
			put_line ("edges total:" & count_type'image (get_edges_total (polygon)));
			new_line;
		end if;

		-- A polygon in general has at least 3 edges. But a polygon to be optimized
		-- must have more than 3 edges. Otherwise there is nothing to do:
		if get_edges_total (polygon) > 3 then

			-- The initial scratch edge is the last edge of the
			-- given polygon:
			scratch := polygon.edges.last_element;

			-- Iterate the edges of the given polygon:
			polygon.edges.iterate (query_edge'access);

			-- Overwrite the given polygon by the optimized one:
			polygon := result;
		end if;
		
	end optimize_edges;


	function optimize_edges (
		polygon : in type_polygon;
		debug	: in boolean := false)
		return type_polygon
	is
		result : type_polygon := polygon;
	begin
		optimize_edges (result, debug);
		return result;
	end optimize_edges;


	
	procedure merge_overlapping_edges (
		polygon : in out type_polygon;
		debug	: in boolean := false)
	is
		-- The merge process consists of iterations. 
		-- Each of which calls procedure remove_overlap
		-- and merges only two overlapping edges.
		-- If a removal has taken place then this flag goes true:
		overlap_removed : boolean := true;

		edge_ct : constant count_type := get_edges_total (polygon);

		-- In order to prevent infinite looping we implement a 
		-- safety counter:
		subtype type_safe_count is count_type range 0 .. edge_ct;
		safety_counter : type_safe_count := 0;
		
		
		procedure remove_overlap is
			edge_cursor : pac_edges.cursor := polygon.edges.first;

			next_cursor : pac_edges.cursor;
			next_edge : type_edge;
			
			procedure merge (edge : in out type_edge) is begin
				edge.end_point := next_edge.end_point;
			end merge;
			
		begin
			-- Initially we assume there is no overlap.
			-- At the end of this procedure this flag is set if
			-- a removal has been taken place. Otherwise it remains
			-- cleared:
			overlap_removed := false;

			-- We probe all edges (except the last one) against the
			-- successor edge:
			while edge_cursor /= polygon.edges.last loop				
				next_cursor := next (edge_cursor);
				next_edge := element (next_cursor);

				-- We are interested in edges that overlap and run into
				-- opposide direction:
				if opposide_direction (element (edge_cursor), next_edge) then

					-- "Drag" the end of the candidate edge to the end point
					-- of the successor edge:
					polygon.edges.update_element (edge_cursor, merge'access);

					-- Remove the successor edge:
					polygon.edges.delete (next_cursor);

					-- A removal has taken place:
					overlap_removed := true;

					-- Abort edge probing here.
					exit;
				end if;
				
				next (edge_cursor);
			end loop;
		end remove_overlap;

		
	begin -- merge_overlapping_edges
		
		if debug then
			put_line ("Merge overlapping edges. Edges given" & count_type'image (edge_ct));
		end if;

		-- A polygon in general has at least 3 edges. But a polygon with edges
		-- to be merged must have more than 3 edges. -- CS is this statement true ?
		-- Otherwise there is nothing to do:
		if edge_ct > 3 then

			-- Initially the flag overlap_removed is true (see above). So this loop
			-- starts in any case. The loop is aborted after the procedure 
			-- remove_overlap did not detect and remove an overlap. remove_overlap
			-- modifies overlap_removed. As long as remove_overlap finds overlaps
			-- to remove this loop goes on. For safety reasons an exception is
			-- raised if more iterations are done than edges are given:
			while overlap_removed loop

				-- Count iterations:
				safety_counter := safety_counter + 1;

				--if debug then
					--put_line ("iteration" & count_type'image (safety_counter));
				--end if;
				
				remove_overlap;
			end loop;


			if debug then
				put_line ("iterations" & count_type'image (safety_counter));
			end if;

			
			-- Since remove_overlap does not test the last edge against the first edge,
			-- we must do the test here explicitely.
			-- If last and first edge overlap, then rotate the polygon and 
			-- call remove_overlap one last time:
			if opposide_direction (polygon.edges.last_element, polygon.edges.first_element) then

				if debug then
					put_line ("last overlaps first");
				end if;
				
				rotate (polygon);

				remove_overlap;
			end if;
			
			-- Final clean-up measure:
			-- There may be successive edges that run into the same direction.
			-- They must be merged:
			optimize_edges (polygon);

			
			if debug then
				put_line ("Edges final" & count_type'image (get_edges_total (polygon)));
			end if;

		end if;
		
		
		exception when others =>
			put_line ("Safety counter overrun ! (" & count_type'image (safety_counter) & ")");
			raise;
		
	end merge_overlapping_edges;
	

	
	function merge_overlapping_edges (
		polygon : in type_polygon;
		debug	: in boolean := false)
		return type_polygon
	is
		result : type_polygon := polygon;
	begin
		merge_overlapping_edges (result, debug);
		return result;
	end merge_overlapping_edges;

	


	procedure update_boundaries (
		polygons : in out pac_polygon_list.list)
	is
		procedure do_it (p : in out type_polygon) is
		begin
			p.boundaries := get_boundaries (p);
		end do_it;
		
		procedure query_polygon (p : in pac_polygon_list.cursor) is begin
			polygons.update_element (p, do_it'access);
		end query_polygon;
	
	begin
		polygons.iterate (query_polygon'access);
	end update_boundaries;

	

	function "=" (
		left, right : in pac_polygon_list.list)
		return boolean
	is
		use pac_polygon_list;
		
		result : boolean := false;

		length_left  : count_type := left.length;
		length_right : count_type := right.length;
		
		c_left  : pac_polygon_list.cursor;
		c_right : pac_polygon_list.cursor;
	begin
		if length_left + length_right = 0 then
			return true;
		end if;
		
		if left.length /= right.length then
			return false;
		else
			c_left  := left.first;
			c_right := right.first;

			for i in 1 .. length_left loop
				if not are_congruent (element (c_left), element (c_right)) then
					return false;
				end if;

				next (c_left);
				next (c_right);
			end loop;

			return true;
		end if;
		
	end "=";


	
	
	procedure iterate (
		holes	: in pac_polygon_list.list;
		process	: not null access procedure (position : in pac_polygon_list.cursor);
		proceed	: not null access boolean)
	is
		use pac_polygon_list;
		c : pac_polygon_list.cursor := holes.first;
	begin
		while c /= pac_polygon_list.no_element and proceed.all = TRUE loop
			process (c);
			next (c);
		end loop;
	end iterate;
	

	
	function to_polygon (
		vertices	: in string;
		clean_up	: in type_clean_up := true)
		return type_polygon
	is
		v_fields : constant type_fields_of_line := 
			read_line (line => vertices, comment_mark => "#");

		function f (p : in count_type) return string is begin
			return to_lower (get_field (v_fields, p));
		end;
		
		-- The place in vertices which we fetch a field from:
		place : count_type := 1;

		v : type_vertex := (category => REGULAR, location => OUTSIDE, position => null_vector);
		v_list : pac_vertices.list;

	begin
		-- Check number of given vertices:
		if field_count (v_fields) < 3 then
			put_line (error_message_too_few_vertices);
			raise semantic_error_1;
		end if;	
		
		
		-- Iterate all fields of given list of arguments:
		while place <= field_count (v_fields) loop

			v.position.x := type_float_internal'value (f (place));
			v.position.y := type_float_internal'value (f (place + 1));

			v_list.append (v);
			
			place := place + 2;
		end loop;

		return to_polygon (v_list, clean_up);

		exception 
			when others =>
				put_line (vertices);
				raise;
		
	end to_polygon;


	function to_polygon (
		vectors		: in pac_vectors.list;
		clean_up	: in type_clean_up := true)					
		return type_polygon
	is 
		v_list : pac_vertices.list;

		use pac_vectors;

		procedure query_vector (c : in pac_vectors.cursor) is
			v : type_vertex := (category => REGULAR, location => OUTSIDE, position => null_vector);
		begin
			v.position := element (c);
			v_list.append (v);
		end query_vector;
		
	begin
		if vectors.length < 3 then
			raise semantic_error_1;
		end if;
		
		vectors.iterate (query_vector'access);
		return to_polygon (v_list, clean_up);

		exception when event: others =>
			put_line (error_message_too_few_vertices);
			raise;
	end to_polygon;



	function get_vertices (polygon : in type_polygon)
		return pac_vectors.list
	is
		result : pac_vectors.list;
		
		procedure query_edge (e : in pac_edges.cursor) is begin
			result.append (element (e).start_point);
		end query_edge;
		
	begin
		polygon.edges.iterate (query_edge'access);
		return result;
	end get_vertices;


	procedure mirror_polygon (
		polygon	: in out type_polygon;
		axis	: in type_axis_2d)
	is
		vertices : pac_vectors.list;
	begin
		vertices := get_vertices (polygon);
		mirror_vectors (vertices, Y);
		polygon := to_polygon (vertices);
	end mirror_polygon;
	
	
	
	function rotate (
		polygon	: in type_polygon;
		center	: in type_vector;
		angle	: in type_angle)
		return type_polygon
	is 
		vertices : pac_vectors.list;
	begin
		vertices := get_vertices (polygon);
		
		move_by (vertices, invert (to_offset (center)));
		rotate_by (vertices, angle);
		move_by (vertices, to_offset (center));

		return to_polygon (vertices);
	end rotate;

	
	
	function get_boundaries (
		polygon : in type_polygon)
		return type_boundaries
	is
		result : type_boundaries; -- to be returned
		
		procedure query_edge (c : in pac_edges.cursor) is begin
			union (result, get_boundaries (element (c)));
		end query_edge;
		
	begin
		polygon.edges.iterate (query_edge'access);
		return result;
	end get_boundaries;

	--pragma inline (get_boundaries);
	

	
	function get_winding (
		polygon : in type_polygon)
		return type_direction_of_rotation
	is
		result : type_direction_of_rotation := CCW;

		-- https://stackoverflow.com/questions/1165647/how-to-determine-if-a-list-of-polygon-points-are-in-clockwise-order/1165943#1165943
		-- http://blog.element84.com/polygon-winding.html

		sum : type_float_internal := 0.0;
		
		procedure query_edge (c : in pac_edges.cursor) is
			x1, x2, y1, y2 : type_float_internal;
		begin
			x1 := type_float_internal (get_x (element (c).start_point));
			y1 := type_float_internal (get_y (element (c).start_point));

			if c /= polygon.edges.last then
				x2 := type_float_internal (get_x (element (next (c)).start_point));
				y2 := type_float_internal (get_y (element (next (c)).start_point));
			else
				x2 := type_float_internal (get_x (element (polygon.edges.first).start_point));
				y2 := type_float_internal (get_y (element (polygon.edges.first).start_point));
			end if;

			-- Sum over the edges, (x2 − x1)(y2 + y1).
			--put_line ("sum " & to_string (sum));
			sum := sum + (x2 - x1) * (y2 + y1);
		end query_edge;
		
	begin
		polygon.edges.iterate (query_edge'access);

		if sum > 0.0 then
			return CW;
		elsif sum < 0.0 then
			return CCW;
		else
			raise constraint_error; -- CS ??
		end if;

	end get_winding;



	procedure set_winding (
		polygon : in out type_polygon;
		winding	: in type_direction_of_rotation := winding_default)
	is
		-- Get the winding of the given polygon:
		w_actual : constant type_direction_of_rotation := get_winding (polygon);

		-- This polygon will be formed in the course of this procedure.
		-- It will later overwrite the given polygon:
		polygon_new : type_polygon;

		-- This procedure reverses the edges of the given polygon and
		-- appends them in opposide order to the new polygon:
		procedure do_it is

			procedure query_edge (c : in pac_edges.cursor) is 
				edge_new : type_edge;
				cursor_new : pac_edges.cursor;
			begin
				edge_new := reverse_line (element (c));
				
				if polygon_new.edges.is_empty then
					polygon_new.edges.append (edge_new);
				else
					cursor_new := polygon_new.edges.first;
					polygon_new.edges.prepend (edge_new);
				end if;
			end query_edge;
		
		begin
			polygon.edges.iterate (query_edge'access);
		end do_it;

		
	begin -- set_winding
		
		-- If actual winding is different from given winding then
		-- create a new polygon with the opposide winding. 
		-- Otherwise nothing to do.
		if w_actual /= winding then
			do_it;
			polygon := polygon_new;
		end if;
	end set_winding;
	

	function are_congruent (
		polygon_A 	: in type_polygon;
		polygon_B 	: in type_polygon;
		debug		: in boolean := false)			   
		return boolean
	is 
		result : boolean := false;

		ct_A : constant count_type := get_edges_total (polygon_A);
		ct_B : constant count_type := get_edges_total (polygon_B);
		
		edge_A : type_edge;

		edge_B_cursor : pac_edges.cursor;

		
		procedure compare_edges is
			proceed : aliased boolean := true;

			procedure query_A_edge (edge_A_cursor : pac_edges.cursor) is begin
				-- If the two edges are not equal then abort 
				-- the iteration:
				if debug then
					put_line ("comparing " & to_string (element (edge_A_cursor)) 
					& " " & to_string (element (edge_B_cursor)));
				end if;
				
				if element (edge_A_cursor) /= element (edge_B_cursor) then
					proceed := false;

					if debug then
						put_line ("not equal");
					end if;
				end if;

				-- Prepare for the next edge of polygon B:
				next (edge_B_cursor);

				-- If end of list of B-edges reached, set B-cursor to
				-- begin of list:
				if edge_B_cursor = pac_edges.no_element then
					edge_B_cursor := polygon_B.edges.first;
				end if;
			end query_A_edge;
			
		begin
			iterate (polygon_A.edges, query_A_edge'access, proceed'access);
			result := proceed;
		end compare_edges;

		
	begin
		if debug then
			put_line ("congruent test:");
		end if;
		
		-- The first and easiest test is to compare the number of edges.
		-- If they differ, then the polygons are definitely not congruent:
		if ct_A /= ct_B then
			if debug then
				put_line ("edge count mismatch. ct A:" & count_type'image (ct_A)
				& " ct B:" & count_type'image (ct_B));
			end if;
				
			result := false;
		else
			--put_line ("A");
			
			-- Get the first segment of polygon A:
			edge_A := polygon_A.edges.first_element;

			--put_line ("B");
			
			-- Search for that element in polygon B:
			edge_B_cursor := polygon_B.edges.find (edge_A);

			--put_line ("C");
			
			-- If polygon B contains this edge then proceed comparing
			-- the segments from this position on.
			-- If polygon B does not contain this starting edge then
			-- the polygons are not congruent:
			if edge_B_cursor /= pac_edges.no_element then
				--put_line ("D");
				compare_edges;
			else
				-- not congruent
				if debug then
					put_line (to_string (edge_A) 
					& " not found in polygon B.");
				end if;
					
				result := false;
			end if;
		end if;
		
		return result;
	end are_congruent;

	

	function get_edge (
		polygon	: in type_polygon;
		edge	: in type_edge)
		return pac_edges.cursor
	is
		result : pac_edges.cursor;
	begin
		result := polygon.edges.find (edge);
		return result;
	end get_edge;
	


	function get_shortest_edge (
		polygon	: in type_polygon)
		return pac_edges.cursor
	is
		length : type_float_internal_positive := line_length_max;
		result : pac_edges.cursor; -- to be returned

		procedure query_edge (c : in pac_edges.cursor) is
			l_tmp : type_float_internal_positive := get_length (element (c));
		begin
			if l_tmp < length then
				length := l_tmp;
				result := c;
			end if;
		end query_edge;
		
	begin
		polygon.edges.iterate (query_edge'access);
		return result;
	end get_shortest_edge;
	


	function get_shortest_edge (
		polygon	: in type_polygon)
		return type_float_internal_positive
	is begin
		return get_length (element (get_shortest_edge (polygon)));
	end get_shortest_edge;


	procedure check_length (
		polygon	: in type_polygon)
	is 
		edge_length_min : constant type_float_internal_positive := 1.0E-6;
		-- CS use constant defined in package spec
		
		procedure query_edge (c : in pac_edges.cursor) is
			l : type_float_internal_positive := get_length (element (c));
		begin
			--put_line ("edge length" & to_string (l));
			
			if l < edge_length_min then
				put_line ("WARNING: Edge too short. Length:" & to_string (l));
				put_line (to_string (element (c)));
				new_line;
			end if;
		end query_edge;
		
	begin
		polygon.edges.iterate (query_edge'access);
	end check_length;
	
	

	

	
	function to_string (
		polygon	: in type_polygon)
		return string
	is
		use ada.strings.unbounded;
		use ada.characters.latin_1;
		
		result : unbounded_string := to_unbounded_string ("polygon vertices:");

		ct : natural := 0;
		
		procedure query_edge (c : in pac_edges.cursor) is begin

			-- We output the start points only.
			-- Because: The end point of an edge is always the start point of the
			-- next edge.

			result := result & LF
				& to_unbounded_string (to_string (element (c).start_point));

			ct := ct + 1;
		end query_edge;

		
	begin
		polygon.edges.iterate (query_edge'access);
		
		return to_string (result) & LF & "total:" & positive'image (ct);
	end to_string;




	

	function get_edges_total (
		polygon : in type_polygon)
		return count_type
	is begin
		return length (polygon.edges);
	end get_edges_total;

	
	
	
	
-- private

	


	function get_shortest_distance (
		polygon	: in type_polygon;
		point	: in type_vector)
		return type_float_internal
	is
		result : type_float_internal := type_float_internal'last;
		
		procedure update (d : in type_float_internal) is begin
			--put_line (to_string (d));
			if d < result then
				result := d;
			end if;
		end update;

		
		procedure query_edge (c : in pac_edges.cursor) is begin
			--put_line (to_string (s.segment_line));
			update (get_shortest_distance (point, element (c)));
		end query_edge;

		
	begin
		polygon.edges.iterate (query_edge'access);				
		return result;
	end get_shortest_distance;


	
	
	function is_vertex (
		polygon	: in type_polygon;
		point	: in type_vector)
		return boolean
	is
		proceed : aliased boolean := true;

		procedure query_edge (c : in pac_edges.cursor) is begin
			if element (c).start_point = point then
				proceed := false;
			end if;
		end query_edge;
		
	begin
		iterate (
			edges	=> polygon.edges,
			process	=> query_edge'access,
			proceed	=> proceed'access);

		return not proceed;
	end is_vertex;



	function get_edge (
		polygon	: in type_polygon;
		point	: in type_vector)
		return pac_edges.cursor
	is
		result : pac_edges.cursor;
		proceed : aliased boolean := true;

		procedure query_edge (c : in pac_edges.cursor) is begin
			if on_line (point, element (c)) then
				result := c;
				proceed := false; -- abort iteration
			end if;
		end query_edge;
		
	begin
		-- Make sure the given point is NOT a vertex:
		-- CS: Maybe no need if caller cares for this check.
		if is_vertex (polygon, point) then
			raise constraint_error with "Point is a vertex !";
		else
			iterate (polygon.edges, query_edge'access, proceed'access);					 
		end if;

		return result;
	end get_edge;

	
	function get_neigboring_edges (
		polygon	: in type_polygon;
		vertex	: in type_vector)
		return type_neigboring_edges
	is
		result : type_neigboring_edges;
		proceed : aliased boolean := true;

		end_found, start_found : boolean := false;
		
		procedure query_edge (c : in pac_edges.cursor) is begin
			--put_line ("test: " & to_string (element (c)));
			
			if element (c).end_point = vertex then
				--put_line ("end");
				result.edge_1 := c;
				end_found := true;
			end if;

			if element (c).start_point = vertex then
				--put_line ("start");
				result.edge_2 := c;
				start_found := true;
			end if;

			-- Abort iteration once start and end point have been found
			-- ("proceed" is low-active):
			proceed := not (end_found and start_found);
		end query_edge;

	begin
		-- Make sure the given point IS a vertex:
		if is_vertex (polygon, vertex) then
			iterate (polygon.edges, query_edge'access, proceed'access);					 
		else
			raise constraint_error with "Point is a vertex !";
		end if;

		-- Safety check:
		-- Two edges must have been found. Otherwise raise exception:
		if result.edge_1 = pac_edges.no_element 
		or result.edge_2 = pac_edges.no_element
		then
			raise constraint_error with "Search for neigboring edges incomplete !";
		end if;

		return result;
	end get_neigboring_edges;



	function to_string (status : in type_location) return string is begin
		return type_location'image (status);
	end to_string;





	
	
	function to_string (
		i : in type_point_status)
		return string
	is
		use ada.strings.unbounded;
		use pac_float_numbers;

		result : unbounded_string;
		
		procedure query_intersection (c : pac_float_numbers.cursor) is begin
			result := result & type_float_internal'image (element (c)); 
						--& "/" & trim (to_string (element (c).angle), left);
		end query_intersection;

	begin
		--case i.status is
			--when OUTSIDE =>
				--result := to_unbounded_string ("Point" 
					--& to_string (i.start) 
					--& " is OUTSIDE of polygon. ");

			--when INSIDE =>
				--result := to_unbounded_string ("Point" 
					--& to_string (i.start)
					--& " is INSIDE polygon. ");

			--when ON_EDGE =>
				result := to_unbounded_string ("Point" 
					& to_string (i.start) 
					& " is " & to_string (i.location) & " of polygon. ");

		--end case;

		result := result & "Distance to polygon: " & to_string (i.distance) & ". ";
				
		if is_empty (i.x_intersections) then
			result := result & "No intersections with probe line.";
		else
			result := result & "Intersection(s) with probe line (x):";
		end if;
		
		iterate (i.x_intersections, query_intersection'access);
		
		return to_string (result);
	end to_string;



	function get_intersections (
		status	: in type_point_status;
		after	: in type_float_internal := type_float_internal'first;
		before	: in type_float_internal := type_float_internal'last)		
		return pac_vectors.list
	is
		result : pac_vectors.list;

		use pac_float_numbers;

		procedure query_intersection (i : in pac_float_numbers.cursor) is
			x : type_float_internal renames element (i);
		begin
			if x > after and x < before then
				result.append ((element (i), status.start.y, 0.0));
			end if;
		end query_intersection;
		
	begin
		status.x_intersections.iterate (query_intersection'access);
		return result;
	end get_intersections;

	

	function get_point_status (
		polygon	: in type_polygon;	
		point	: in type_vector;
		debug	: in boolean := false)
		return type_point_status 
	is
		-- This function bases on the algorithm published at
		-- <http://www.alienryderflex.com/polygon//>

		-- A probe line will be formed which starts at the given point
		-- and runs to the right (direction zero degree).
		-- The places, after the given start point, where the probe line 
		-- intersects the polygon edges are returned in a list.
		-- If an edge of the polygon crosses the imaginary probe line somewhere,
		-- then this place is regarded as intersection.
		
		-- NOTE: An edge that overlaps the probe line is NOT regarded as 
		-- "crossing" the probe line. It causes no intersection and
		-- will thus be ignored.
		
		-- The approach to detect whether the given point lies inside or outside 
		-- the polygon area is as follows:
		-- 1. Build a probe line (starting at point) that runs at zero degrees
		--    to the right. The probe line divides the area in two: an upper half and a
		--    lower half. Special situations arise if objects start or end exactly at
		--    the probe line.
		-- 2. The parity of intersections after the start point then tells us:
		--    - odd -> point is inside the polygon area
		--    - zero or even -> point is outside the polygon area

		-- These are the components of the return value.
		-- In the end of this function they will be assembled 
		-- to the actual return:
		result_status : type_location;

		use pac_float_numbers;
		result_intersections : pac_float_numbers.list;
		result_distance : type_float_internal := 0.0;
		result_edge : pac_edges.cursor;
		result_neigboring_edges : type_neigboring_edges;

		
		probe_line : constant type_line_vector := (
			v_start => point,
			v_direction => (1.0, 0.0, 0.0));

		-- For segments that end or start exactly on the Y value of the probe line
		-- we define a threshold:
		y_threshold : constant type_float_internal := get_y (point);


		
		-- This is the variable for the number of intersections detected.
		-- From this number we will later deduce the position of the given point,
		-- means whether it is inside or outside the polygon:
		it : count_type := 0;



		
		-- This procedure collects the intersection in the return value.
		procedure collect_intersection (
			intersection: in et_geometry_1.type_intersection)
		is 
			xi : constant type_float_internal := get_x (intersection.vector);
		begin
			-- The intersection will be collected if it is ON or
			-- AFTER the given start point. If it is before the start
			-- point then we ignore it:
			if xi >= get_x (point) then				
				append (result_intersections, xi);
			end if;
		end collect_intersection;


		-- If the start/end point of the candidate edge is ABOVE-OR-ON the 
		-- threshold AND if the end/start point of the candidate line is BELOW the
		-- threshold then we consider the edge to be threshold-crossing.
		function crosses_threshold (
			edge : in type_edge;
			y_th : in type_float_internal)
			return boolean
		is 

			function less_than (left, right : in type_float_internal) return boolean is begin
				if right - left > accuracy then
					return true;
				else
					return false;
				end if;
			end less_than;

			
			function greater_or_equal (left, right : in type_float_internal) return boolean is begin
				if right = left then
					return true;
				elsif left - right > accuracy then
					return true;
				else
					return false;
				end if;
			end greater_or_equal;

			
		begin
			if	
				-- edge comes from above
				greater_or_equal (edge.start_point.y, y_th) and
				less_than        (edge.end_point.y  , y_th) then
				return true;
				
			elsif
				-- edge comes from below
				less_than        (edge.start_point.y, y_th) and
				greater_or_equal (edge.end_point.y  , y_th) then
				return true;
				
			else
				return false;
			end if;
		end crosses_threshold;
	
		
		procedure query_edge (c : in pac_edges.cursor) is 
			-- Find out whether there is an intersection of the probe line
			-- and the candidate edge of the polygon.
			i : constant type_intersection_of_two_lines := 
				get_intersection (probe_line, element (c), debug);
			
		begin
			if debug then
				put_line ("##");		
				put_line (to_string (element (c)));
			end if;
			
			if i.status = EXISTS then
				if debug then
					put_line ("intersection exists");
					--put_line (to_string (l));
					put_line ("y-threshold:" & to_string (y_threshold));
					--put_line (to_string (i.intersection.vector));
				end if;
				
				-- If the candidate line segment crosses the y_threshold then 
				-- count the intersection:
				if crosses_threshold (element (c), y_threshold) then
					if debug then
						put_line ("edge crosses threshold");
					end if;
					
					-- Add the intersection to the result:
					collect_intersection (i.intersection);
				end if;
			end if;				
		end query_edge;


		use pac_float_numbers_sorting;
		
		
	begin -- get_point_status
		--put_line ("Y-threshold:" & to_string (y_threshold));
		
		polygon.edges.iterate (query_edge'access);
		
		-- get the total number of intersections
		it := pac_float_numbers.length (result_intersections);

		--put_line ("intersections total:" & count_type'image (it));
		--put_line (to_string (result_intersections));
		
		-- If the total number of intersections is an odd number, then the given point
		-- is inside the polygon.
		-- If the total is even, then the point is outside the polygon.
		if (it rem 2) = 1 then
			result_status := INSIDE;
			--put_line ("inside");
		else 
			result_status := OUTSIDE;
			--put_line ("outside");
		end if;


		-- The x-values are not sorted yet. We need them sorted with the
		-- smallest x first:
		sort (result_intersections);

		-- Further on they must be cleaned up so that
		-- redundant values are removed. 
		-- Example: "4.0 4.0 6.3 12.0 12.0 13.3" becomes "6.3 13.3":
		clean_up (result_intersections, REMOVE_REDUNDANT);
		
		
		-- Figure out whether the given point is a vertex, whether
		-- it lies on an edge or whether it lies somewhere else:
		if is_vertex (polygon, point) then
			result_status := ON_VERTEX;
			-- NOTE: result.distance is zero by default

			-- Get the edges that meet at the given point:
			result_neigboring_edges := get_neigboring_edges (polygon, point);
		else
			result_edge := get_edge (polygon, point);

			if result_edge /= pac_edges.no_element then
				result_status := ON_EDGE;
				-- NOTE: result.distance is zero by default
			else
				-- Point is somewhere else.
				
				-- Compute the distance of the given point to the polygon.
				-- If the distance is zero then the given point lies on
				-- a vertex or on an edge:
				result_distance := get_shortest_distance (polygon, point); 
			end if;
			
		end if;


		-- Assemble the return:
		case result_status is
			when INSIDE =>
				return (INSIDE, point, result_intersections, result_distance);
					
			when OUTSIDE =>
				return (OUTSIDE, point, result_intersections, result_distance);
				
			when ON_EDGE =>
				return (ON_EDGE, point, result_intersections, result_edge);
				
			when ON_VERTEX =>
				return (ON_VERTEX, point, result_intersections, result_neigboring_edges);
				
		end case;
		
	end get_point_status;


	


	
	function get_location (
		polygon	: in type_polygon;	
		point	: in type_vector)
		return type_location
	is begin
		return get_point_status (polygon, point).location;
	end get_location;

	



	
	
	procedure toggle_direction (
		d : in out type_intersection_direction) 
	is begin
		case d is
			when ENTERING => d := LEAVING;
			when LEAVING => d := ENTERING;
		end case;
	end toggle_direction;

	

	
	function contains (
		intersections	: in pac_line_edge_intersections.list;
		place			: in type_vector)
		return boolean
	is
		result : boolean := false;

		use pac_line_edge_intersections;
		c : pac_line_edge_intersections.cursor := intersections.first;
	begin
		while c /= pac_line_edge_intersections.no_element loop
			if element (c).position = place then
				result := true;
				exit;
			end if;
			
			next (c);
		end loop;

		return result;
	end contains;

	
	procedure sort_by_distance (
		intersections	: in out pac_line_edge_intersections.list;
		reference		: in type_vector)
	is
		type type_item is record
			intersection: type_intersection_line_edge;
			distance	: type_float_internal_positive;
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


		use pac_line_edge_intersections;
		
		procedure query_intersection (i : in pac_line_edge_intersections.cursor) is 
			d : type_float_internal_positive;
		begin
			d := get_distance_total (reference, element (i).position);
			
			items.append (new_item => (
				intersection	=> element (i),
				distance		=> d));
		end query_intersection;

		

		package pac_sorting is new pac_items.generic_sorting;
		use pac_sorting;
		

		procedure query_item (i : in pac_items.cursor) is begin
			intersections.append (element (i).intersection);
		end query_item;
		
		
	begin
		-- Collect intersections and their distance to the reference
		-- in list "items":
		intersections.iterate (query_intersection'access);

		-- Sort items by distance to reference:
		sort (items);

		-- The old intersections are no longer required:
		intersections.clear;
		-- New intersections will be appended here.
		

		-- Traverse items and append them one by one to the
		-- list of intersections:
		items.iterate (query_item'access);
	end sort_by_distance;



	function to_string (
		line_end : in type_line_end)
		return string
	is
		use ada.strings.unbounded;
		use ada.characters.latin_1;
		result : unbounded_string := to_unbounded_string (type_location'image (line_end.location));
	begin
		-- CS edges
		return to_string (result);
	end to_string;
	
	
	
	function to_string (
		status	: in type_edge_status)
		return string
	is 
		use ada.strings.unbounded;
		use ada.characters.latin_1;
		result : unbounded_string := to_unbounded_string ("edge to polygon status:" & LF);
	begin
		result := result & to_string (status.edge) & LF;
		result := result & "start: " & to_string (status.start_point) & LF;
		result := result & "end  : " & to_string (status.end_point);

		-- CS intersections
		return to_string (result);
	end to_string;


	

	function get_previous_status (
		status_list	: in pac_edge_status_list.list;
		candidate	: in pac_edge_status_list.cursor)
		return pac_edge_status_list.cursor
	is
		use pac_edge_status_list;
		result : pac_edge_status_list.cursor;
	begin
		if candidate = status_list.first then
			result := status_list.last;
		else
			result := previous (candidate);
		end if;
		
		return result;
	end get_previous_status;


	function get_next_status (
		status_list	: in pac_edge_status_list.list;
		candidate	: in pac_edge_status_list.cursor)
		return pac_edge_status_list.cursor
	is
		use pac_edge_status_list;
		result : pac_edge_status_list.cursor;
	begin
		if candidate = status_list.last then
			result := status_list.first;
		else
			result := next (candidate);
		end if;
		
		return result;
	end get_next_status;

	
	--function get_first_outside (
		--polygon	: in type_polygon;
		--edges	: in pac_edge_status_list.list)		
		--return pac_edge_status_list.cursor
	--is
		--result : pac_edge_status_list.cursor;
		
		--use pac_edge_status_list;
		--c : pac_edge_status_list.cursor := edges.first;

		--center : type_vector;
	--begin
		---- look for a start point that is outside:
		--while c /= pac_edge_status_list.no_element loop			
			--if element (c).start_point.location = OUTSIDE then
				--result := c;
				--exit;
			--end if;			
			--next (c);
		--end loop;

		---- If no suitable start point found, then look for an edge that
		---- has at least one intersection:
		--if result = pac_edge_status_list.no_element then
			--c := edges.first;

			--while c /= pac_edge_status_list.no_element loop
				--if element (c).intersections.length > 1 then
					--result := c;
					--exit;
				--end if;
				--next (c);
			--end loop;
		--end if;
		

		---- If no suitable edge found, then look for an edge that
		---- - has no intersection and
		---- - has its center outside
		--if result = pac_edge_status_list.no_element then
			--c := edges.first;

			--while c /= pac_edge_status_list.no_element loop
				--if element (c).intersections.length = 0 then

					--if element (c).start_point.location = ON_EDGE then
						--center := get_center (element (element (c).start_point.edge));
						--if get_location (polygon, center) = OUTSIDE then
							--result := c;
							--exit;
						--end if;
					--end if;

					--if element (c).start_point.location = ON_VERTEX then
						--center := get_center (element (element (c).start_point.edges.edge_2));
						--if get_location (polygon, center) = OUTSIDE then
							--result := c;
							--exit;
						--end if;
					--end if;
					
				--end if;
				--next (c);
			--end loop;
		--end if;

		
		--if result = pac_edge_status_list.no_element then
			--raise constraint_error with "No suitable edge found !";
		--end if;		
		
		--return result;
	--end get_first_outside;


	function get_section_location (
		polygon			: in type_polygon;
		status_cursor	: in pac_edge_status_list.cursor;
		section			: in type_section)		
		return type_location
	is
		result : type_location := OUTSIDE;

		use pac_edge_status_list;
		use pac_line_edge_intersections;

		sts : type_edge_status renames element (status_cursor);
		
		-- The number of intersections on the given edge:
		int_count : constant count_type := sts.intersections.length;

	begin
		if int_count = 0 then
			-- Edge is not intersected at all. There is only
			-- the start and end point of the edge.
			-- Se we look at the center of the edge:
			return get_location (polygon, get_center (sts.edge));
			
		else
			-- Edge is intersected at least once:
			case section is
				when FIRST =>
					case sts.intersections.first_element.direction is
						when LEAVING =>
							result := INSIDE;

						when ENTERING =>
							result := OUTSIDE;
					end case;

				when LAST =>
					case sts.intersections.last_element.direction is
						when LEAVING =>
							result := OUTSIDE;

						when ENTERING =>
							result := INSIDE;
					end case;

			end case;
		end if;
		
		return result;
	end get_section_location;
	
	

	
	
	function get_edge_status (
		polygon	: in type_polygon;
		edge	: in type_edge)
		return type_edge_status
	is
		-- Pass the affected edge right away to the result:
		result : type_edge_status := (edge => edge, others => <>);

		edge_direction : constant type_angle := get_direction (edge);
		edge_length : constant type_float_internal_positive := get_length (edge);
		P_rotated : type_polygon;
		intersections : pac_vectors.list;
		
		
		intersection_count : count_type := 0;
		count_is_even : boolean := false;
		
		
		procedure set_line_start is 
			PPS : constant type_point_status := 
				get_point_status (polygon, edge.start_point);
		begin
			case PPS.location is
				when INSIDE => 
					result.start_point := (location => INSIDE);

				when OUTSIDE => 
					result.start_point := (location => OUTSIDE);

				when ON_EDGE => 
					result.start_point := (
						location	=> ON_EDGE, 
						edge		=> PPS.edge);

				when ON_VERTEX => 
					result.start_point := (
						location	=> ON_VERTEX, 
						edges		=> PPS.edges);
					
			end case;
		end set_line_start;

		
		procedure set_line_end is 
			PPS : constant type_point_status := 
				get_point_status (polygon, edge.end_point);
		begin
			case PPS.location is
				when INSIDE => 
					result.end_point := (location => INSIDE);

				when OUTSIDE => 
					result.end_point := (location => OUTSIDE);

				when ON_EDGE => 
					result.end_point := (
						location	=> ON_EDGE, 
						edge		=> PPS.edge);

				when ON_VERTEX => 
					result.end_point := (
						location	=> ON_VERTEX, 
						edges		=> PPS.edges);
		
			end case;
		end set_line_end;


		procedure set_entering_leaving is
			use pac_line_edge_intersections;
			i : pac_line_edge_intersections.cursor := result.intersections.first;

			initial_direction : type_intersection_direction;

			-- The edge may start/end outside, inside, on an edge
			-- or on a vertex of the given polygon.
			-- Depending on this constellation and the parity of intersections
			-- the direction of the first intersection is deduced:
			procedure set_initial_direction is begin
				case result.start_point.location is
					when OUTSIDE =>
						initial_direction := ENTERING;

					when INSIDE =>
						initial_direction := LEAVING;

					when ON_EDGE | ON_VERTEX =>
						case result.end_point.location is
							when OUTSIDE =>
								if count_is_even then
									initial_direction := ENTERING;
								else
									initial_direction := LEAVING;
								end if;

							when INSIDE | ON_EDGE | ON_VERTEX =>
								if count_is_even then
									initial_direction := LEAVING;
								else
									initial_direction := ENTERING;
								end if;								
						end case;
				end case;						
			end set_initial_direction;
		
						
			-- This procedure assigns a definite direction 
			-- to the candidate intersection:
			procedure set_candidate_direction (
				i : in out type_intersection_line_edge) 
			is begin
				i.direction := initial_direction;
			end set_candidate_direction;

			
		begin -- set_entering_leaving
			set_initial_direction;
			
			-- Iterate through the intersections and assign each of 
			-- them a direction. The first intersection gets the initial_direction.
			while i /= pac_line_edge_intersections.no_element loop
				result.intersections.update_element (i, set_candidate_direction'access);
				toggle_direction (initial_direction);
				next (i);
			end loop;
		end set_entering_leaving;


		-- Assigns to the resulting intersections the 
		-- x/y-position and the affected edges:
		procedure assign_position_and_edge is 

			procedure query_intersection (c : in pac_vectors.cursor) is 
				I : type_vector renames pac_vectors.element (c);
				N : type_neigboring_edges;
				E : pac_edges.cursor;
			begin
				if is_vertex (polygon, I) then
					N := get_neigboring_edges (polygon, I);
					E := N.edge_2;
				else
					E := get_edge (polygon, I);
				end if;

				result.intersections.append ((
					position => I,
					edge => E, 
					others => <>)); -- The direction will be set later.
				
			end query_intersection;

		
		begin
			intersections.iterate (query_intersection'access);
		end assign_position_and_edge;

		
		
	begin -- get_edge_status

		-- Set the status of the start/end point of the given line:
		set_line_start;
		set_line_end;

		-- Rotate the given polygon about the start point of the edge.
		-- Rotate the polygon by the negative edge direction:
		P_rotated := rotate (polygon, edge.start_point, - edge_direction);

		-- Extract the intersections of the probe line between "after" and "before":
		intersections := get_intersections (
			status	=> get_point_status (P_rotated, edge.start_point),
			after	=> edge.start_point.x,
			before	=> edge.start_point.x + edge_length - accuracy);
			-- CS: Due to unavoidable inaccuracy, intersections very close
			-- to the end of the range must be ignored. Not sure if this is reasonable
			-- and whether this solution works in all cases.

		
		move_by (intersections, invert (to_offset (edge.start_point)));
		rotate_by (intersections, edge_direction);
		move_by (intersections, to_offset (edge.start_point));

		assign_position_and_edge;

		intersection_count := result.intersections.length;

		if (intersection_count rem 2) = 0 then
			count_is_even := true;
		end if;

		
		if intersection_count > 0 then

			-- Assign directions to the intersections
			-- betweeen start and end:
			set_entering_leaving;

		end if;
		
		return result;
	end get_edge_status;


	

	function to_string (intersection : in type_intersection)
		return string
	is begin
		return to_string (intersection.position) 
			& " " & type_intersection_direction'image (intersection.direction);
			-- CS output A and B edge ?
	end to_string;


	procedure iterate (
		intersections	: in pac_intersections.list;
		process			: not null access procedure (position : in pac_intersections.cursor);
		proceed			: not null access boolean)
	is
		c : pac_intersections.cursor := intersections.first;
	begin
		while c /= pac_intersections.no_element and proceed.all = TRUE loop
			process (c);
			next (c);
		end loop;
	end iterate;


	
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


	--function get_real_intersections (
		--intersections	: in pac_intersections.list)
		--return pac_intersections.list
	--is
		--result : pac_intersections.list;

		--procedure compare_position_and_direction (
			--i1, i2 : in pac_intersections.cursor)
		--is begin
			---- The two intersections in question must have
			---- same x/y position and differing direction, means
			---- one is an entering and the other is a leaving one.
			---- On this x/y position we have no real intersection
			---- but just a touch point:
			--if same_position (i1, i2)
			--and element (i1).direction /= element (i2).direction
			--then 
				--null; -- A touches B -> skip this intersection
			--else
				---- is real -> collect this intersection
				--result.append (element (i1)); 
				----put_line ("real intersection:" & to_string (element (i1)));
			--end if;
		--end compare_position_and_direction;
				
		--procedure query_intersection (
			--c : in pac_intersections.cursor) 
		--is begin
			---- CS: We assume that on a touch point a leaving and an
			---- entering node follow each other (in the given 
			---- list of intersections). So we always look at the 
			---- predecessor of the candidate intersection (indicated by 
			---- cursor c):
			--if c = intersections.first then
				--compare_position_and_direction (c, intersections.last);
			--else
				--compare_position_and_direction (c, previous (c));
			--end if;
		--end query_intersection;
		
	--begin
		--intersections.iterate (query_intersection'access);
		--return result;
	--end get_real_intersections;


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
	


	
	function all_vertices_of_A_inside_B (
		polygon_A	: in type_polygon; -- the clipped polygon
		polygon_B	: in type_polygon) -- the clipping polygon
		return boolean
	is
		proceed : aliased boolean := true;

		-- Query the start point of segment of polygon A.
		-- The segment is indicated by cursor c.
		-- Aborts the iteration on the first vertex that is
		-- outside polygon B.
		procedure query_edge (c : in pac_edges.cursor) is 
			IPQ : constant type_point_status :=
				get_point_status (polygon_B, element (c).start_point);
		begin
			if IPQ.location = OUTSIDE then
				proceed := false; -- abort iteration
			end if;
		end query_edge;
		
	begin
		iterate (
			edges	=> polygon_A.edges,
			process	=> query_edge'access,
			proceed	=> proceed'access);
			
		return proceed;
	end all_vertices_of_A_inside_B;


	
	function to_string (vertex : in type_vertex)
		return string
	is begin
		case vertex.category is
			when REGULAR =>
				return to_string (vertex.position);

			when INTERSECTION =>
				return to_string (vertex.position)
				& " " & type_intersection_direction'image (vertex.direction);
		end case;
	end to_string;



	function to_string (vertices : in pac_vertices.list) 
		return string 
	is
		use ada.strings.unbounded;
		use ada.characters.latin_1;
		
		result : unbounded_string;
		
		procedure query_vertex (v : in pac_vertices.cursor) is begin
			result := result & LF & to_string (element (v));
		end query_vertex;
			
	begin
		vertices.iterate (query_vertex'access);
		return to_string (result);
	end to_string;



	procedure sort_by_distance (
		vertices	: in out pac_vertices.list;
		reference	: in type_vector)
	is
		type type_item is record
			-- We will be sorting intersections only (no regular vertices):
			vertex		: type_vertex (category => INTERSECTION);
			distance	: type_float_internal_positive;
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
			d : type_float_internal_positive;
		begin
			d := get_distance_total (reference, element (v).position);
			
			items.append (new_item => (
				vertex		=> element (v),
				distance	=> d));
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

	
	function is_inside (v : pac_vertices.cursor) return boolean is begin
		if element (v).category = REGULAR then
			if element (v).location = INSIDE then
				return true;
			else
				return false;
			end if;
		else
			return false;
		end if;
	end is_inside;
	

	function is_outside (v : pac_vertices.cursor) return boolean is begin
		if element (v).category = REGULAR then
			if element (v).location = OUTSIDE then
				return true;
			else
				return false;
			end if;
		else
			return false;
		end if;
	end is_outside;

	
	
	procedure remove_redundant_positions (vertices : in out pac_vertices.list) is
		-- Since we start with the first vertex (of given list)
		-- we must compare with the position of the last vertex (of given list):
		last_position : type_vector := vertices.last_element.position;

		-- This is the new list that will finally overwrite the given list:
		v_list_new : pac_vertices.list;

		procedure query_vertex (v : in pac_vertices.cursor) is 
			-- This is the x/x position of the candidate vertex:
			candidate_position : constant type_vector := element (v).position;
		begin
			-- If the candidate does not sit on the previous vertex
			-- then append it to the new list:
			if candidate_position /= last_position then
				v_list_new.append (element (v));

				-- Update last position so that the next vertex will be
				-- compared with that position:
				last_position := candidate_position;
			end if;

			-- If the candidate has the same position as the previous vertex
			-- then it will be ignored.
		end query_vertex;
		
	begin
		-- Probe the given vertices one by one:
		vertices.iterate (query_vertex'access);

		-- Overwrite the given vertices by our new list:
		vertices := v_list_new;
	end remove_redundant_positions;


	function to_polygon (
		vertices : in pac_vertices.list;
		clean_up : in type_clean_up := true)
		return type_polygon
	is
		vertices_cleaned_up : pac_vertices.list := vertices;
		result : type_polygon;
		
		procedure query_vertex (v : in pac_vertices.cursor) is 
			edge : type_edge;
		begin
			-- The candidate vertex becomes the end of 
			-- the edge:
			edge.end_point := element (v).position;

			-- The vertex before the candidate vertex 
			-- will be the start of the edge:
			if v = vertices_cleaned_up.first then
				edge.start_point := element (vertices_cleaned_up.last).position;
			else
				edge.start_point := element (previous (v)).position;
			end if;
			
			append (result.edges, edge);
		end query_vertex;
		
	begin
		-- Clean up the given vertices:
		remove_redundant_positions (vertices_cleaned_up);

		
		-- Convert the list of vertices to a list of lines (or edges):
		vertices_cleaned_up.iterate (query_vertex'access);

		-- Whatever the order of the given vertices was,
		-- return a polygon with default winding:
		set_winding (result);

		
		if clean_up then
			-- merge successive edges running into the same direction
			optimize_edges (result);

			-- merge successive edges running into the opposide direction
			merge_overlapping_edges (result);
		end if;
		
		return result;
	end to_polygon;



	

	function get_intersections (
		polygon_A	: in type_polygon;
		polygon_B	: in type_polygon;
		debug		: in boolean := false)
		return pac_intersections.list
	is
		intersections : pac_intersections.list;
		use pac_line_edge_intersections;

		use pac_edge_status_list;
		status_list : pac_edge_status_list.list;

		-- This procedure computes the status of a single A-edge to polygon B.
		-- The status will then be appended to the status_list:
		procedure query_edge (a : in pac_edges.cursor) is begin
			status_list.append (get_edge_status (polygon_B, element (a)));
		end query_edge;
	
		
		procedure query_status (sts_candidate : in pac_edge_status_list.cursor) is
			-- This is just a shortcut to the candidate status:
			sts : type_edge_status renames element (sts_candidate);

			-- Special case 1:
			-- If the start point of the edge is on a vertex or on an edge,
			-- then this point could be an entering or leaving intersection.
			-- To make sure it is definitely entering or leaving, the status of the
			-- previous edge must be checked:
			sts_tmp : pac_edge_status_list.cursor;

			-- Regarding special case 1:
			-- It must be figured out whether the the last section of the previous edge
			-- is inside or outside polygon B.
			last_section : type_location;
			
			-- Likewise the first section of the candidate edge:
			first_section : type_location;

			
			-- This procedure builds an intersection as given in the edge-to-polygon status
			-- to an intersection required for the result:
			procedure build_intersection (i : pac_line_edge_intersections.cursor) is begin
				intersections.append ((
					type_intersection_base (element (i)) with -- position and direction
					edge_A => sts.edge, -- the candidate edge of polygon A
					edge_B => element (element (i).edge))); -- the intersected edge of polygon B

					if debug then
						put_line ("intersection: " & to_string (intersections.last_element));
					end if;
				
			end build_intersection;


			procedure build_leaving is begin
				-- If start point is on edge of polygon B then build a leaving
				-- intersection here. The affected edge of polygon B is the
				-- edge that comes right after the the intersection:
				if sts.start_point.location = ON_EDGE then
					intersections.append ((
						position	=> sts.edge.start_point,
						direction	=> LEAVING,
						edge_A		=> sts.edge,
						edge_B		=> element (sts.start_point.edge)));

				else
				-- If start point is on a vertex of polygon B then build a leaving
				-- intersection here. The affected edge of polygon B is the
				-- edge that comes right after the the intersection:
					intersections.append ((
						position	=> sts.edge.start_point,
						direction	=> LEAVING,
						edge_A		=> sts.edge,
						edge_B		=> element (sts.start_point.edges.edge_2)));

				end if;

				if debug then
					put_line ("intersection: " & to_string (intersections.last_element));
				end if;
			end build_leaving;
	

			procedure build_entering is begin
				-- If start point is on edge of polygon B then build an entering
				-- intersection here. The affected edge of polygon B is the
				-- edge that comes right after the the intersection:
				if sts.start_point.location = ON_EDGE then
					intersections.append ((
						position	=> sts.edge.start_point,
						direction	=> ENTERING,
						edge_A		=> sts.edge,
						edge_B		=> element (sts.start_point.edge)));

				else
				-- If start point is on a vertex of polygon B then build an entering
				-- intersection here. The affected edge of polygon B is the
				-- edge that comes right after the the intersection:
					intersections.append ((
						position	=> sts.edge.start_point,
						direction	=> ENTERING,
						edge_A		=> sts.edge,
						edge_B		=> element (sts.start_point.edges.edge_2)));

				end if;

				if debug then
					put_line ("intersection: " & to_string (intersections.last_element));
				end if;
			end build_entering;

			
		begin -- query_status
			if debug then
				put_line (to_string (sts));
			end if;

			
			-- Test the location of the start point of the candidate edge:
			case sts.start_point.location is
				when OUTSIDE | INSIDE =>
					null; -- no special case. nothing special to do.

				when ON_EDGE | ON_VERTEX =>
					-- Candidate edge starts on an edge or on a vertex of polygon B.
					-- So it is not definitely clear whether this intersection is leaving or entering.
					-- Look at last section of predecessing edge:
					sts_tmp := get_previous_status (status_list, sts_candidate);
					last_section := get_section_location (polygon_B, sts_tmp, LAST);
					
					-- Look at first section of the candidate edge:
					first_section := get_section_location (polygon_B, sts_candidate, FIRST);
					
					
					-- Now with the two flags last_section and first_section we get
					-- some possible scenarios:
					case last_section is
						when INSIDE =>
							case first_section is
								when INSIDE =>
									-- A change from inside to inside -> no intersection.
									null;
									
								when OUTSIDE =>
									-- A change from inside to outside -> leaving intersection.
									build_leaving;

								when others =>  -- on edge or on vertex
									--raise constraint_error;
									build_leaving;
							end case;

							
						when OUTSIDE =>
							case first_section is
								when INSIDE =>
									-- A change from outside to inside -> entering intersection.
									build_entering;
									
								when OUTSIDE =>
									-- A change from outside to outside -> no intersection.
									null;

								when others =>  -- on edge or on vertex
									--raise constraint_error;
									build_entering;
							end case;


						when others => -- on edge or on vertex
							case first_section is
								when INSIDE =>
									-- A change from outside to inside -> entering intersection.
									build_entering;
									
								when OUTSIDE =>
									build_leaving;

								when others =>  -- on edge or on vertex
									--raise constraint_error;
									null;
							end case;

					end case;
					
			end case;

			-- Build all remaining intersections that are AFTER the start point of the 
			-- candidate edge:
			sts.intersections.iterate (build_intersection'access);


			if debug then
				new_line;
			end if;
		end query_status;

		
	begin		
		if debug then
			put_line ("getting intersections ...");
		end if;
			
		-- Traverse the edges of polygon A and fill the status_list:
		polygon_A.edges.iterate (query_edge'access);

		-- Now by examining the status of individual edges we build the resulting
		-- list of intersections:
		status_list.iterate (query_status'access);
		
		return intersections;
	end get_intersections;


	
	function get_overlap_status (
		polygon_A, polygon_B	: in type_polygon;
		intersections			: in pac_intersections.list;
		debug					: in boolean := false)
		return type_overlap_status
	is
		result : type_overlap_status;
		--real_intersections : pac_intersections.list;
	begin
		if are_congruent (polygon_A, polygon_B) then
			result := CONGRUENT;
		else
			-- The given list of intersections may include places where 
			-- the two polygons just touch but do not really intersect.
			-- Extract the nodes (from given list of intersection) where
			-- the edges of the two polygons truly intersect.
			--real_intersections := get_real_intersections (intersections);
			
			-- CS: intuitively get_real_intersections is no longer required
			-- and causes more harm than good. Instead we just look at the
			-- number of given intersections:
			
			--case real_intersections.length is
			case intersections.length is
				when 0 => -- no intersections of edges or vertices
					
					if all_vertices_of_A_inside_B (polygon_A, polygon_B) then
						result := A_INSIDE_B;
						
					elsif all_vertices_of_A_inside_B (polygon_B, polygon_A) then
						result := B_INSIDE_A;
						
					else
						-- A and B do not overlap. They are apart from each other:
						result := A_DOES_NOT_OVERLAP_B;
					end if;
					
				--when 1 => raise constraint_error; -- CS should never happen

				when others =>
					result := A_OVERLAPS_B;

			end case;
		end if;
		
		if debug then
			put_line ("overlap status: " & type_overlap_status'image (result));
		end if;

		return result;
	end get_overlap_status;



	function get_overlap_status (
		polygon_A, polygon_B	: in type_polygon;
		debug					: in boolean := false)
		return type_overlap_status
	is begin
		return get_overlap_status (
			polygon_A		=> polygon_A,
			polygon_B		=> polygon_B, 
			intersections	=> get_intersections (polygon_A, polygon_B),
			debug			=> debug);
	end get_overlap_status;

		
	
	function get_polygons (
		area		: in type_polygon;
		polygons	: in out pac_polygon_list.list;
		status		: in pac_overlap_status.set;
		delete		: in boolean := true)
		return pac_polygon_list.list
	is
		use pac_polygon_list;
		result : pac_polygon_list.list;

		-- If required by caller, cursors of affected polygons
		-- are collected here for later removal:
		package pac_processed is new doubly_linked_lists (pac_polygon_list.cursor);
		use pac_processed;
		processed : pac_processed.list;


		-- This procedure tests whether the given candidate polygon
		-- is inside the given area. If so then the affected polygon
		-- is collected in list "result":
		procedure query_polygon (p : in pac_polygon_list.cursor) is

			status_candidate : constant type_overlap_status :=
				get_overlap_status (
					polygon_A		=> area,
					polygon_B		=> element (p), 
					debug			=> false);
			
		begin
			if status.contains (status_candidate) then
				result.append (element (p));

				-- Collect the cursor of the affected polygon
				-- in list "processed" if required by caller:
				if delete then
					processed.append (p);
				end if;
			end if;
		end query_polygon;


		-- This procedure queries the cursor of a processed polygon
		-- and removes the polygon from the given list "polygons":
		procedure query_processed (p : in pac_processed.cursor) is
			p_tmp : pac_polygon_list.cursor := element (p);
		begin
			polygons.delete (p_tmp);
		end query_processed;
		
	begin
		--put_line ("polygons in :" & count_type'image (polygons.length));
		polygons.iterate (query_polygon'access);

		-- Remove the affected polygons required by caller:
		if delete then
			processed.iterate (query_processed'access);
		end if;

		--put_line ("polygons out:" & count_type'image (polygons.length));
		return result;
	end get_polygons;


	
	-- Replaces three successive vertices which meet these criterions:
	-- - having same position,
	-- - the first is entering,
	-- - the second is regular,
	-- - the third is leaving
	-- by a regular vertice:
	procedure replace_entering_leaving_by_regular (
		vertices : in out pac_vertices.list)
	is
		c : pac_vertices.cursor := vertices.first;

		result : pac_vertices.list;

		function is_to_be_replaced (v1, v2, v3 : in pac_vertices.cursor) return boolean is begin
			if element (v1).position = element (v2).position
			and element (v1).position = element (v3).position
				
			and is_entering (v1) and is_regular (v2) and is_leaving (v3) then
				return true;
			else
				return false;
			end if;
		end is_to_be_replaced;

		look_ahead : boolean := true;
		
	begin
		while c /= pac_vertices.no_element loop

			if next (next (c)) = pac_vertices.no_element then
				look_ahead := false;
			end if;

			if look_ahead then
				if is_to_be_replaced (c, next (c), next (next (c))) then

					result.append (element (next (c)));
					next (c);
					next (c);
				else
					result.append (element (c));
				end if;
			else
				result.append (element (c));
			end if;
			
			next (c);
		end loop;

		vertices := result;
	end replace_entering_leaving_by_regular;
	
		
	function get_vertices (
		polygon_primary		: in type_polygon;
		polygon_secondary	: in type_polygon;
		intersections		: in pac_intersections.list;
		AB					: in type_AB_polygon)
		return pac_vertices.list
	is
		vertices : pac_vertices.list; -- to be returned
		
		-- Returns the intersection points on a given edge.
		-- Searches in list "intersections" using the supportive information
		-- of affected edges (see specs of type_intersection and function get_intersections).
		-- Orders the points by their distance to the start point of 
		-- the edge (nearest first).
		-- Intersection points can lie on the start or end point of the given line.
		-- The parameter AB determines whether to look for intersections
		-- on edges of the A or the B polygon:
		function get_intersections_on_edge (
			edge	: in type_edge)
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

		
		procedure query_edge (s : in pac_edges.cursor) is
			v_list : pac_vertices.list;
			position : type_vector;
		begin
			-- Get the intersections (on the current edge) that follow
			-- after the start point:
			v_list := get_intersections_on_edge (element (s));

			-- The first vertex to be added to the result is where
			-- the candidate edge starts:
			position := element (s).start_point;
			
			vertices.append (new_item => (
				category	=> REGULAR,
				-- Use the given secondary polygon to figure out
				-- whether the candidate vertex (of the primary polygon ) 
				-- is inside, outside, on edge or on vertex:
				location	=> get_location (polygon_secondary, position),
				position	=> position));
			
			-- Append the vertices to result:
			splice (target => vertices, before => pac_vertices.no_element, source => v_list);
		end query_edge;

		
	begin
		polygon_primary.edges.iterate (query_edge'access);

		--replace_entering_leaving_by_regular (vertices);
		return vertices;
	end get_vertices;


	function get_first (
		direction	: in type_intersection_direction;
		vertices	: in pac_vertices.list)
		return pac_vertices.cursor
	is
		v : pac_vertices.cursor;
	begin
		if not is_empty (vertices) then
			v := vertices.first;
			
			while v /= pac_vertices.no_element loop

				case direction is
					when ENTERING =>
						if is_entering (v) then
							exit;
						end if;

					when LEAVING =>
						if is_leaving (v) then
							exit;
						end if;
				end case;
				
				next (v);
			end loop;
		end if;
		
		return v;
	end get_first;


	function get_first (
		location	: in type_location;
		vertices	: in pac_vertices.list)
		return pac_vertices.cursor
	is
		v : pac_vertices.cursor;
	begin
		if not is_empty (vertices) then
			v := vertices.first;
			
			while v /= pac_vertices.no_element loop

				case location is
					when INSIDE =>
						if is_inside (v) then
							exit;
						end if;

					when OUTSIDE =>
						if is_outside (v) then
							exit;
						end if;

					when others => null;
				end case;
				
				next (v);
			end loop;
		end if;
		
		return v;
	end get_first;


	function get_vertices (
		location	: in type_location;
		vertices	: in pac_vertices.list)
		return pac_vertices.list
	is
		v_list : pac_vertices.list; -- to be returned

		procedure query_vertex (c : pac_vertices.cursor) is begin
			if is_regular (c) then
				if element (c).location = location then
					v_list.append (element (c));
				end if;
			end if;
		end query_vertex;
		
	begin
		if not is_empty (vertices) then
			vertices.iterate (query_vertex'access);
		end if;

		return v_list;
	end get_vertices;

	

	function get_until (
		vertices					: in out pac_vertices.list;
		start_vertex				: in pac_vertices.cursor;
		direction_of_intersection	: in type_intersection_direction;
		direction_of_search			: in type_direction_of_rotation; -- CW, CCW
		delete_visited				: in boolean := true)
		return pac_vertices.list
	is
		-- The list of vertices to be returned;
		result : pac_vertices.list;

		
		-- This function returns true if the given vertex
		-- does not sit on the given start_vertex:
		function not_on_start (c : in pac_vertices.cursor)
			return boolean is
		begin
			if element (c).position /= element (start_vertex).position then
				return true;
			else
				return false;
			end if;
		end not_on_start;
		
		-- This cursor points at the vertex being processed:
		v : pac_vertices.cursor;


		-- Traverses through the vertices starting where cursor
		-- v is pointing at. Appends each of them to the result.
		-- Aborts iteration on a suitable vertex. 
		-- A suitable vertex is qualified if it:
		-- - does not sit on the given start_vertex. Means is must
		--   come after the start_vertex.
		-- - is leaving/entering according to the given direction_of_intersection.
		procedure do_collect is begin
			case direction_of_search is
				when CCW => -- traverse forward
					
					while v /= pac_vertices.no_element loop
						result.append (element (v));

						if not_on_start (v) then
							case direction_of_intersection is
								when LEAVING =>
									if is_leaving (v) then
										exit;
									end if;

								when ENTERING =>
									if is_entering (v) then
										exit;
									end if;

							end case;
						end if;
						
						next (v);
					end loop;

				when CW => -- traverse backward
					while v /= pac_vertices.no_element loop
						result.append (element (v));

						if not_on_start (v) then						
							case direction_of_intersection is
								when LEAVING =>
									if is_leaving (v) then
										exit;
									end if;

								when ENTERING =>
									if is_entering (v) then
										exit;
									end if;

							end case;
						end if;
						
						previous (v);
					end loop;
			end case;
		end do_collect;


		-- Returns the number of vertices from and including
		-- the given start_vertex to the end of the list of 
		-- given vertices:
		function get_until_end return count_type is
			ct : count_type := 0;
			cu : pac_vertices.cursor := start_vertex;
		begin
			while cu /= pac_vertices.no_element loop
				ct := ct + 1;
				next (cu);
			end loop;
			return ct;
		end get_until_end;

		ct_ccw : count_type := 0;

		
		-- Returns the number of vertices from and including
		-- the given start_vertex to the begin of the list of 
		-- given vertices:
		function get_until_begin return count_type is
			ct : count_type := 0;
			cu : pac_vertices.cursor := start_vertex;
		begin
			while cu /= pac_vertices.no_element loop
				ct := ct + 1;
				previous (cu);
			end loop;
			return ct;
		end get_until_begin;

		ct_cw : count_type := 0;

		
		
		collected_vertices : count_type := 0;
		deleted_vertices : count_type := 0;
		restart_required : boolean := false;


		procedure delete_ccw is 
			c : pac_vertices.cursor;
		begin
			if not restart_required then
				vertices.delete (position => v, count => collected_vertices);
			else
				--put_line ("ct_ccw" & count_type'image (ct_ccw));
				vertices.delete (position => v, count => ct_ccw);
				c := vertices.first;
				vertices.delete (position => c, count => collected_vertices - ct_ccw);
			end if;
		end delete_ccw;

		
		procedure delete_cw is 
			c : pac_vertices.cursor;
		begin
			null;
			-- CS
			
			--if not restart_required then
				--vertices.delete (position => v, count => collected_vertices);
			--else
				----put_line ("ct_ccw" & count_type'image (ct_ccw));
				--vertices.delete (position => v, count => ct_ccw);
				--c := vertices.first;
				--vertices.delete (position => c, count => collected_vertices - ct_ccw);
			--end if;
		end delete_cw;

		
	begin
		--if delete_visited then
			--if is_triplet (start_vertex) then
				----put_line ("triplet");
						
				--v := previous (start_vertex);
				--vertices.delete (v);
				--v := previous (start_vertex);
				--vertices.delete (v);
			--end if;
		--end if;
		
		-- Preset cursor v to the given entering/leaving vertex.
		-- The search starts here:
		v := start_vertex;

		-- Collect vertices from given vertex to the next leaving/entering vertex:
		do_collect;

		case direction_of_search is
			when CCW =>
				-- If no leaving/entering vertex found until end of list,
				-- restart the search from the begin of the list:
				if v = pac_vertices.no_element then
					restart_required := true;
					ct_ccw := get_until_end;
					v := vertices.first;
					do_collect;
				end if;

			when CW =>
				-- If no leaving/entering vertex found until begin of list,
				-- restart the search from the end of the list:
				if v = pac_vertices.no_element then
					restart_required := true;
					ct_cw := get_until_begin;
					v := vertices.last;
					do_collect;
				end if;
		end case;

		
		-- If requested by the caller
		-- remove the visited vertices from given list of vertices:
		collected_vertices := length (result);
		
		if delete_visited then
			v := start_vertex;
			
			case direction_of_search is
				when CCW =>	delete_ccw;
				when CW  => delete_cw;
			end case;
		end if;

		
		-- The first item of the result is not required because
		-- this is where we have started:
		result.delete_first;

		return result;
	end get_until;


	procedure increment_safety_counter (
		count	: in out natural;
		limit	: in natural)
	is begin
		count := count + 1;

		if count >= limit then
			raise constraint_error 
			with "Safety counter overrun ! (max." & natural'image (limit) & ")";
		end if;
	end increment_safety_counter;



	function to_polygon (
		line		: in type_line;
		linewidth	: in type_float_internal_positive;
		tolerance	: in type_float_internal_positive;
		mode		: in type_approximation_mode)
		return type_polygon
	is
		result : type_polygon;

		direction : constant type_angle := get_direction (line);
		distance : constant type_float_internal_positive := linewidth * 0.5;

		center : constant type_edge := type_edge (line);
		edge_right, edge_left : type_edge := center;
		
		arc : type_arc;
		edges : pac_edges.list;
		
	begin
		-- Build the right edge and append it to the polygon as it is:
		edge_right := move_by (edge_right, add (direction, -90.0), distance);
		result.edges.append (edge_right);

		-- Build the left edge but do NOT yet append it to the polygon:
		edge_left  := move_by (edge_left,  add (direction, +90.0), distance);
		reverse_line (edge_left);

		-- Build the cap on the end of the line. The cap is an arc:
		arc.center := center.end_point;
		arc.start_point := edge_right.end_point;
		arc.end_point := edge_left.start_point;
		arc.direction := arc_direction_default;

		-- Convert the arc to a list of edges and append them to the polygon:
		edges := to_edges (arc, tolerance, mode);
		result.edges.splice (before => pac_edges.no_element, source => edges);
		-- Container edges is now empty.

		-- Now append the left edge to the polygon:
		result.edges.append (edge_left);

		-- Build the cap on the start of the line. The cap is an arc:
		arc.center := center.start_point;
		arc.start_point := edge_left.end_point;
		arc.end_point := edge_right.start_point;
		arc.direction := arc_direction_default;

		-- Convert the arc to a list of edges and append them to the polygon:
		edges := to_edges (arc, tolerance, mode);
		result.edges.splice (before => pac_edges.no_element, source => edges);

		optimize_edges (result); -- MANDATORY !!
		
		return result;
	end to_polygon;


	function to_polygon (
		arc			: in type_arc;
		linewidth	: in type_float_internal_positive;
		tolerance	: in type_float_internal_positive;
		mode		: in type_approximation_mode)
		return type_polygon
	is
		result : type_polygon;

		-- Normalize given arc so that it runs CCW.
		-- Basing on arc_n everything else will be computed.
		arc_n : constant type_arc := normalize_arc (arc);

		center_radius : constant type_float_internal_positive := get_radius_start (arc_n);
		half_width    : constant type_float_internal_positive := linewidth * 0.5;
		inner_radius, outer_radius : type_float_internal_positive;

		
		-- At first we build a contour consisting of 4 arcs. These
		-- arcs form the outer contour of an arc with a given linewidth.
		-- In the end these arcs are converted to edges of the resulting polygon.
		
		-- There will be an arc on the inner side (small radius) 
		-- and another on the outer side (great radius).
		arc_i, arc_o : type_arc_angles;

		-- There will be an arc on the start and an arc on the end.
		-- These arcs form the round caps:
		arc_s, arc_e : type_arc;

		scratch_i, scratch_o : type_arc;		

		edges : pac_edges.list;

	begin
		-- set radii
		inner_radius := center_radius - half_width;
		outer_radius := center_radius + half_width;

		-- set outer edge:
		arc_o := to_arc_angles (arc_n);
		arc_o.radius := outer_radius;
		scratch_o := to_arc (arc_o);

		-- set inner edge:
		arc_i := to_arc_angles (reverse_arc (arc_n));
		arc_i.radius := inner_radius;
		scratch_i := to_arc (arc_i);
		
		-- set cap at start point:		
		arc_s.start_point := scratch_i.end_point;
		arc_s.end_point   := scratch_o.start_point;
		arc_s.direction   := arc_direction_default;
		arc_s.center      := arc_n.start_point;
		
		-- set cap at end point:
		arc_e.start_point := scratch_o.end_point;
		arc_e.end_point   := scratch_i.start_point;
		arc_e.direction   := arc_direction_default;
		arc_e.center      := arc_n.end_point;


		--put_line ("start " & to_string (arc_s));
		--put_line ("outer " & to_string (scratch_o));
		--put_line ("end   " & to_string (arc_e));
		--put_line ("inner " & to_string (scratch_i));

		
		-- Approximate the four arcs to edges and append them to
		-- the resulting polygon:

		-- cap on start:
		edges := to_edges (arc_s, tolerance, mode);
		result.edges.splice (before => pac_edges.no_element, source => edges);

		-- outer arc:
		edges := to_edges (scratch_o, tolerance, mode);
		result.edges.splice (before => pac_edges.no_element, source => edges);

		-- cap on end:
		edges := to_edges (arc_e, tolerance, mode);
		result.edges.splice (before => pac_edges.no_element, source => edges);

		-- inner arc:
		edges := to_edges (scratch_i, tolerance, mode);
		result.edges.splice (before => pac_edges.no_element, source => edges);

		optimize_edges (result); -- MANDATORY !!
		
		return result;
	end to_polygon;
	
	
end et_geometry_1.et_polygons;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

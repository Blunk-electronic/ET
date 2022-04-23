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

with et_exceptions;				use et_exceptions;


package body et_geometry_2.polygons is


	function to_string (
		edge : in type_edge)
		return string
	is begin
		return "edge: start: " & to_string (edge.start_point) 
			& " end: " & to_string (edge.end_point);
	end to_string;
	

	function to_edge (
		line : in type_line)
		return type_edge
	is
		result : type_edge;
	begin
		result.start_point := to_vector (line.start_point);
		result.end_point := to_vector (line.end_point);
		return result;
	end to_edge;
	

	function to_line (
		edge : in type_edge)
		return type_line
	is
		result : type_line;
	begin
		result.start_point := to_point (edge.start_point);
		result.end_point := to_point (edge.end_point);
		return result;
	end to_line;


	function reverse_edge (
		edge	: in type_edge)
		return type_edge 
	is begin
		return (edge.end_point, edge.start_point);
	end reverse_edge;

	
	function get_center (
		edge : in type_edge)
		return type_vector
	is
		result : type_vector;
		dp : type_vector; -- displacement vector
	begin
		-- get the displacement:
		dp := get_displacement (edge.start_point, edge.end_point);

		-- halve the displacement
		dp := scale (dp, 0.5);

		-- move start point by displacement
		result := add (edge.start_point, dp);
		return result;
	end get_center;


	function get_direction (
		edge : in type_edge)
		return type_rotation
	is
		dp : type_vector;
	begin
		-- get the displacement:
		dp := get_displacement (edge.start_point, edge.end_point);

		-- NOTE: If dx and dy are zero then the arctan operation is not possible. 
		-- In this case we assume the resulting angle is zero.
		if dp.x = 0.0 and dp.y = 0.0 then
			return zero_rotation;
		else
			return to_rotation (arctan (dp.y, dp.x, units_per_cycle));
		end if;
	end get_direction;
		

	procedure move_by (
		edge		: in out type_edge;
		direction	: in type_rotation;
		distance	: in type_float_internal_positive)
	is begin
		move_by (
			v 			=> edge.start_point, 
			direction	=> direction, 
			distance	=> distance);
	end move_by;

	
	procedure move_by (
		edge		: in out type_edge;
		direction	: in type_rotation;
		distance	: in type_distance_positive)
	is begin
		move_by (
			v 			=> edge.start_point, 
			direction	=> direction, 
			distance	=> type_float_internal_positive (distance));
	end move_by;


	function to_line_vector (
		edge	: in type_edge)
		return type_line_vector
	is
		result : type_line_vector;
		dp : type_vector;
	begin
		dp := get_displacement (edge.start_point, edge.end_point);
		
		result.v_start := edge.start_point;
		result.v_direction := dp;
		return result;
	end to_line_vector;


	function get_distance (
		edge	: in type_edge;
		vector	: in type_vector)
		return type_float_internal
	is
		dv : constant type_vector := to_line_vector (edge).v_direction;
		sv : constant type_vector := edge.start_point;
		
		d1 : constant type_vector := subtract (vector, sv);
		m, n : type_float_internal;
	begin
		m := absolute (cross_product (dv, d1));
		n := absolute (dv);
		
		return (m / n);
	end get_distance;



	function get_distance (
		vector		: in type_vector;
		edge		: in type_edge;
		line_range	: in type_line_range)
		return type_distance_point_line 
	is
		result : type_distance_point_line;
	
		-- Imagine a line that starts on the given location vector,
		-- travels perpendicular towards
		-- the given line and finally intersects the given line somewhere.
		-- The intersection may be betweeen the start and end point of the given line.
		-- The intersection may be virtual, before start or after end point 
		-- of the given line.
		
		line_direction : constant type_rotation := get_direction (edge);
		line_direction_vector : constant type_vector := to_line_vector (edge).v_direction;
		line_start_vector, line_end_vector : type_vector;

		iv : type_vector;

		procedure compute_intersection is 
			distance : type_float_internal;
		begin
			-- Compute the point of intersection: The intersection of a line that runs
			-- from the given location vector perpendicular to the given line.
			-- For the moment we do not know which direction to go. So we just try
			-- to go in 90 degree direction. If the distance from iv to the line
			-- is not zero, then we try in -90 degree direction.

			iv := vector;
			move_by (iv, line_direction + 90.0, result.distance);

			distance := get_distance (edge, iv);
			--log (text => "delta  :" & type_float_internal'image (distance));
			
			if distance > rounding_threshold then
				--put_line ("wrong direction");
				
				-- we went the wrong direction
				iv := vector; -- restore iv

				-- try opposite direction:
				move_by (iv, line_direction - 90.0, result.distance);
			end if;

			--put_line ("iv" & to_string (iv));
			
			-- Assign the direction (from point to intersection) to the result:
			result.direction := get_angle (get_distance (vector, iv));
			--put_line ("direction" & to_string (result.direction));

			-- Assign the virtual point of intersection to the result:
			result.intersection := iv;
		end compute_intersection;
		
		lambda_forward, lambda_backward : type_float_internal;
	begin
		--put_line ("line direction vector: " & to_string (line_direction_vector));
		--put_line ("line direction angle : " & to_string (line_direction));
		
		-- The first and simplest test is to figure out whether
		-- the given point sits exactly on the start or end point of the line.
		-- Mind: result.distance has default zero.
		-- This test includes the start and end points of the line. 
		-- On match we exit this function prematurely and return the result
		-- with the appropiate flags set.
		case line_range is
			when WITH_END_POINTS | BEYOND_END_POINTS =>
				
				if vector = edge.start_point then
					
					result.sits_on_start := true;
					result.out_of_range := false;
					return result;

				elsif vector = edge.end_point then
					
					result.sits_on_end := true;
					result.out_of_range := false;
					return result;

				end if;
				
			when others => null;
		end case;

		---- If the ends of the line are included,
		---- test whether the point is in the vicinity of the line start or end point.
		---- Exit this function prematurely in that case.
		--if line_range = WITH_END_POINTS then

			---- compute distance of point to start of line:
			--result.distance := get_distance_total (point, line.start_point);
			
			--if result.distance <= accuracy then
				--result.out_of_range := false;
				--return result;
			--end if;

			--if point = line.start_point then
				--result.out_of_range := false;
				--return result;
			--end if;
				

			---- compute distance of point to end of line:
			--result.distance := get_distance_total (point, line.end_point);
			
			--if result.distance <= accuracy then
				--result.out_of_range := false;
				--return result;
			--end if;

		--end if;
		
		-- Compute the distance from the given point to the given line.
		-- This computation does not care about end or start point of the line.
		-- It assumes an indefinite long line without start or end point.
		result.distance := get_distance (edge, vector);

		--put_line ("distance " & to_string (result.distance));

		-- Set iv so that it points to the intersection. The
		-- intersection can be anywhere on that indefinite long line.
		compute_intersection;

		
		-- Any point on a line can be computed by this formula (see textbook on vector algebra):
		-- iv = line.start_point + lambda_forward  * line_direction_vector
		-- iv = line.end_point   + lambda_backward * line_direction_vector

		-- Using these formula we can calculate whether iv points between 
		-- (or to) the start and/or end points of the line:
		
		line_start_vector := edge.start_point;
		lambda_forward := divide (subtract (iv, line_start_vector), line_direction_vector);

		--put_line ("lambda forward:" & to_string (lambda_forward));
		
		if lambda_forward < 0.0 then -- iv points BEFORE start of line
			--put_line ("before start point");
			case line_range is
				when BEYOND_END_POINTS => result.out_of_range := false;
				when others => result.out_of_range := true;
			end case;

			return result; -- no more computations required
		end if;
		
		if lambda_forward = 0.0 then -- iv points TO start point of line
			--put_line ("on start point");
			case line_range is
				when BETWEEN_END_POINTS => result.out_of_range := true;
				when others => result.out_of_range := false;
			end case;

			return result; -- no more computations required
		end if;

		--put_line ("after start point");

		
		line_end_vector := edge.end_point;
		lambda_backward := divide (subtract (iv, line_end_vector), line_direction_vector);

		--put_line ("lambda backward:" & to_string (lambda_backward));
		
		if lambda_backward > 0.0 then -- iv points AFTER end of line
			--put_line ("after end point");
			case line_range is
				when BEYOND_END_POINTS => result.out_of_range := false;
				when others => result.out_of_range := true;
			end case;

			return result; -- no more computations required
		end if;

		if lambda_backward = 0.0 then -- iv points TO end point of line
			--put_line ("on end point");
			case line_range is
				when BETWEEN_END_POINTS => result.out_of_range := true;
				when others => result.out_of_range := false;
			end case;

			return result; -- no more computations required
		end if;

		--put_line ("before end point");

		result.out_of_range := false;

		return result;
	end get_distance;

	
	
	function get_shortest_distance (
		vector	: in type_vector;
		edge	: in type_edge)
		return type_float_internal
	is
		result : type_float_internal := 0.0;
		
		d : constant type_distance_point_line := get_distance (
			vector		=> vector,
			edge		=> edge,
			line_range	=> WITH_END_POINTS);

		d_to_start, d_to_end : type_float_internal;
	begin
		--put_line ("point" & to_string (point) & " " & to_string (line));
		
		if on_start_point (d) or on_end_point (d) then
			-- Point is on top of start or end point of line.
			--log (text => "on start or end");
			null; -- result keeps its default (zero distance, zero angle)
		else

			--if on_line (get_intersection (d), line) then 
			if not out_of_range (d) then
				
				-- An imaginary line can be drawn perpendicular from
				-- point to line. Both intersect each other.
				result := get_distance (d);
			else
				
				-- No imaginary line can be drawn perpendicular from
				-- point to line.

				-- Compare the distances to the end points of the line:
				d_to_start := get_distance_total (edge.start_point, vector);
				d_to_end   := get_distance_total (edge.end_point, vector);

				if d_to_start < d_to_end then
					result := d_to_start;
				else
					result := d_to_end;
				end if;
				
			end if;

		end if;

		--put_line (to_string (result));
		
		return result;
	end get_shortest_distance;



	function on_edge (
		vector	: in type_vector;
		edge	: in type_edge)
		return boolean
	is
		distance : type_distance_point_line;
	begin
		distance := get_distance (vector, edge, WITH_END_POINTS);
		
		if not distance.out_of_range and distance.distance < rounding_threshold then
			return true;
		else
			return false;
		end if;
	end on_edge;



	function get_nearest (
		edge	: in type_edge;
		vector	: in type_vector;
		place	: in type_nearest := AFTER)
		return type_vector
	is
		result : type_vector := vector;
		d : constant type_distance_polar := get_distance (edge.start_point, edge.end_point);
	begin
		case place is
			when AFTER => -- move forward in direction of line
				--result := type_point (move (
					--point		=> point,
					--direction	=> get_angle (d),
					--distance	=> m * type_distance'small));

				move_by (
					v			=> result,
					direction	=> get_angle (d),
					distance	=> type_float_internal (type_distance'small));
					--distance	=> type_float_internal (type_float_internal'small)); -- CS

			when BEFORE => -- move backward in opposite direction
				--result := type_point (move (
					--point		=> point,
					--direction	=> add (get_angle (d), 180.0),
					--distance	=> m * type_distance'small));

				move_by (
					v			=> result,
					direction	=> add (get_angle (d), 180.0),
					distance	=> type_float_internal (type_distance'small));
					--distance	=> type_float_internal (type_float_internal'small)); -- CS
				
		end case;
		return result;
	end get_nearest;



	

	function lines_overlap (
		edge_1, edge_2 : in type_edge)
		return boolean
	is
		I2L : constant type_intersection_of_two_lines :=
			get_intersection (edge_1, edge_2);
	begin
		if I2L.status = OVERLAP then
			return true;
		else
			return false;
		end if;
	end lines_overlap;


	
	function get_intersection (
		line : in type_line_vector;
		edge : in type_edge)
		return type_intersection_of_two_lines
	is
		i : constant type_intersection_of_two_lines := get_intersection (
				line_1	=> line,
				line_2	=> to_line_vector (edge));
		
	begin
		case i.status is
			when EXISTS =>
				--put_line ("exists");
				--put_line (to_string (probe_line));
				--put_line (to_string (candidate_line));
				--put_line (to_string (i.intersection.point));
				
				-- The intersection must be between start and end point of
				-- the candidate line (start and end point itself included).
				-- If the intersection is between start and end point
				-- of candidate line, then return the intersection as it is.
				-- If the intersection is before start point or
				-- beyond end point, then return NOT_EXISTENT.
				if on_edge (i.intersection.vector, edge) then
					return i;
				else
					return (status => NOT_EXISTENT);
				end if;

			when others =>		
				return i;
		end case;
		
	end get_intersection;


	function get_intersection (
		edge_1 : in type_edge;
		edge_2 : in type_edge)
		return type_intersection_of_two_lines
	is
		lv_1 : constant type_line_vector := to_line_vector (edge_1);
		lv_2 : constant type_line_vector := to_line_vector (edge_2);

		int_A : constant type_intersection_of_two_lines := get_intersection (lv_1, edge_2);
		int_B : constant type_intersection_of_two_lines := get_intersection (lv_2, edge_1);

		status : type_intersection_status_of_two_lines;
		intersection : et_geometry_2.type_intersection;
		
	begin
		--if int_A.status = NOT_EXISTENT or int_B.status = NOT_EXISTENT then
			--status := NOT_EXISTENT;

		if int_A.status = OVERLAP and int_B.status = OVERLAP then -- CS ? correct ?
			status := OVERLAP;
			
		elsif int_A.status = EXISTS and int_B.status = EXISTS then

			-- double check: location vectors must match !
			if get_absolute (get_distance (int_A.intersection.vector, int_B.intersection.vector)) = 0.0 then
				status := EXISTS;
				intersection.vector := int_A.intersection.vector;
				intersection.angle := int_A.intersection.angle;
			else
				raise constraint_error with 
					"Intersection point mismatch: " & to_string (int_A.intersection.vector)
					& to_string (int_B.intersection.vector);
			end if;

		else
			status := NOT_EXISTENT;
		end if;


		case status is
			when NOT_EXISTENT =>
				return (status => NOT_EXISTENT);

			when OVERLAP =>
				return (status => OVERLAP);

			when EXISTS =>
				return (
					status			=> EXISTS,
					intersection	=> intersection);	   
		end case;

	end get_intersection;


	
	function crosses_threshold (
		edge : in type_edge;
		y_th : in type_float_internal)
		return boolean
	is begin
		if	
			edge.start_point.y >= y_th and 
			edge.end_point.y   <  y_th then
			return true;
			
		elsif
			edge.end_point.y   >= y_th and 
			edge.start_point.y <  y_th then
			return true;
			
		else
			return false;
		end if;
	end crosses_threshold;

	
	
	function get_boundaries (
		edge	: in type_edge)
		return type_boundaries
	is 
		result : type_boundaries;
	begin
		-- X axis
		if edge.start_point.x = edge.end_point.x then -- both ends on a vertical line

			result.smallest_x := edge.start_point.x;
			result.greatest_x := edge.start_point.x;
			
		elsif edge.start_point.x < edge.end_point.x then
			
			result.smallest_x := edge.start_point.x;
			result.greatest_x := edge.end_point.x;
		else
			result.smallest_x := edge.end_point.x;
			result.greatest_x := edge.start_point.x;
		end if;

		-- Y axis
		if edge.start_point.y = edge.end_point.y then -- both ends on a horizontal line

			result.smallest_y := edge.start_point.y;
			result.greatest_y := edge.start_point.y;
			
		elsif edge.start_point.y < edge.end_point.y then
			
			result.smallest_y := edge.start_point.y;
			result.greatest_y := edge.end_point.y;
		else
			result.smallest_y := edge.end_point.y;
			result.greatest_y := edge.start_point.y;
		end if;

		
		return result;
	end get_boundaries;

	
	
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
	

	function to_polygon (vertices : in string)
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
		-- Iterate all fields of given list of arguments:
		while place <= field_count (v_fields) loop

			v.position.x := type_float_internal'value (f (place));
			v.position.y := type_float_internal'value (f (place + 1));

			v_list.append (v);
			
			place := place + 2;
		end loop;

		return to_polygon (v_list);
	end to_polygon;


	
	function get_boundaries (
		polygon		: in type_polygon;
		line_width	: in type_distance_positive)
		return type_boundaries
	is
		result : type_boundaries; -- to be returned

		half_width : constant type_float_internal_positive := type_float_internal (line_width) * 0.5;

		procedure query_edge (c : in pac_edges.cursor) is begin
			union (result, get_boundaries (element (c)));
		end query_edge;
		
	begin
		polygon.edges.iterate (query_edge'access);
					
		-- Extend the boundaries by half the line width;
		result.smallest_x := result.smallest_x - half_width;
		result.smallest_y := result.smallest_y - half_width;

		result.greatest_x := result.greatest_x + half_width;
		result.greatest_y := result.greatest_y + half_width;
		
		return result;
	end get_boundaries;
	

	
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
				edge_new := reverse_edge (element (c));
				
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
		polygon_A, polygon_B : in type_polygon)
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
				if element (edge_A_cursor) /= element (edge_B_cursor) then
					proceed := false;
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
		-- The first and easiest test is to compare the number of edges.
		-- If they differ, then the polygons are definitely not congruent:
		if ct_A /= ct_B then
			result := false;
		else
			-- Get the first segment of polygon A:
			edge_A := polygon_A.edges.first_element;

			-- Search for that element in polygon B:
			edge_B_cursor := polygon_B.edges.find (edge_A);

			-- If polygon B contains this edge then proceed comparing
			-- the segments from this position on.
			-- If polygon B does not contain this starting edge then
			-- the polygons are not congruent:
			if edge_B_cursor /= pac_edges.no_element then
				compare_edges;
			else
				-- not congruent
				result := false;
			end if;
		end if;
		
		return result;
	end are_congruent;

	

	function get_segment_edge (
		polygon	: in type_polygon;
		edge	: in type_edge)
		return pac_edges.cursor
	is
		result : pac_edges.cursor;
	begin
		result := polygon.edges.find (edge);
		return result;
	end get_segment_edge;
	


	--function get_segment_edge (
		--polygon	: in type_polygon;
		--point	: in type_point)
		--return pac_edges.cursor
	--is
		--result : pac_edges.cursor;
		--proceed : aliased boolean := true;

		----procedure query_segment (c : in pac_edges.cursor) is begin
			----case element (c).shape is
				----when LINE => 
					----if on_line (point, element (c).segment_line) then
						----result := c;
						----proceed := false; -- abort iteration
					----end if;
					
				----when ARC => null; -- CS
			----end case;
		----end query_segment;

		--procedure query_edge (c : in pac_edges.cursor) is begin
			--if on_line (point, element (c)) then
				--result := c;
				--proceed := false; -- abort iteration
			--end if;
		--end query_edge;

		
	--begin
		---- Make sure the given point is NOT a vertex:
		---- CS: Maybe no need if caller cares for this check.
		--if is_vertex (polygon, point) then
			--raise constraint_error with "Point is a vertex !";
		--else
			--iterate (polygon.edges, query_edge'access, proceed'access);					 
		--end if;

		---- CS exception message if polygon consists of just a circle.
		--return result;
	--end get_segment_edge;





	
	
	--function get_neigboring_edges (
		--polygon	: in type_polygon;
		--vertex	: in type_point)
		--return type_neigboring_edges
	--is
		--result : type_neigboring_edges;
		--proceed : aliased boolean := true;

		--end_found, start_found : boolean := false;
		

		--procedure query_edge (c : in pac_edges.cursor) is begin
			----put_line ("test: " & to_string (element (c)));
			
			--if element (c).end_point = vertex then
				----put_line ("end");
				--result.edge_1 := c;
				--end_found := true;
			--end if;

			--if element (c).start_point = vertex then
				----put_line ("start");
				--result.edge_2 := c;
				--start_found := true;
			--end if;

			---- Abort iteration once start and end point have been found
			---- ("proceed" is low-active):
			--proceed := not (end_found and start_found);
	
		--end query_edge;

		
	--begin
		---- Make sure the given point IS a vertex:
		--if is_vertex (polygon, vertex) then
			--iterate (polygon.edges, query_edge'access, proceed'access);					 
		--else
			--raise constraint_error with "Point is a vertex !";
		--end if;

		---- Safety check:
		---- Two edges must have been found. Otherwise raise exception:
		--if result.edge_1 = pac_edges.no_element 
		--or result.edge_2 = pac_edges.no_element
		--then
			--raise constraint_error with "Search for neigboring edges incomplete !";
		--end if;

		--return result;
	--end get_neigboring_edges;




	

	
	function to_string (
		polygon	: in type_polygon)
		return string
	is
		use ada.strings.unbounded;
		
		result : unbounded_string := to_unbounded_string ("polygon:");

		procedure query_edge (c : in pac_edges.cursor) is begin

			-- We output the start points only.
			-- Because: The end point of an edge is always the start point of the
			-- next edge.
			
			result := result & space
				& to_unbounded_string ("edge start:" & to_string (element (c).start_point));

		end query_edge;

		
	begin
		polygon.edges.iterate (query_edge'access);
		
		return to_string (result);
	end to_string;



	
	
	--function get_nearest_corner_point (
		--polygon		: in type_polygon;
		--reference	: in type_point)
		--return type_point
	--is
		--result : type_point := origin;
		
		--d1 : type_distance_positive := zero;
		--d2 : type_distance_positive := get_absolute (get_distance (reference, far_upper_right));

		--procedure query_segment (c : in pac_polygon_segments.cursor) is
			--s : constant type_polygon_segment := element (c);
		--begin
			--case s.shape is
				--when LINE =>

					---- test start point
					--d1 := get_absolute (get_distance (reference, s.segment_line.start_point));
					
					--if d1 < d2 then
						--d2 := d1;
						
						--result := s.segment_line.start_point;
					--end if;

					---- test end point
					--d1 := get_absolute (get_distance (reference, s.segment_line.end_point));
					
					--if d1 < d2 then
						--d2 := d1;
						
						--result := s.segment_line.end_point;
					--end if;

				--when ARC =>
					---- test start point
					--d1 := get_absolute (get_distance (reference, s.segment_arc.start_point));
					
					--if d1 < d2 then
						--d2 := d1;
						
						--result := s.segment_arc.start_point;
					--end if;

					---- test end point
					--d1 := get_absolute (get_distance (reference, s.segment_arc.end_point));
					
					--if d1 < d2 then
						--d2 := d1;
						
						--result := s.segment_arc.end_point;
					--end if;
					
			--end case;
		--end query_segment;

		--procedure query_edge (c : in pac_edges.cursor) is
			--s : constant type_line := element (c);
		--begin
			---- test start point
			--d1 := get_absolute (get_distance (reference, s.start_point));
			
			--if d1 < d2 then
				--d2 := d1;
				
				--result := s.start_point;
			--end if;

			---- test end point
			--d1 := get_absolute (get_distance (reference, s.end_point));
			
			--if d1 < d2 then
				--d2 := d1;
				
				--result := s.end_point;
			--end if;
		--end query_edge;
		
	--begin
		--polygon.edges.iterate (query_edge'access);				
		
		--return result;
	--end get_nearest_corner_point;

	



	

	function get_edges_total (
		polygon : in type_polygon)
		return count_type
	is begin
		return length (polygon.edges);
	end get_edges_total;

	
	

	
	
	

	
	--function to_string (scale : in type_polygon_scale) return string is begin
		--return type_polygon_scale'image (scale);
	--end to_string;

	--function to_scale (scale : in string) return type_polygon_scale is begin
		--return type_polygon_scale'value (scale);
	--end to_scale;



	
	
	--procedure scale_polygon (
		--polygon	: in out type_polygon;
		--scale	: in type_polygon_scale) 
	--is
		
		--function scale_point (point	: in type_point) return type_point is
			--x_new : type_distance := get_x (point) * scale;
			--y_new : type_distance := get_y (point) * scale;
		--begin
			--return type_point (set (x_new, y_new));
		--end scale_point;

		
		
		--procedure do_segment (c : in pac_polygon_segments.cursor) is

			
			--procedure do_line (s : in out type_polygon_segment) is begin 
				--s.segment_line.start_point	:= scale_point (s.segment_line.start_point);
				--s.segment_line.end_point	:= scale_point (s.segment_line.end_point);
			--end;

			
			--procedure do_arc (s : in out type_polygon_segment) is begin
				--null; -- CS
			--end;

			
		--begin
			--case element (c).shape is
				
				--when LINE =>
					--update_element (
						--container	=> polygon.contours.segments,
						--position	=> c,
						--process		=> do_line'access);

				--when ARC =>
					--update_element (
						--container	=> polygon.contours.segments,
						--position	=> c,
						--process		=> do_arc'access);

			--end case;
		--end do_segment;

		
	--begin

		--if polygon.contours.circular then

			---- scale the single circle that forms the polygon:
			---- CS change radius of  polygon.contours.circle
			--null;
		--else
			--polygon.contours.segments.iterate (do_segment'access);
		--end if;
		
	--end scale_polygon;

	

	--function is_vertex (
		--polygon	: in type_polygon;
		--point	: in type_point)
		--return boolean
	--is
		--proceed : aliased boolean := true;

		--procedure query_edge (c : in pac_edges.cursor) is begin
			----case element (c).shape is
				----when LINE =>
					----if element (c).segment_line.start_point = point then
						----proceed := false;
					----end if;
					
				----when ARC =>
					----if element (c).segment_arc.start_point = point then
						----proceed := false;
					----end if;
			
			----end case;

			--if element (c).start_point = point then
				--proceed := false;
			--end if;
		--end query_edge;
		
	--begin
		--iterate (
			--edges	=> polygon.edges,
			--process	=> query_edge'access,
			--proceed	=> proceed'access);

		--return not proceed;
	--end is_vertex;

	
	
	
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



	function get_segment_edge (
		polygon	: in type_polygon;
		point	: in type_vector)
		return pac_edges.cursor
	is
		result : pac_edges.cursor;
		proceed : aliased boolean := true;

		procedure query_edge (c : in pac_edges.cursor) is begin
			if on_edge (point, element (c)) then
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
	end get_segment_edge;

	
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


	
	
	
	function "<" (left, right : in type_probe_line_intersection_polygon)
		return boolean
	is
		result : boolean := false;
	begin
		if left.x_position < right.x_position then
			result := true;
		else
			result := false;
		end if;

		-- CS compare angles ?
		
		return result;
	end "<";

	
	
	function to_string (
		i : in type_point_to_polygon_status)
		return string
	is
		use ada.strings.unbounded;
		use pac_probe_line_intersections_polygon;

		result : unbounded_string;
		
		procedure query_intersection (c : pac_probe_line_intersections_polygon.cursor) is begin
			result := result & type_float_internal'image (element (c).x_position) 
						& "/" & trim (to_string (element (c).angle), left);
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

		result := result & "Distance to polygon: " & to_string (i.distance) & ".";
				
		if is_empty (i.intersections) then
			result := result & "No intersections with probe line.";
		else
			result := result & "Intersection(s) with probe line (x/angle):";
		end if;
		
		iterate (i.intersections, query_intersection'access);
		
		return to_string (result);
	end to_string;





	function get_point_to_polygon_status (
		polygon		: in type_polygon;	
		point		: in type_vector) -- CS rename to vector ?
		return type_point_to_polygon_status 
	is
		-- This function bases on the algorithm published at
		-- <http://www.alienryderflex.com/polygon//>
		-- The algorithm has further been extended to detect intersections
		-- with arcs and even circles.

		-- A probe line will be formed which starts at the given point
		-- and runs to the right (direction zero degree).
		-- The places, after the given start point, where the probe line 
		-- intersects the polygon edges are returned in a list.
		-- If a segment of the polygon crosses the imaginary probe line,
		-- then it is regarded as intersection.
		-- NOTE: A line segment that runs exactly along the probe line
		-- is NOT regarded as "crossing" the probe line.
		
		-- The approach to detect whether the given point lies inside or outside 
		-- the polygon area is as follows:
		-- 1. Build a probe line (starting at point) that runs at zero degrees
		--    to the right. The probe line divides the area in two: an upper half and a
		--    lower half. Special situations arise if objects start or end exactly at
		--    the probe line.
		-- 2. The number of intersections after the start point then tells us:
		--    - odd -> point is inside the polygon area
		--    - zero or even -> point is outside the polygon area

		-- These are the components of the return value.
		-- In the end of this function they will be assembled 
		-- to the actual return:
		result_status : type_location;
		result_intersections : pac_probe_line_intersections_polygon.list;
		result_distance : type_float_internal := 0.0;
		result_edge : pac_edges.cursor;
		result_neigboring_edges : type_neigboring_edges;

		--vertex : constant type_point := to_point (point);
		
		--line_pre : constant type_line := (
				--start_point	=> point,
				--end_point	=> type_point (set (get_x (point) + 1.0, get_y (point))));
		
		--probe_line : constant type_line_vector := to_line_vector (line_pre);
		probe_line : constant type_line_vector := (
			v_start => point,
			v_direction => (1.0, 0.0, 0.0));

		-- For segments that end or start exactly on the Y value of the probe line
		-- we define a threshold:
		y_threshold : constant type_float_internal := get_y (point);


		function crosses_threshold (
			line		: in type_line;	
			y_threshold	: in type_float_internal)
			return boolean
		is begin
			if	
				type_float_internal (get_y (line.start_point)) >= y_threshold and 
				type_float_internal (get_y (line.end_point))   <  y_threshold then
				return true;
				
			elsif
				type_float_internal (get_y (line.end_point))   >= y_threshold and 
				type_float_internal (get_y (line.start_point)) <  y_threshold then
				return true;
				
			else
				return false;
			end if;
		end crosses_threshold;

		
		-- This is the variable for the number of intersections detected.
		-- From this number we will later deduce the position of the given point,
		-- means whether it is inside or outside the polygon:
		it : count_type := 0;

		use pac_probe_line_intersections_polygon;

		
		-- This procedure collects the intersection in the return value.
		procedure collect_intersection (
			intersection: in et_geometry_2.type_intersection; -- incl. point and angle
			edge		: in type_edge)
		is 
			xi : constant type_float_internal := get_x (intersection.vector);
		begin
			-- The intersection will be collected if it is ON or
			-- AFTER the given start point. If it is before the start
			-- point then we ignore it:
			--if xi >= type_float_internal (get_x (point)) then
			if xi >= get_x (point) then
				
				append (result_intersections, (
					x_position	=> xi,
					angle		=> intersection.angle,
					edge		=> edge));

			end if;
		end collect_intersection;

		
		procedure query_edge (c : in pac_edges.cursor) is 
			-- Find out whether there is an intersection of the probe line
			-- and the candidate edge of the polygon.
			i : constant type_intersection_of_two_lines := 
				get_intersection (probe_line, element (c));
			
		begin
			--put_line ("##");		
			--put_line (to_string (l));
			
			if i.status = EXISTS then
				--put_line ("exists");
				--put_line (to_string (l));
				--put_line (to_string (y_threshold));
				--put_line (to_string (i.intersection.vector));

				-- If the candidate line segment crosses the y_threshold then 
				-- count the intersection:
				if crosses_threshold (element (c), y_threshold) then
					--put_line ("crosses threshold");
					
					-- Add the intersection to the result:
					--collect_intersection (i.intersection, (LINE, element (c)));
					collect_intersection (i.intersection, element (c));
				end if;
			end if;				
		end query_edge;
		

		
		
		procedure sort_x_values is
			package pac_probe_line_intersections_sorting is new 
				pac_probe_line_intersections_polygon.generic_sorting;
			
			use pac_probe_line_intersections_sorting;
			--c : pac_probe_line_intersections.cursor;
		begin
			sort (result_intersections);

			-- for testing/verifying only:
			--put_line ("with redundant intersections:");
			--c := result.intersections.first;				
			--while c /= pac_probe_line_intersections.no_element loop
				--put_line (type_float_internal'image (element (c).x_position));
				--next (c);
			--end loop;

			
			-- If x-positions differ by type_distance'small then we
			-- treat them as redundant.
			-- Remove redundant x-positions:
			--c := result.intersections.first;
			--while c /= pac_probe_line_intersections.no_element loop

				--if c /= result.intersections.first then
					--if abs (element (c).x_position - element (previous (c)).x_position)
						--<= type_distance'small 
					--then
						--delete (result.intersections, c);
					--end if;
				--end if;
					
				--next (c);
			--end loop;

			-- for testing/verifying only:
			--put_line ("without redundant intersections:");
			--c := result.intersections.first;		
			--while c /= pac_probe_line_intersections.no_element loop
				--put_line (type_float_internal'image (element (c).x_position));
				--next (c);
			--end loop;

		end sort_x_values;

		
	begin -- get_point_to_polygon_status
		
		--put_line ("Y-threshold:" & to_string (y_threshold));
		
		polygon.edges.iterate (query_edge'access);


		
		-- The x-values are not sorted yet. We need them sorted with the
		-- smallest x first
		-- CS: and redundant x-positions removed: -- no longer required
		sort_x_values;

		-- get the total number of intersections
		it := pac_probe_line_intersections_polygon.length (result_intersections);
		--put_line ("intersections total:" & count_type'image (it));
		
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


		-- Figure out whether the given point is a vertex, whether
		-- it lies on an edge or whether it lies somewhere else:
		if is_vertex (polygon, point) then
			result_status := ON_VERTEX;
			-- NOTE: result.distance is zero by default

			-- Get the edges that meet at the given point:
			result_neigboring_edges := get_neigboring_edges (polygon, point);
		else
			result_edge := get_segment_edge (polygon, point);

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
		
	end get_point_to_polygon_status;


	


	
	function get_location (
		polygon		: in type_polygon;	
		point		: in type_vector)
		return type_location
	is begin
		return get_point_to_polygon_status (polygon, point).location;
	end get_location;

	



	
	
	procedure toggle_direction (
		d : in out type_intersection_direction) 
	is begin
		case d is
			when ENTERING => d := LEAVING;
			when LEAVING => d := ENTERING;
		end case;
	end toggle_direction;



	function get_direction (
		polygon	: in type_polygon;
		line	: in type_edge;
		point	: in type_vector)
		return type_point_of_contact
	is
		result_is_intersection : boolean := false;
		result_direction : type_intersection_direction;

		-- This is a supportive point right after a given point
		-- on the given line towards or beyond the end of the given line:
		SP_after : type_vector;

		-- This is a supportive point right before a given point
		-- on the given line towards or beyond the start of the given line:
		SP_before : type_vector;

		-- These flags indicate that the given point lies
		-- directly on the start or end point of the given line:
		point_on_start, point_on_end : boolean := false;
		
	begin
		if point = line.start_point then
			point_on_start := true;
		end if;

		if point = line.end_point then
			point_on_end := true;
		end if;

		-- Safety check:
		if point_on_start and point_on_end then
			raise constraint_error; -- CS should never happen
		end if;

		
		SP_before := get_nearest (line, point, BEFORE);
		SP_after := get_nearest (line, point, AFTER);

		
		-- NOTE:
		-- CS: The supportive points are by type_distance'small away from
		-- the given point. In some cases this distance could be too much.
		-- A more accurate approach (basing on float types) should be implemented.

		declare
			PPS_before : constant type_location := 
				get_point_to_polygon_status (polygon, SP_before).location;

			PPS_after : constant type_location := 
				get_point_to_polygon_status (polygon, SP_after).location;
		begin
			--put_line ("before " & to_string (SP_before));
			--put_line ("after  " & to_string (SP_after));

			case PPS_before is
				when OUTSIDE =>
					
					case PPS_after is
						when OUTSIDE => 
							if point_on_start then
								result_is_intersection := true;
								result_direction := LEAVING;
								
							elsif point_on_end then
								result_is_intersection := true;
								result_direction := ENTERING;
							else
								-- not an intersection but just a touch point
								null;
							end if;
							
						when INSIDE | ON_EDGE | ON_VERTEX =>
							result_is_intersection := true;
							result_direction := ENTERING;
					end case;


				when INSIDE =>
					case PPS_after is
						when INSIDE => 
							if point_on_start then
								result_is_intersection := true;
								result_direction := ENTERING;
								
							elsif point_on_end then
								result_is_intersection := true;
								result_direction := LEAVING;
							else
								-- not an intersection but just a touch point
								null;
							end if;
							
						when OUTSIDE | ON_EDGE | ON_VERTEX =>
							result_is_intersection := true;
							result_direction := LEAVING;
					end case;


				when ON_EDGE | ON_VERTEX =>
					case PPS_after is
						when OUTSIDE => 
							result_is_intersection := true;
							result_direction := LEAVING;
							
						when INSIDE =>
							result_is_intersection := true;
							result_direction := ENTERING;

						when ON_EDGE | ON_VERTEX =>
							-- line runs parallel to an edge
							if point_on_start then
								result_is_intersection := true;
								result_direction := ENTERING;
								
							elsif point_on_end then
								result_is_intersection := true;
								result_direction := LEAVING;
							else
								raise constraint_error; -- CS
							end if;
					end case;

			end case;
		end;

		
		case result_is_intersection is
			when TRUE =>
				return (is_intersection => TRUE, direction => result_direction);

			when FALSE =>
				return (is_intersection => FALSE);
		end case;
	end get_direction;
	

	
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
			--distance	: type_distance_positive;
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

	

	function equals (left, right : in type_line_to_polygon_status)
		return boolean
	is
		result : boolean := true;

		use pac_line_edge_intersections;
		cr : pac_line_edge_intersections.cursor := right.intersections.first;
		
		procedure query_left (cl : in pac_line_edge_intersections.cursor) is
			i_left  : type_intersection_line_edge := element (cl);
			i_right : type_intersection_line_edge := element (cr);
		begin
			--if element (i_left.edge) = element (i_right.edge) 
			if i_left.edge = i_right.edge				
			and	i_left.direction = i_right.direction
			and equals (i_left.position, i_right.position)
			then			
				null;
			else
				result := false;
				-- CS use special iterator procedure to abort on first mismatch
			end if;
			
			next (cr);
		end query_left;
		
	begin
		if 	left.start_point 	= right.start_point
		and left.end_point		= right.end_point
		and left.center_point	= right.center_point
		then
			left.intersections.iterate (query_left'access);
		else
			result := false;
		end if;

		return result;
	end equals;
	
	
	function get_line_to_polygon_status (
		polygon	: in type_polygon;
		edge	: in type_edge)
		return type_line_to_polygon_status
	is
		result : type_line_to_polygon_status;

		line_center : type_vector;
		
		procedure set_line_start is 
			PPS : constant type_point_to_polygon_status := 
				get_point_to_polygon_status (polygon, edge.start_point);
		begin
			case PPS.location is
				when INSIDE => 
					result.start_point := (location => INSIDE);

				when OUTSIDE => 
					result.start_point := (location => OUTSIDE);

				when ON_EDGE => 
					result.start_point := (
						location	=> ON_EDGE, 
						edge		=> PPS.edge,
						others 		=> <>); -- direction will be set later

				when ON_VERTEX => 
					result.start_point := (
						location	=> ON_VERTEX, 
						edges		=> PPS.edges,
						others 		=> <>); -- direction will be set later
					
			end case;
		end set_line_start;

		
		procedure set_line_end is 
			PPS : constant type_point_to_polygon_status := 
				get_point_to_polygon_status (polygon, edge.end_point);
		begin
			case PPS.location is
				when INSIDE => 
					result.end_point := (location => INSIDE);

				when OUTSIDE => 
					result.end_point := (location => OUTSIDE);

				when ON_EDGE => 
					result.end_point := (
						location	=> ON_EDGE, 
						edge		=> PPS.edge,
						others 		=> <>); -- direction will be set later

				when ON_VERTEX => 
					result.end_point := (
						location	=> ON_VERTEX, 
						edges		=> PPS.edges,
						others 		=> <>); -- direction will be set later
		
			end case;
		end set_line_end;

		
		procedure find_intersections is
			
			procedure query_edge (c : in pac_edges.cursor) is 
				I2L : constant type_intersection_of_two_lines := 
					get_intersection (element (c), edge);

				I_rounded : type_vector;
				IP : type_vector;
	
			begin
				-- We are interested in an edge that DOES intersect in some way
				-- the given edge. Otherwise the candidate edge is to be skipped:
				if I2L.status = EXISTS then
					IP := I2L.intersection.vector;

					-- If the intersection is right on the start or the end
					-- of the given edge the the candidate edge is to be skipped:
					if IP = edge.start_point 
					or IP = edge.end_point
					then
						null; -- skip this intersection point entirely
					else
						-- Collect this intersection point if it has
						-- not already been collected yet:

						I_rounded := round (
							vector		=> I2L.intersection.vector, 
							accuracy	=> type_float_internal'digits -1);

						if not contains (result.intersections, I_rounded) then
							result.intersections.append ((
								position => I_rounded, edge => c, others => <>));
							-- The direction will be set later.
						end if;
					end if;
				end if;
			end query_edge;

		begin
			polygon.edges.iterate (query_edge'access);
		end find_intersections;


		procedure set_entering_leaving is
			use pac_line_edge_intersections;
			ic : pac_line_edge_intersections.cursor := result.intersections.first;
			ic_bak : pac_line_edge_intersections.cursor;
			delete_first : boolean := false;
			
			-- This flag is set in case an intersection is erroneously mistaken
			-- as such:
			delete_intersection : boolean := false;

			-- This procedure:
			-- - assigns a definite direction to an intersection.
			-- - detects whether the candidate intersection is erroneous
			--   by setting the flag "delete_intersection".
			--   This erroneous intersection will later be removed from the
			--   list of intersections:			
			procedure set_direction (i : in out type_intersection_line_edge) is
				POC : constant type_point_of_contact := get_direction (polygon, edge, i.position);
			begin
				-- If the given supposed intersection can not be confirmed as such
				-- then the intersection is erroneous and the flag "delete_intersection" is set,
				-- so that it will be deleted from the list later.
				
				if POC.is_intersection then -- intersection confirmed
					i.direction := POC.direction;
					delete_intersection := false;
				else
					-- not confirmed -> must be removed
					delete_intersection := true;
				end if;	
			end set_direction;

			
		begin -- set_entering_leaving
			
			-- In case the start point of the line lies on an edge or on a vertex
			-- then assign the direction of this intersection:
			case result.start_point.location is
				when OUTSIDE | INSIDE =>
					null; -- not an intersection -> nothing to do
					
				when ON_EDGE =>
					declare
						POC : constant type_point_of_contact := 
							get_direction (polygon, edge, edge.start_point);
					begin
						result.start_point.direction_on_edge := POC.direction;
					end;
					
				when ON_VERTEX =>
					declare
						POC : constant type_point_of_contact := 
							get_direction (polygon, edge, edge.start_point);
					begin
						result.start_point.direction_on_vertex := POC.direction;
					end;
			end case;

			-- In case the end point of the edge lies on an edge or on a vertex
			-- then assign the direction of this intersection:
			case result.end_point.location is
				when OUTSIDE | INSIDE =>
					null; -- not an intersection -> nothing to do
					
				when ON_EDGE =>
					declare
						POC : constant type_point_of_contact := 
							get_direction (polygon, edge, edge.end_point);
					begin
						result.end_point.direction_on_edge := POC.direction;
					end;
					
				when ON_VERTEX =>
					declare
						POC : constant type_point_of_contact := 
							get_direction (polygon, edge, edge.end_point);
					begin
						result.end_point.direction_on_vertex := POC.direction;
					end;
					
			end case;
			
			-- Iterate through the intersections and assign each of 
			-- them a definite direction. If no intersections exist then
			-- nothing happens here:
			while ic /= pac_line_edge_intersections.no_element loop
				result.intersections.update_element (ic, set_direction'access);

				-- If the candidate intersection has not been confirmed by
				-- procedure set_direction then we delete it here:
				if delete_intersection then

					-- Detect whether we are about to delete
					-- the first intersection:
					if ic = result.intersections.first then
						delete_first := true;
					else
						-- In case a deletion is required, then the
						-- cursor to the element before the candidate intersection
						-- must be saved. 
						-- Why ? The delete command (see below) resets the 
						-- main cursor "ic" to no_element:
						ic_bak := previous (ic);
					end if;

					-- Delete the candidate intersection:
					result.intersections.delete (ic);
					-- Now the main cursor "ic" points to no_element !

					if delete_first then
						-- Since we have deleted the first intersection,
						-- the list has now another first element.
						-- So the main cursor must be set to that first intersection:
						ic := result.intersections.first;
						delete_first := false;
					else
						-- Restore the main cursor:
						ic := ic_bak;
						next (ic);
					end if;

				else
					next (ic);
				end if;
				
			end loop;
		end set_entering_leaving;

		
	begin -- get_line_to_polygon_status

		-- Set the properties of the start/end point of the given line:
		set_line_start;
		set_line_end;

		-- Find the intersections that are between start and end point
		-- of the given line with the polygon:
		find_intersections;

		-- If there are no intersections between start and end point
		-- then the location of the 
		-- center of the given line tells whether the line runs
		-- completely inside or outside the polygon.
		if result.intersections.is_empty then
			line_center := get_center (edge);

			--put_line ("center" & to_string (line_center));
			
			case get_point_to_polygon_status (polygon, line_center).location is
				when INSIDE => result.center_point := INSIDE;
				when OUTSIDE => result.center_point := OUTSIDE;
				when ON_EDGE | ON_VERTEX => result.center_point := ON_EDGE;
			end case;
			
		end if;

		-- Sort intersections by their distance to the start point.
		sort_by_distance (result.intersections, edge.start_point);
		
		-- Assign directions of start and end point and the intersections
		-- betweeen start and end:
		set_entering_leaving;
		
		return result;
	end get_line_to_polygon_status;


	

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
		-- CS: Due to inevitable rounding errors this direction comparison may not
		-- work in all cases:
		if element (intersection_1).position = element (intersection_2).position then

		-- ? Instead use this:
		--if equals (element (intersection_1).position, element (intersection_2).position) then
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
			-- CS: We assume that on a touch point a leaving and an
			-- entering node follow each other (in the given 
			-- list of intersections). So we always look at the 
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
			IPQ : constant type_point_to_polygon_status :=
				get_point_to_polygon_status (polygon_B, element (c).start_point);
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
	


	function to_polygon (vertices : in pac_vertices.list)
		return type_polygon
	is
		result : type_polygon;
		
		procedure query_vertex (v : in pac_vertices.cursor) is 
			edge : type_edge;
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
			
			append (result.edges, edge);
		end query_vertex;
		
	begin
		-- Convert the list of vertices to a list of lines (or edges):
		vertices.iterate (query_vertex'access);

		return result;
	end to_polygon;



	

	function get_intersections (
		polygon_A	: in type_polygon;
		polygon_B	: in type_polygon;
		debug		: in boolean := false)
		return pac_intersections.list
	is
		intersections : pac_intersections.list;
		
		procedure query_A_edge (a : in pac_edges.cursor) is

			-- The status of the A-edge relative to polygon B:
			LPS : constant type_line_to_polygon_status := 
				get_line_to_polygon_status (polygon_B, element (a));

			
			procedure collect_intersections is 

				procedure query_intersection (i : in pac_line_edge_intersections.cursor) is
					use pac_line_edge_intersections;
					b_edge : pac_edges.cursor := element (i).edge;
				begin
					-- Ignore intersection if edge A runs "parallel" to edge B:
					--if not lines_overlap (element (a).segment_line, element (b_edge).segment_line) then
					if not lines_overlap (element (a), element (b_edge)) then
						intersections.append ((
							type_intersection_base (element (i)) with
							edge_A => element (a),
							edge_B => element (b_edge)));

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
				IAB.position := element (a).start_point;
				IAB.edge_A := element (a);
				
				case LPS.start_point.location is
					when ON_EDGE =>
						IAB.direction := LPS.start_point.direction_on_edge;

						-- Get the touched B-edge at the start point:
						IAB.edge_B := element (LPS.start_point.edge); 

						-- Ignore intersection if edge A runs "parallel" to edge B:
						if not lines_overlap (element (a), IAB.edge_B) then
							-- collect intersection:
							intersections.append (IAB);

							if debug then
								put_line ("intersection (start on edge): " & to_string (intersections.last_element));
							end if;
						end if;
						
					when ON_VERTEX =>
						IAB.direction := LPS.start_point.direction_on_vertex;

						-- Get the touched B-edge that starts at the start point:
						IAB.edge_B := element (LPS.start_point.edges.edge_2);

						-- Ignore intersection if edge A runs "parallel" to edge B:
						if not lines_overlap (element (a), IAB.edge_B) then
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
				IAB.position := element (a).end_point;
				IAB.edge_A := element (a);
				
				case LPS.end_point.location is
					when ON_EDGE =>
						IAB.direction := LPS.end_point.direction_on_edge;

						-- Get the touched B-edge at the end point:
						IAB.edge_B := element (LPS.end_point.edge); 

						-- Ignore intersection if edge A runs "parallel" to edge B:
						if not lines_overlap (element (a), IAB.edge_B) then
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
						IAB.edge_B := element (LPS.end_point.edges.edge_1);

						-- Ignore intersection if edge A runs "parallel" to edge B:
						if not lines_overlap (element (a), IAB.edge_B) then
							-- collect intersection:
							intersections.append (IAB);

							if debug then
								put_line ("intersection (end on vertex): " & to_string (intersections.last_element));
							end if;
						end if;

					when others => raise constraint_error; -- CS should never happen
				end case;
			end use_end_point_as_intersection;
		
			
		begin -- query_A_edge

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
			
		end query_A_edge;


		-- Removes successive redundant intersections from 
		-- list "intersections" so that only one of them is left:
		--procedure remove_redundant_intersections is
			--c : pac_intersections.cursor := intersections.first;
			--i_list_new : pac_intersections.list;
		--begin
			----put_line ("removing redundant intersections ...");
			--while c /= pac_intersections.no_element loop
				----put_line (to_string (element (c)));
				
				--if c /= intersections.last then
					--if not are_redundant (next (c), c) then
						--i_list_new.append (element (c));
					--end if;
				--else
					--if not are_redundant (intersections.first, c) then
						--i_list_new.append (element (c));
					--end if;
				--end if;
				
				--next (c);
			--end loop;			

			--intersections := i_list_new;
		--end remove_redundant_intersections;


		-- Removes redundant intersections from 
		-- list "intersections" so that only one of them is left:
		procedure remove_redundant_intersections is
			c : pac_intersections.cursor := intersections.first;
			i_list_new : pac_intersections.list;
			
			function redundant return boolean is
				proceed : aliased boolean := true;
				
				procedure query_intersection (i : pac_intersections.cursor) is begin
					if are_redundant (i, c) then
						proceed := false; -- first match. abort iteration
					end if;
				end query_intersection;
			
			begin
				-- Search in i_list_new for an intersection that is
				-- redundant with the candidate indicated by cursor c.
				-- Abort the search on first match:
				iterate (i_list_new, query_intersection'access, proceed'access);
				return not proceed;
			end redundant;

			
		begin
			-- Copy one intersection by the other to i_list_new,
			-- Redundant intersections (those which are already in i_list_new)
			-- are skipped:
			
			--put_line ("removing redundant intersections ...");
			while c /= pac_intersections.no_element loop
				--put_line (to_string (element (c)));
				
				if not redundant then
					i_list_new.append (element (c));
				end if;
				
				next (c);
			end loop;			

			intersections := i_list_new;
		end remove_redundant_intersections;

		
	begin
		-- Traverse the edges of polygon A:
		polygon_A.edges.iterate (query_A_edge'access);

		remove_redundant_intersections;

		return intersections;
	end get_intersections;


	
	function get_overlap_status (
		polygon_A, polygon_B	: in type_polygon;
		intersections			: in pac_intersections.list;
		debug					: in boolean := false)
		return type_overlap_status
	is
		result : type_overlap_status;
		real_intersections : pac_intersections.list;
	begin
		if are_congruent (polygon_A, polygon_B) then
			result := CONGRUENT;
		else
			-- The given list of intersections may include places where 
			-- the two polygons just touch but do not really intersect.
			-- Extract the nodes (from given list of intersection) where
			-- the edges of the two polygons truly intersect.
			real_intersections := get_real_intersections (intersections);
			
			case real_intersections.length is
				when 0 => -- no intersections of edges or vertices
					
					if all_vertices_of_A_inside_B (polygon_A, polygon_B) then
						result := A_INSIDE_B;
						
					elsif all_vertices_of_A_inside_B (polygon_B, polygon_A) then
						result := B_INSIDE_A;
						
					else
						-- A and B do not overlap. They are apart from each other:
						result := A_DOES_NOT_OVERLAP_B;
					end if;
					
				when 1 => raise constraint_error; -- CS should never happen

				when others =>
					result := A_OVERLAPS_B;

			end case;
		end if;
		
		if debug then
			put_line ("overlap status: " & type_overlap_status'image (result));
		end if;

		return result;
	end get_overlap_status;



	procedure delete_regular_after_intersection (
		vertices : in out pac_vertices.list)
	is
		-- This cursor points to the candidate vertex in the given list:
		c : pac_vertices.cursor := vertices.first;

		-- In the following this list will be filled with vertices by
		-- copying vertices from the given list "vertices" one by one.
		-- If a regular vertex is detected right after an intersection
		-- then this particular vertex will be ignored:
		v_list_new : pac_vertices.list;
	begin
		while c /= pac_vertices.no_element loop
			--put_line ("c: " & to_string (element (c).position));
			
			if c = vertices.first then
				-- Look at the vertex at the end of the list:
				if same_position (vertices.last, c) then
					null; -- ignore vertex
				else
					-- copy vertex
					v_list_new.append (element (c));
				end if;
				
			else
				
				if same_position (previous (c), c) then
					null; -- ignore vertex
				else
					-- copy vertex
					v_list_new.append (element (c));
				end if;
			end if;
			
			next (c);
		end loop;

		-- overwrite the given list by the new list:
		vertices := v_list_new;
	end delete_regular_after_intersection;


	procedure delete_regular_before_intersection (
		vertices : in out pac_vertices.list)
	is
		c : pac_vertices.cursor := vertices.first;
		c2 : pac_vertices.cursor;

		-- CS: Probably rework required as in delete_regular_after_intersection ?
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

		--put_line ("vertices pre: " & to_string (vertices));
		delete_regular_after_intersection (vertices);
		delete_regular_before_intersection (vertices);
		
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

		v : pac_vertices.cursor;
		result : pac_vertices.list;

		procedure do_collect is begin
			case direction_of_search is
				when CCW =>
					while v /= pac_vertices.no_element loop
						result.append (element (v));

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
						
						next (v);
					end loop;

				when CW =>
					while v /= pac_vertices.no_element loop
						result.append (element (v));

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
		-- Preset cursor v to the given entering/leaving vertex.
		-- The serach starts here:
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

	
	
end et_geometry_2.polygons;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

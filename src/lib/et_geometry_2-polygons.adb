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
		segment	: in type_contour_segment)
		return string
	is begin
		case segment.shape is
			when LINE =>
				return ("line segment: " & to_string (segment.segment_line));

			when ARC =>
				return ("arc segment: " & to_string (segment.segment_arc));
		end case;
	end to_string;


	procedure iterate (
		segments	: in pac_contour_segments.list;
		process		: not null access procedure (position : in pac_contour_segments.cursor);
		proceed		: not null access boolean)
	is
		c : pac_contour_segments.cursor := segments.first;
	begin
		while c /= pac_contour_segments.no_element and proceed.all = TRUE loop
			process (c);
			next (c);
		end loop;
	end iterate;


	function to_string (
		contour	: in type_contour)
		return string
	is
		use ada.strings.unbounded;
		
		result : unbounded_string := to_unbounded_string ("contour:");

		procedure query_segment (c : in pac_contour_segments.cursor) is begin

			-- We output the start points only (for arcs in addition the center).
			-- Because: The end point of a segment is always the start point of the
			-- next segment.
			
			case element (c).shape is
				when LINE =>
					--result := result & space & to_unbounded_string (to_string (element (c).segment_line));
					result := result & space
						& to_unbounded_string ("line start:" & to_string (element (c).segment_line.start_point));
					
				when ARC =>
					--result := result & space & to_unbounded_string (to_string (element (c).segment_arc));
					result := result & space 
						& to_unbounded_string ("arc center:" & to_string (element (c).segment_arc.center))
						& to_unbounded_string ("start:" & to_string (element (c).segment_arc.start_point));
					
			end case;
		end query_segment;
		
	begin
		if contour.contour.circular then
			result := result & space & to_unbounded_string (to_string (contour.contour.circle));
		else
			contour.contour.segments.iterate (query_segment'access);
		end if;
		
		return to_string (result);
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
				line_new : type_line;
				cursor_new : pac_edges.cursor;
			begin
				line_new := type_line (reverse_line (element (c)));
				
				if polygon_new.edges.is_empty then
					polygon_new.edges.append (line_new);
				else
					cursor_new := polygon_new.edges.first;
					polygon_new.edges.prepend (line_new);
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
		
		edge_A : type_line;

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
		edge	: in type_line)
		return pac_edges.cursor
	is
		result : pac_edges.cursor;
		--segment : type_polygon_segment := (shape => LINE, segment_line => edge);
	begin
		result := polygon.edges.find (edge);
		return result;
	end get_segment_edge;
	


	function get_segment_edge (
		polygon	: in type_polygon;
		point	: in type_point)
		return pac_edges.cursor
	is
		result : pac_edges.cursor;
		proceed : aliased boolean := true;

		--procedure query_segment (c : in pac_edges.cursor) is begin
			--case element (c).shape is
				--when LINE => 
					--if on_line (point, element (c).segment_line) then
						--result := c;
						--proceed := false; -- abort iteration
					--end if;
					
				--when ARC => null; -- CS
			--end case;
		--end query_segment;

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

		-- CS exception message if polygon consists of just a circle.
		return result;
	end get_segment_edge;



	function get_segment (
		contour	: in type_contour;
		point	: in type_point)
		return pac_contour_segments.cursor
	is
		result : pac_contour_segments.cursor;
		proceed : aliased boolean := true;

		procedure query_segment (c : in pac_contour_segments.cursor) is begin
			case element (c).shape is
				when LINE => 
					if on_line (point, element (c).segment_line) then
						result := c;
						proceed := false; -- abort iteration
					end if;
					
				when ARC => null; -- CS
			end case;
		end query_segment;

		
	begin
		-- Make sure the given point is NOT a vertex:
		-- CS: Maybe no need if caller cares for this check.
		if is_vertex (contour, point) then
			raise constraint_error with "Point is a vertex !";
		else
			iterate (contour.contour.segments, query_segment'access, proceed'access);					 
		end if;

		-- CS exception message if polygon consists of just a circle.
		return result;
	end get_segment;


	
	
	function get_neigboring_edges (
		polygon	: in type_polygon;
		vertex	: in type_point)
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



	function get_neigboring_segments (
		contour	: in type_contour;
		vertex	: in type_point)
		return type_neigboring_segments
	is
		result : type_neigboring_segments;
		proceed : aliased boolean := true;

		end_found, start_found : boolean := false;

		
		procedure query_segment (c : in pac_contour_segments.cursor) is begin
			--put_line ("test: " & to_string (element (c)));
			
			case element (c).shape is
			
				when LINE => 
					if element (c).segment_line.end_point = vertex then
						--put_line ("end");
						result.segment_1 := c;
						end_found := true;
					end if;

					if element (c).segment_line.start_point = vertex then
						--put_line ("start");
						result.segment_2 := c;
						start_found := true;
					end if;
					
				when ARC => null; -- CS
			end case;

			-- Abort iteration once start and end point have been found
			-- ("proceed" is low-active):
			proceed := not (end_found and start_found);
			
		end query_segment;

		
	begin
		-- Make sure the given point IS a vertex:
		if is_vertex (contour, vertex) then
			iterate (contour.contour.segments, query_segment'access, proceed'access);					 
		else
			raise constraint_error with "Point is a vertex !";
		end if;

		-- Safety check:
		-- Two edges must have been found. Otherwise raise exception:
		if result.segment_1 = pac_contour_segments.no_element 
		or result.segment_2 = pac_contour_segments.no_element
		then
			raise constraint_error with "Search for neigboring edges incomplete !";
		end if;

		-- CS exception message if polygon consists of just a circle.
		return result;
	end get_neigboring_segments;

	

	
	function to_string (
		polygon	: in type_polygon)
		return string
	is
		use ada.strings.unbounded;
		
		result : unbounded_string := to_unbounded_string ("polygon:");

		--procedure query_segment (c : in pac_polygon_segments.cursor) is begin

			---- We output the start points only (for arcs in addition the center).
			---- Because: The end point of a segment is always the start point of the
			---- next segment.
			
			--case element (c).shape is
				--when LINE =>
					----result := result & space & to_unbounded_string (to_string (element (c).segment_line));
					--result := result & space
						--& to_unbounded_string ("line start:" & to_string (element (c).segment_line.start_point));
					
				--when ARC =>
					----result := result & space & to_unbounded_string (to_string (element (c).segment_arc));
					--result := result & space 
						--& to_unbounded_string ("arc center:" & to_string (element (c).segment_arc.center))
						--& to_unbounded_string ("start:" & to_string (element (c).segment_arc.start_point));
					
			--end case;
		--end query_segment;

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



	
	
	function get_nearest_corner_point (
		polygon		: in type_polygon;
		reference	: in type_point)
		return type_point
	is
		result : type_point := origin;
		
		d1 : type_distance_positive := zero;
		d2 : type_distance_positive := get_absolute (get_distance (reference, far_upper_right));

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

		procedure query_edge (c : in pac_edges.cursor) is
			s : constant type_line := element (c);
		begin
			-- test start point
			d1 := get_absolute (get_distance (reference, s.start_point));
			
			if d1 < d2 then
				d2 := d1;
				
				result := s.start_point;
			end if;

			-- test end point
			d1 := get_absolute (get_distance (reference, s.end_point));
			
			if d1 < d2 then
				d2 := d1;
				
				result := s.end_point;
			end if;
		end query_edge;
		
	begin
		polygon.edges.iterate (query_edge'access);				
		
		return result;
	end get_nearest_corner_point;

	
	function get_shortest_distance (
		contour	: in type_contour;
		point	: in type_point)
		return type_distance_polar
	is
		result : type_distance_polar := to_polar (type_distance_positive'last, zero_rotation);
		
		procedure update (d : in type_distance_polar) is begin
			--put_line (to_string (d));
			if get_absolute (d) < get_absolute (result) then
				result := d;
			end if;
		end update;

		
		procedure query_segment (c : in pac_contour_segments.cursor) is
			s : constant type_contour_segment := element (c);
		begin
			case s.shape is
				when LINE =>
					--put_line (to_string (s.segment_line));
					update (get_shortest_distance (point, s.segment_line));

				when ARC =>
					--put_line (to_string (s.segment_arc));
					update (get_shortest_distance (point, s.segment_arc));
					
			end case;
		end query_segment;

		
	begin -- get_shortest_distance
		if contour.contour.circular then
			result := get_shortest_distance (point, contour.contour.circle);
		else
			contour.contour.segments.iterate (query_segment'access);				
		end if;			

		--set_absolute (result, type_distance (round (get_absolute (result))));
		
		return result;
	end get_shortest_distance;



	
	procedure load_segments (
		contour		: in out type_contour;
		segments	: in type_contour_segments)
	is begin
		contour.contour := segments;
	end load_segments;

	
	procedure delete_segments (
		contour : in out type_contour)
	is begin
		contour.contour := (others => <>);
	end delete_segments;			

	
	procedure append_segment (
		contour	: in out type_contour;
		segment	: in type_contour_segment)
	is begin
		-- CS check discriminant and issue helpful error message ?

		-- CS length check for line:
		--if get_length (line) > type_distance'small then
			--return line;
		--else
			--raise constraint_error with "Line has zero length !";
		--end if;

		-- CS length check for arc:		
		
		contour.contour.segments.append (segment);			
	end append_segment;

	
	procedure set_circle (
		contour	: in out type_contour;
		circle	: in type_circle'class)
	is begin
		-- CS check discriminant and issue helpful error message ?
		
		contour.contour.circle := type_circle (circle);
	end set_circle;
				
	
	function get_segments (
		contour	: in type_contour)
		return type_contour_segments
	is begin
		return contour.contour;
	end get_segments;


	function get_segments_total (
		contour : in type_contour)
		return count_type
	is begin
		if contour.contour.circular then
			return 1;
		else
			return length (contour.contour.segments);
		end if;
	end get_segments_total;


	function get_edges_total (
		polygon : in type_polygon)
		return count_type
	is begin
		return length (polygon.edges);
	end get_edges_total;

	
	
	procedure transpose_contour (
		contour	: in out type_contour'class;
		offset	: in type_distance)
	is 
		procedure move (point : in out type_point) is
			new_y : type_distance;
		begin
			new_y := offset - get_y (point);
			point.set (Y, new_y);
		end move;

		
		procedure move_segment (c : in pac_contour_segments.cursor) is

			procedure do_line (s : in out type_contour_segment) is begin 
				move (s.segment_line.start_point);
				move (s.segment_arc.end_point);
			end;
			
			procedure do_arc (s : in out type_contour_segment) is begin
				move (s.segment_arc.start_point);
				move (s.segment_arc.end_point); 
				move (s.segment_arc.center); 
			end;

		begin -- move_segment
			case element (c).shape is
				
				when LINE =>
					update_element (
						container	=> contour.contour.segments,
						position	=> c,
						process		=> do_line'access);

				when ARC =>
					update_element (
						container	=> contour.contour.segments,
						position	=> c,
						process		=> do_arc'access);

			end case;
		end move_segment;

		
	begin
		if contour.contour.circular then

			-- move the single circle that forms the contour:
			move (contour.contour.circle.center);
		else
			-- move lines and arcs:
			contour.contour.segments.iterate (move_segment'access);
		end if;
		
	end transpose_contour;

	
	function to_contour (
		arguments : in type_fields_of_line) -- line 0 0 line 160 0 line 160 80 line 0 80
		return type_contour'class
	is
		-- CS: to do:
		-- - length check for lines and arc segments
		-- - merge neigboring line segments that run in the same direction
		
		result : type_contour; -- will be converted back to class type on return
		-- NOTE: The default discriminant if the result makes it a
		-- contour consisting of lines an arcs.
		-- If a circle is to be added to the contour then the discriminant
		-- changes accordingly.

		function f (place : in count_type) return string is begin
			return to_lower (get_field (arguments, place));
		end;
		
		l : type_line;
		a : type_arc;
		c : type_circle;

		-- The shape of the segment being processed:
		shape : type_shape;

		-- The place in arguments at which we fetch a field from:
		p : count_type := 1;

		start_point_set : boolean := false;
		contour_start_point : type_point; -- start point of contour

		end_point_previous : type_point;

		
		procedure update_end_point (s : in out type_contour_segment) is begin
			case s.shape is
				when LINE =>						
					s.segment_line.end_point := end_point_previous;
					
				when ARC =>
					s.segment_arc.end_point := end_point_previous;
			end case;
		end update_end_point;
		
	begin
		-- Iterate all fields of given list of arguments:
		while p <= field_count (arguments) loop

			-- If a keyword like "line", "arc" or "circle" occurs,
			-- then set shape accordingly:
			if f (p) = to_string (LINE) then
				--put_line ("line");
				shape := LINE;
				
			elsif f (p) = to_string (ARC) then
				--put_line ("arc");
				shape := ARC;

			elsif f (p) = to_string (CIRCLE) then
				--put_line ("circle");
				shape := CIRCLE;

				-- Once a circle occurs, only a single
				-- circle can be assigned to the result.
				-- The discriminant of the result mutates here.
				-- An attempt to append a line or an arc to the
				-- contour segments will cause a exception.
				result.contour := (
					circular	=> true,
					others		=> <>);
				
			end if;

			-- Fetch the parameters for the shape by
			-- looking ahead of p:
			case shape is
				when LINE => -- line 0 0

					-- assign start point of line
					l.start_point := type_point (set (
							x => to_distance (f (p + 1)),
							y => to_distance (f (p + 2))));

					-- The start point of the line is also the end point of
					-- the previous segment:
					end_point_previous := l.start_point;

					if start_point_set then
						-- assign the end point of the previous segment:
						update_element (
							container	=> result.contour.segments,
							position	=> result.contour.segments.last,
							process		=> update_end_point'access);
					
					else
						-- register the start point of the contour
						contour_start_point := l.start_point;
						start_point_set := true;
					end if;
					
					result.contour.segments.append (new_item => (LINE, l));

					-- fast forward p to next shape:
					p := p + 3;

					
				when ARC => -- arc 50 100 100 100 ccw

					-- assign center of arc
					a.center := type_point (set (
							x => to_distance (f (p + 1)),
							y => to_distance (f (p + 2))));
						
					-- assign start point of arc
					a.start_point := type_point (set (
							x => to_distance (f (p + 3)),
							y => to_distance (f (p + 4))));

					-- The start point of the arc is also the end point
					-- of the previous segment:
					end_point_previous := a.start_point;

					if start_point_set then
						-- assign the end point of the previous segment:
						update_element (
							container	=> result.contour.segments,
							position	=> result.contour.segments.last,
							process		=> update_end_point'access);
					
					else
						-- register the start point of the contour
						contour_start_point := a.start_point;
						start_point_set := true;
					end if;

					-- assign direction of arc
					a.direction := to_direction (f (p + 5));
					
					result.contour.segments.append (new_item => (ARC, a));

					-- fast forward p to next shape:						
					p := p + 6;

					
					
				when CIRCLE => -- circle 40 40 10

					-- assign center of circle
					c.center := type_point (set (
							x => to_distance (f (p + 1)),
							y => to_distance (f (p + 2))));

					-- assigne radius of circle
					c.radius := to_distance (f (p + 3));

					result.contour.circle := c;
					
					-- NOTE: No further shape must follow.
					-- So we do not move p further forward 
					-- and abort this loop:
					exit;
					
			end case;
		end loop;

		
		-- If the contour consists of lines and/or arcs then 
		-- the end point of the last segment is where the contour
		-- has started:
		if shape /= CIRCLE then
			
			end_point_previous := contour_start_point;
			
			-- Assign the end point of the last segment:
			update_element (
				container	=> result.contour.segments,
				position	=> result.contour.segments.last,
				process		=> update_end_point'access);

		end if;
			
		return type_contour'class (result);

		-- CS exception handler required for invalid fields:
		
		--exception when event: others =>
			--put_line (exception_message);
			--return p;
	
	end to_contour;
	

	
	function get_boundaries (
		contour		: in type_contour;
		line_width	: in type_distance_positive)
		return type_boundaries 
	is
		result : type_boundaries; -- to be returned

		half_width : constant type_distance_positive := line_width * 0.5;
		

		procedure query_segment (c : in pac_contour_segments.cursor) is begin
			case element (c).shape is
				when LINE =>
					union (result, get_boundaries (element (c).segment_line, zero));

				when ARC =>
					union (result, get_boundaries (element (c).segment_arc, zero));
			end case;						
		end query_segment;

		
	begin -- get_boundaries
		if contour.contour.circular then

			-- Get the boundaries of the single circle:
			union (result, get_boundaries (contour.contour.circle, zero));
			
		else
			-- Iterate lines and arcs:
			contour.contour.segments.iterate (query_segment'access);
		end if;

					
		-- Extend the boundaries by half the line width;
		result.smallest_x := result.smallest_x - half_width;
		result.smallest_y := result.smallest_y - half_width;

		result.greatest_x := result.greatest_x + half_width;
		result.greatest_y := result.greatest_y + half_width;
		
		return result;
	end get_boundaries;
	
	
	function to_string (
		gaps : in pac_contour_gaps.list) 
		return string 
	is
		use pac_contour_gaps;
		use ada.strings.unbounded;
		use ada.characters.latin_1;
		
		result : unbounded_string;
		
		procedure query_gap (g : in pac_contour_gaps.cursor) is 
			scratch : unbounded_string;
		begin
			if next (g) /= pac_contour_gaps.no_element then
				scratch := to_unbounded_string (to_string (element (g)) & comma);
			else
				scratch := to_unbounded_string (to_string (element (g)));
			end if;

			result := result & scratch;
		end query_gap;
	
	begin
		iterate (gaps, query_gap'access);

		return to_string (result);
	end to_string;

	
	function is_closed (
		contour	: in type_contour)
		return type_contour_status 
	is
		-- Goes false once a gap has been detected:
		closed : boolean := true;

		-- The point where the contour outline starts:
		start_point		: type_point;

		-- The end point of a segment. Once the last segment has been processed,
		-- this point must match the start point:
		last_end_point	: type_point;

		-- Goes true once a start point has been set:
		started : boolean := false;

		-- Here we collect the points where a gap begins:
		use pac_contour_gaps;
		gaps : pac_contour_gaps.list;

		-- Sets the start point of the outline if the start point
		-- has not been set already.
		-- Clears the closed-flag if the given point does NOT
		-- match the last_end_point and appends the last_end_point to
		-- the list of gaps.
		procedure set_start_point (p : in type_point) is begin
			if not started then
				start_point := p;
				last_end_point := start_point;
				started := true;
			end if;

			if p /= last_end_point then
				closed := false;
				append (gaps, last_end_point);
			end if;

		end set_start_point;

		procedure query_segment (c : in pac_contour_segments.cursor) is begin
			case element (c).shape is
				when LINE =>
					set_start_point (element (c).segment_line.start_point);
					last_end_point := element (c).segment_line.end_point;

				when ARC =>
					set_start_point (element (c).segment_arc.start_point);
					last_end_point := element (c).segment_arc.end_point;
					
			end case;
		end query_segment;
		
	begin -- is_closed
		
		if contour.contour.circular then
			closed := true; -- because this is a single circle
		else
			-- Iterate lines and arcs:
			contour.contour.segments.iterate (query_segment'access);
		end if;

		
		-- Start point and end point of polygon outline must match:
		if last_end_point /= start_point then
			closed := false;
			append (gaps, last_end_point);
		end if;
		
		-- Return the polygon status:
		if closed then
			return (closed => true);
		else
			return (closed => false, gaps => gaps);
		end if;
	end is_closed;
	
	
	procedure move_by (
		contour	: in out type_contour;
		offset	: in type_distance_relative) 
	is

		procedure move_segment (c : in pac_contour_segments.cursor) is

			procedure do_line (s : in out type_contour_segment) is begin 
				move_by (s.segment_line, offset);
			end;
			
			procedure do_arc (s : in out type_contour_segment) is begin
				move_by (s.segment_arc, offset);
			end;

		begin -- move_segment
			case element (c).shape is
				
				when LINE =>
					update_element (
						container	=> contour.contour.segments,
						position	=> c,
						process		=> do_line'access);

				when ARC =>
					update_element (
						container	=> contour.contour.segments,
						position	=> c,
						process		=> do_arc'access);

			end case;
		end move_segment;
		
	begin -- move_by
		if contour.contour.circular then

			-- move the single circle that forms the contour:
			move_by (contour.contour.circle, offset);
		else
			-- move lines and arcs:
			contour.contour.segments.iterate (move_segment'access);
		end if;
	end move_by;

	
	procedure mirror (
		contour	: in out type_contour;
		axis	: in type_axis_2d) 
	is

		procedure mirror_segment (c : in pac_contour_segments.cursor) is

			procedure do_line (s : in out type_contour_segment) is begin 
				mirror (s.segment_line, axis);
			end;
			
			procedure do_arc (s : in out type_contour_segment) is begin
				mirror (s.segment_arc, axis);
			end;

		begin -- move_segment
			case element (c).shape is
				
				when LINE =>
					update_element (
						container	=> contour.contour.segments,
						position	=> c,
						process		=> do_line'access);

				when ARC =>
					update_element (
						container	=> contour.contour.segments,
						position	=> c,
						process		=> do_arc'access);

			end case;
		end mirror_segment;
		
	begin -- mirror
		if contour.contour.circular then

			-- mirror the single circle that forms the contour:
			mirror (contour.contour.circle, axis);
		else
			-- move lines and arcs:
			contour.contour.segments.iterate (mirror_segment'access);
		end if;
	end mirror;

	
	procedure rotate_by (
		contour		: in out type_contour;
		rotation	: in type_rotation) 
	is

		procedure rotate_segment (c : in pac_contour_segments.cursor) is

			procedure do_line (s : in out type_contour_segment) is begin 
				rotate_by (s.segment_line, rotation);
			end;
			
			procedure do_arc (s : in out type_contour_segment) is begin
				rotate_by (s.segment_arc, rotation);
			end;

		begin -- rotate_segment
			case element (c).shape is
				
				when LINE =>
					update_element (
						container	=> contour.contour.segments,
						position	=> c,
						process		=> do_line'access);

				when ARC =>
					update_element (
						container	=> contour.contour.segments,
						position	=> c,
						process		=> do_arc'access);

			end case;
		end rotate_segment;

		
	begin -- rotate_by
		if contour.contour.circular then

			-- rotate the single circle that forms the contour:
			rotate_by (contour.contour.circle.center, rotation);
		else
			-- rotate lines and arcs:
			contour.contour.segments.iterate (rotate_segment'access);
		end if;			
	end rotate_by;


	
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

	

	function is_vertex (
		polygon	: in type_polygon;
		point	: in type_point)
		return boolean
	is
		proceed : aliased boolean := true;

		procedure query_edge (c : in pac_edges.cursor) is begin
			--case element (c).shape is
				--when LINE =>
					--if element (c).segment_line.start_point = point then
						--proceed := false;
					--end if;
					
				--when ARC =>
					--if element (c).segment_arc.start_point = point then
						--proceed := false;
					--end if;
			
			--end case;

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


	function is_vertex (
		contour	: in type_contour;
		point	: in type_point)
		return boolean
	is
		proceed : aliased boolean := true;

		procedure query_segment (c : in pac_contour_segments.cursor) is begin
			case element (c).shape is
				when LINE =>
					if element (c).segment_line.start_point = point then
						proceed := false;
					end if;
					
				when ARC =>
					if element (c).segment_arc.start_point = point then
						proceed := false;
					end if;
			
			end case;
		end query_segment;
		
	begin
		-- CS do something in case the contour is just a circle
	
		iterate (
			segments	=> contour.contour.segments,
			process		=> query_segment'access,
			proceed		=> proceed'access);

		return not proceed;
	end is_vertex;

	

	function get_lower_left_corner (
		contour	: in type_contour)
		return type_lower_left_corner
	is
		result : type_lower_left_corner;

		boundaries : constant type_boundaries := get_boundaries (contour, zero);
	begin
		-- compose the lower left corner point:
		result.point := type_point (set (boundaries.smallest_x, boundaries.smallest_y));

		-- figure out whether the point is real or virtual:
		case get_point_to_contour_status (contour, result.point).location is
			when INSIDE =>
				result.status := REAL;
				
			when OUTSIDE | ON_EDGE | ON_VERTEX =>
				result.status := VIRTUAL;
		end case;
		
		return result;
	end get_lower_left_corner;


	
	
-- private

	
	function get_shortest_distance (
		contour	: in type_contour;
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

		
		procedure query_segment (c : in pac_contour_segments.cursor) is
			s : constant type_contour_segment := element (c);
		begin
			case s.shape is
				when LINE =>
					--put_line (to_string (s.segment_line));
					update (get_shortest_distance (point, s.segment_line));

				when ARC =>
					--put_line (to_string (s.segment_arc));
					update (get_shortest_distance (point, s.segment_arc));
					
			end case;
		end query_segment;

		
	begin -- get_shortest_distance
		if contour.contour.circular then
			result := get_shortest_distance (point, contour.contour.circle);
		else
			contour.contour.segments.iterate (query_segment'access);				
		end if;			

		--set_absolute (result, type_distance (round (get_absolute (result))));
		
		return result;
	end get_shortest_distance;


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

		--procedure query_segment (c : in pac_polygon_segments.cursor) is begin
			--case element (c).shape is
				--when LINE =>
					--if to_vector (element (c).segment_line.start_point) = point then
						--proceed := false;
					--end if;
					
				--when ARC =>
					--if to_vector (element (c).segment_arc.start_point) = point then
						--proceed := false;
					--end if;
			
			--end case;
		--end query_segment;

		procedure query_edge (c : in pac_edges.cursor) is begin
			if to_vector (element (c).start_point) = point then
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
			
			if to_vector (element (c).end_point) = vertex then
				--put_line ("end");
				result.edge_1 := c;
				end_found := true;
			end if;

			if to_vector (element (c).start_point) = vertex then
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


	
	function "<" (left, right : in type_probe_line_intersection_contour)
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


	function to_string (
		i : in type_point_to_contour_status)
		return string
	is
		use ada.strings.unbounded;
		use pac_probe_line_intersections_contour;

		result : unbounded_string;
		
		procedure query_intersection (c : pac_probe_line_intersections_contour.cursor) is begin
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
					& " is " & to_string (i.location) & " of contour. ");

		--end case;

		result := result & "Distance to contour: " & to_string (i.distance) & ".";
				
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
			edge		: in type_line)
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


	
	function get_point_to_contour_status (
		contour		: in type_contour;	
		point		: in type_point)
		return type_point_to_contour_status 
	is
		-- This function bases on the algorithm published at
		-- <http://www.alienryderflex.com/polygon//>
		-- The algorithm has further been extended to detect intersections
		-- with arcs and even circles.

		point_v : type_vector := to_vector (point);
		
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
		result_intersections : pac_probe_line_intersections_contour.list;
		result_distance : type_float_internal := 0.0;
		result_segment : pac_contour_segments.cursor;
		result_neigboring_segments : type_neigboring_segments;

		--vertex : constant type_point := to_point (point);
		
		--line_pre : constant type_line := (
				--start_point	=> point,
				--end_point	=> type_point (set (get_x (point) + 1.0, get_y (point))));
		
		--probe_line : constant type_line_vector := to_line_vector (line_pre);
		probe_line : constant type_line_vector := (
			v_start => point_v,
			v_direction => (1.0, 0.0, 0.0));

		-- For segments that end or start exactly on the Y value of the probe line
		-- we define a threshold:
		y_threshold : constant type_float_internal := get_y (point_v);


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


		function crosses_threshold (
			arc			: in type_arc;
			y_threshold	: in type_float_internal)
			return boolean
		is begin
			if	
				type_float_internal (get_y (arc.start_point)) >= y_threshold and 
				type_float_internal (get_y (arc.end_point))   <  y_threshold then
				return true;
				
			elsif
				type_float_internal (get_y (arc.end_point))   >= y_threshold and 
				type_float_internal (get_y (arc.start_point)) <  y_threshold then
				return true;
				
			else
				return false;
			end if;
		end crosses_threshold;

		
		-- This is the variable for the number of intersections detected.
		-- From this number we will later deduce the position of the given point,
		-- means whether it is inside or outside the polygon:
		it : count_type := 0;

		use pac_probe_line_intersections_contour;

		
		-- This procedure collects the intersection in the return value.
		procedure collect_intersection (
			intersection: in et_geometry_2.type_intersection; -- incl. point and angle
			segment		: in type_intersected_segment)
			--center		: in type_point := origin;
			--radius		: in type_distance_positive := zero)
		is 
			xi : constant type_float_internal := get_x (intersection.vector);
		begin
			-- The intersection will be collected if it is ON or
			-- AFTER the given start point. If it is before the start
			-- point then we ignore it:
			--if xi >= type_float_internal (get_x (point)) then
			if xi >= get_x (point_v) then
				
				append (result_intersections, (
					x_position	=> xi,
					angle		=> intersection.angle,
					segment		=> segment));

			end if;
		end collect_intersection;

		
		procedure query_line (l : in type_line) is 
			-- Find out whether there is an intersection of the probe line
			-- and the candidate edge of the polygon.
			i : constant type_intersection_of_two_lines := 
				get_intersection (probe_line, l);
			
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
				if crosses_threshold (l, y_threshold) then
					--put_line ("crosses threshold");
					
					-- Add the intersection to the result:
					collect_intersection (i.intersection, (LINE, l));
				end if;
			end if;				
		end query_line;


		
		procedure query_arc (a : in type_arc) is
			a_norm : constant type_arc := type_arc (normalize_arc (a));
			
			-- the radius of the arc:
			radius : constant type_distance_positive := get_radius_start (a_norm);
			
			-- Find out whether there is an intersection of the probe line
			-- and the candidate arc of the polygon.
			i : constant type_intersection_of_line_and_circle := 
				get_intersection (probe_line, a_norm);

			-- In case we get two intersections (which speaks for a secant)
			-- then they need to be ordered according to their distance to
			-- the start point of the probe line (starts at given point);
			ordered_intersections : type_ordered_line_circle_intersections_2;

			
			procedure count_two is begin
				-- Add the two intersections to the result:
				collect_intersection (
					intersection	=> ordered_intersections.entry_point,
					segment			=> (ARC, a_norm));

				collect_intersection (
					intersection	=> ordered_intersections.exit_point,	
					segment			=> (ARC, a_norm));

			end count_two;

			
		begin -- query_arc
			--put_line ("##");
			--put_line (to_string (a_norm));
			
			case i.status is
				when NONE_EXIST => null;
					--put_line ("none");
				
				when ONE_EXISTS =>
					--put_line ("one");
					
					case i.tangent_status is
						when TANGENT => null; -- not counted
						
						when SECANT =>
							--put_line ("secant");
							
							if crosses_threshold (a_norm, y_threshold) then
								--put_line ("ct");
								
								-- The line intersects the arc at one point.
								-- Start and end point of the arc are opposide 
								-- of each other with the probe line betweeen them:

								collect_intersection (
									intersection	=> i.intersection,	
									segment			=> (ARC, a_norm));
								
							end if;
					end case;

				when TWO_EXIST =>
					--put_line ("two");
					--put_line ("i1" & to_string (i.intersection_1));
					--put_line ("i2" & to_string (i.intersection_2));
					
					-- Order the intersections by their distance to the start point
					-- of the probe line:
					ordered_intersections := order_intersections (
						start_point		=> point_v,
						intersections	=> i);

					count_two;
										
			end case;
		end query_arc;

		
		
		procedure query_segment (c : in pac_contour_segments.cursor) is begin
			case element (c).shape is					
				when LINE	=> query_line (element (c).segment_line);
				when ARC	=> query_arc (element (c).segment_arc);
			end case;
		end query_segment;

		
		procedure query_circle (c : in type_circle) is
			-- Find out whether there is an intersection of the probe line
			-- and the candidate circle of the polygon.
			i : constant type_intersection_of_line_and_circle := 
				get_intersection (probe_line, c);

			-- In case we get two intersections (which speaks for a secant)
			-- then they need to be ordered according to their distance to
			-- the start point of the probe line (starts at given point);
			ordered_intersections : type_ordered_line_circle_intersections_2;
		begin				
			case i.status is
				when NONE_EXIST | ONE_EXISTS => null;
					-- NOTE: If the probe line is a tangent to the
					-- circle, then we threat this NOT as intersection.
				
				when TWO_EXIST =>
					-- The probe line intersects the circle at two points:

					-- Order the intersections by their distance to the start point:
					ordered_intersections := order_intersections (
						start_point		=> point_v,
						intersections	=> i);

					
					-- Add the intersections to the result:
					collect_intersection (
						intersection	=> ordered_intersections.entry_point,	
						segment			=> (CIRCLE, c));

					collect_intersection (
						intersection	=> ordered_intersections.exit_point,	
						segment			=> (CIRCLE, c));

			end case;
		end query_circle;

		
		procedure sort_x_values is
			package pac_probe_line_intersections_sorting is new 
				pac_probe_line_intersections_contour.generic_sorting;
			
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

		
	begin -- get_point_to_contour_status
		
		--put_line ("Y-threshold:" & to_string (y_threshold));
		
		if contour.contour.circular then
			query_circle (contour.contour.circle);
		else
			contour.contour.segments.iterate (query_segment'access);
		end if;

		
		-- The x-values are not sorted yet. We need them sorted with the
		-- smallest x first
		-- CS: and redundant x-positions removed: -- no longer required
		sort_x_values;

		-- get the total number of intersections
		it := pac_probe_line_intersections_contour.length (result_intersections);
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
		if is_vertex (contour, point) then
			result_status := ON_VERTEX;
			-- NOTE: result.distance is zero by default

			-- Get the edges that meet at the given point:
			result_neigboring_segments := get_neigboring_segments (contour, point);
		else
			result_segment := get_segment (contour, point);

			if result_segment /= pac_contour_segments.no_element then
				result_status := ON_EDGE;
				-- NOTE: result.distance is zero by default
			else
				-- Point is somewhere else.
				
				-- Compute the distance of the given point to the polygon.
				-- If the distance is zero then the given point lies on
				-- a vertex or on an edge:
				result_distance := get_shortest_distance (contour, point_v); 
			end if;
			
		end if;


		-- Assemble the return:
		case result_status is
			when INSIDE =>
				return (INSIDE, point, result_intersections, result_distance);
					
			when OUTSIDE =>
				return (OUTSIDE, point, result_intersections, result_distance);
				
			when ON_EDGE =>
				return (ON_EDGE, point, result_intersections, result_segment);
				
			when ON_VERTEX =>
				return (ON_VERTEX, point, result_intersections, result_neigboring_segments);
				
		end case;
		
	end get_point_to_contour_status;


	
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
		line	: in type_line;
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
		if point = to_vector (line.start_point) then
			point_on_start := true;
		end if;

		if point = to_vector (line.end_point) then
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
		reference		: in type_point)
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
		line	: in type_line)
		return type_line_to_polygon_status
	is
		result : type_line_to_polygon_status;

		line_center : type_point;
		
		procedure set_line_start is 
			PPS : constant type_point_to_polygon_status := 
				get_point_to_polygon_status (polygon, to_vector (line.start_point));
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
				get_point_to_polygon_status (polygon, to_vector (line.end_point));
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
					get_intersection (element (c), line);

				I_rounded : type_vector;
				IP : type_point;
	
			begin
				-- We are interested in an edge that DOES intersect in some way
				-- the given line. Otherwise the candidate edge is to be skipped:
				if I2L.status = EXISTS then
					IP := to_point (I2L.intersection.vector);

					-- If the intersection is right on the start or the end
					-- of the given line the the candidate edge is to be skipped:
					if IP = line.start_point 
					or IP = line.end_point
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
				POC : constant type_point_of_contact := get_direction (polygon, line, i.position);
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
							get_direction (polygon, line, to_vector (line.start_point));
					begin
						result.start_point.direction_on_edge := POC.direction;
					end;
					
				when ON_VERTEX =>
					declare
						POC : constant type_point_of_contact := 
							get_direction (polygon, line, to_vector (line.start_point));
					begin
						result.start_point.direction_on_vertex := POC.direction;
					end;
			end case;

			-- In case the end point of the line lies on an edge or on a vertex
			-- then assign the direction of this intersection:
			case result.end_point.location is
				when OUTSIDE | INSIDE =>
					null; -- not an intersection -> nothing to do
					
				when ON_EDGE =>
					declare
						POC : constant type_point_of_contact := 
							get_direction (polygon, line, to_vector (line.end_point));
					begin
						result.end_point.direction_on_edge := POC.direction;
					end;
					
				when ON_VERTEX =>
					declare
						POC : constant type_point_of_contact := 
							get_direction (polygon, line, to_vector (line.end_point));
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
			line_center := get_center (line);

			--put_line ("center" & to_string (line_center));
			
			case get_point_to_polygon_status (polygon, to_vector (line_center)).location is
				when INSIDE => result.center_point := INSIDE;
				when OUTSIDE => result.center_point := OUTSIDE;
				when ON_EDGE | ON_VERTEX => result.center_point := ON_EDGE;
			end case;
			
		end if;

		-- Sort intersections by their distance to the start point.
		sort_by_distance (result.intersections, line.start_point);
		
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
				get_point_to_polygon_status (polygon_B, to_vector (element (c).start_point));
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
		reference	: in type_point)
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
			edge : type_line;
		begin
			-- The candidate vertex becomes the end of 
			-- the edge:
			edge.end_point := to_point (element (v).position);

			-- The vertex before the candidate vertex 
			-- will be the start of the edge:
			if v = vertices.first then
				edge.start_point := to_point (element (vertices.last).position);
			else
				edge.start_point := to_point (element (previous (v)).position);
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
				IAB.position := to_vector (element (a).start_point);
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
				IAB.position := to_vector (element (a).end_point);
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
			edge	: in type_line)
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
			position := to_vector (element (s).start_point);
			
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

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

	use pac_polygon_segments;


	function to_string (
		segment	: in type_polygon_segment)
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
		segments	: in pac_polygon_segments.list;
		process		: not null access procedure (position : in pac_polygon_segments.cursor);
		proceed		: not null access boolean)
	is
		c : pac_polygon_segments.cursor := segments.first;
	begin
		while c /= pac_polygon_segments.no_element and proceed.all = TRUE loop
			process (c);
			next (c);
		end loop;
	end iterate;



	function get_segment_edge (
		polygon	: in type_polygon_base;
		edge	: in type_line)
		return pac_polygon_segments.cursor
	is
		result : pac_polygon_segments.cursor;
		segment : type_polygon_segment := (shape => LINE, segment_line => edge);
	begin
		result := polygon.contours.segments.find (segment);
		return result;
	end get_segment_edge;
	


	function get_segment_edge (
		polygon	: in type_polygon_base;
		point	: in type_point)
		return pac_polygon_segments.cursor
	is
		result : pac_polygon_segments.cursor;
		proceed : aliased boolean := true;

		procedure query_segment (c : in pac_polygon_segments.cursor) is begin
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
		if is_vertex (polygon, point) then
			raise constraint_error with "Point is a vertex !";
		else
			iterate (polygon.contours.segments, query_segment'access, proceed'access);					 
		end if;

		-- CS exception message if polygon consists of just a circle.
		return result;
	end get_segment_edge;


	function get_segment_edge (
		polygon	: in type_polygon_base;
		point	: in type_vector)
		return pac_polygon_segments.cursor
	is
		result : pac_polygon_segments.cursor;
		proceed : aliased boolean := true;

		procedure query_segment (c : in pac_polygon_segments.cursor) is begin
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
		if is_vertex (polygon, point) then
			raise constraint_error with "Point is a vertex !";
		else
			iterate (polygon.contours.segments, query_segment'access, proceed'access);					 
		end if;

		-- CS exception message if polygon consists of just a circle.
		return result;
	end get_segment_edge;



	
	function get_neigboring_edges (
		polygon	: in type_polygon_base;
		vertex	: in type_point)
		return type_neigboring_edges
	is
		result : type_neigboring_edges;
		proceed : aliased boolean := true;

		end_found, start_found : boolean := false;
		
		procedure query_segment (c : in pac_polygon_segments.cursor) is begin
			--put_line ("test: " & to_string (element (c)));
			
			case element (c).shape is
			
				when LINE => 
					if element (c).segment_line.end_point = vertex then
						--put_line ("end");
						result.edge_1 := c;
						end_found := true;
					end if;

					if element (c).segment_line.start_point = vertex then
						--put_line ("start");
						result.edge_2 := c;
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
		if is_vertex (polygon, vertex) then
			iterate (polygon.contours.segments, query_segment'access, proceed'access);					 
		else
			raise constraint_error with "Point is a vertex !";
		end if;

		-- Safety check:
		-- Two edges must have been found. Otherwise raise exception:
		if result.edge_1 = pac_polygon_segments.no_element 
		or result.edge_2 = pac_polygon_segments.no_element
		then
			raise constraint_error with "Search for neigboring edges incomplete !";
		end if;

		-- CS exception message if polygon consists of just a circle.
		return result;
	end get_neigboring_edges;


	function get_neigboring_edges (
		polygon	: in type_polygon_base;
		vertex	: in type_vector)
		return type_neigboring_edges
	is
		result : type_neigboring_edges;
		proceed : aliased boolean := true;

		end_found, start_found : boolean := false;
		
		procedure query_segment (c : in pac_polygon_segments.cursor) is begin
			--put_line ("test: " & to_string (element (c)));
			
			case element (c).shape is
			
				when LINE => 
					if to_vector (element (c).segment_line.end_point) = vertex then
						--put_line ("end");
						result.edge_1 := c;
						end_found := true;
					end if;

					if to_vector (element (c).segment_line.start_point) = vertex then
						--put_line ("start");
						result.edge_2 := c;
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
		if is_vertex (polygon, vertex) then
			iterate (polygon.contours.segments, query_segment'access, proceed'access);					 
		else
			raise constraint_error with "Point is a vertex !";
		end if;

		-- Safety check:
		-- Two edges must have been found. Otherwise raise exception:
		if result.edge_1 = pac_polygon_segments.no_element 
		or result.edge_2 = pac_polygon_segments.no_element
		then
			raise constraint_error with "Search for neigboring edges incomplete !";
		end if;

		-- CS exception message if polygon consists of just a circle.
		return result;
	end get_neigboring_edges;
	

	
	function to_string (
		polygon	: in type_polygon_base)
		return string
	is
		use ada.strings.unbounded;
		
		result : unbounded_string := to_unbounded_string ("polygon:");

		procedure query_segment (c : in pac_polygon_segments.cursor) is begin

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
		if polygon.contours.circular then
			result := result & space & to_unbounded_string (to_string (polygon.contours.circle));
		else
			polygon.contours.segments.iterate (query_segment'access);
		end if;
		
		return to_string (result);
	end to_string;



	
	
	function get_nearest_corner_point (
		polygon		: in type_polygon_base;
		reference	: in type_point)
		return type_point
	is
		result : type_point := origin;
		
		d1 : type_distance_positive := zero;
		d2 : type_distance_positive := get_absolute (get_distance (reference, far_upper_right));

		procedure query_segment (c : in pac_polygon_segments.cursor) is
			s : constant type_polygon_segment := element (c);
		begin
			case s.shape is
				when LINE =>

					-- test start point
					d1 := get_absolute (get_distance (reference, s.segment_line.start_point));
					
					if d1 < d2 then
						d2 := d1;
						
						result := s.segment_line.start_point;
					end if;

					-- test end point
					d1 := get_absolute (get_distance (reference, s.segment_line.end_point));
					
					if d1 < d2 then
						d2 := d1;
						
						result := s.segment_line.end_point;
					end if;

				when ARC =>
					-- test start point
					d1 := get_absolute (get_distance (reference, s.segment_arc.start_point));
					
					if d1 < d2 then
						d2 := d1;
						
						result := s.segment_arc.start_point;
					end if;

					-- test end point
					d1 := get_absolute (get_distance (reference, s.segment_arc.end_point));
					
					if d1 < d2 then
						d2 := d1;
						
						result := s.segment_arc.end_point;
					end if;
					
			end case;
		end query_segment;
		
	begin
		if polygon.contours.circular then
			raise constraint_error with 
			"Polygon consists of a single circle and thus nas no corners !";
		else
			polygon.contours.segments.iterate (query_segment'access);				
		end if;			
		
		return result;
	end get_nearest_corner_point;

	
	function get_shortest_distance (
		polygon	: in type_polygon_base;
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

		
		procedure query_segment (c : in pac_polygon_segments.cursor) is
			s : constant type_polygon_segment := element (c);
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
		if polygon.contours.circular then
			result := get_shortest_distance (point, polygon.contours.circle);
		else
			polygon.contours.segments.iterate (query_segment'access);				
		end if;			

		--set_absolute (result, type_distance (round (get_absolute (result))));
		
		return result;
	end get_shortest_distance;



	function get_shortest_distance (
		polygon	: in type_polygon_base;
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

		
		procedure query_segment (c : in pac_polygon_segments.cursor) is
			s : constant type_polygon_segment := element (c);
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
		if polygon.contours.circular then
			result := get_shortest_distance (point, polygon.contours.circle);
		else
			polygon.contours.segments.iterate (query_segment'access);				
		end if;			

		--set_absolute (result, type_distance (round (get_absolute (result))));
		
		return result;
	end get_shortest_distance;

	
	
	function get_left_end (
		line		: in type_line;
		boundaries	: in type_boundaries := boundaries_default)
		return type_point
	is
		p : type_point; -- to be returned
		b : type_boundaries := boundaries;								   
	begin
		-- If no boundaries provided, compute them.
		-- Otherwise use provided boundaries as they are:
		if b = boundaries_default then
			b := get_boundaries (line, zero); -- a polygon line has zero width
		end if;
		
		if b.smallest_x = get_x (line.start_point) then
			p := line.start_point;
			
		elsif b.smallest_x = get_x (line.end_point) then
			p := line.end_point;

		else
			-- If boundaries where provided and neither start nor end point
			-- of line matches, issue exception:
			raise constraint_error with to_string (b) 
				& " invalid for line" & to_string (line) & " !";
		end if;
			
		return p;
	end get_left_end;

	
	function get_right_end (
		line		: in type_line;
		boundaries	: in type_boundaries := boundaries_default)
		return type_point
	is
		p : type_point; -- to be returned
		b : type_boundaries := boundaries;								   
	begin
		-- If no boundaries provided, compute them.
		-- Otherwise use provided boundaries as they are:
		if b = boundaries_default then
			b := get_boundaries (line, zero); -- a polygon line has zero width
		end if;
		
		if b.greatest_x = get_x (line.start_point) then
			p := line.start_point;
			
		elsif b.greatest_x = get_x (line.end_point) then
			p := line.end_point;

		else
			-- If boundaries where provided and neither start nor end point
			-- of line matches, issue exception:
			raise constraint_error with to_string (b) 
				& " invalid for line" & to_string (line) & " !";
		end if;
			
		return p;
	end get_right_end;

	
	function get_lower_end (
		line		: in type_line;
		boundaries	: in type_boundaries := boundaries_default)
		return type_point
	is
		p : type_point; -- to be returned
		b : type_boundaries := boundaries;								   
	begin
		-- If no boundaries provided, compute them.
		-- Otherwise use provided boundaries as they are:
		if b = boundaries_default then
			b := get_boundaries (line, zero); -- a polygon line has zero width
		end if;
		
		if b.smallest_y = get_x (line.start_point) then
			p := line.start_point;
			
		elsif b.smallest_y = get_x (line.end_point) then
			p := line.end_point;

		else
			-- If boundaries where provided and neither start nor end point
			-- of line matches, issue exception:
			raise constraint_error with to_string (b) 
				& " invalid for line" & to_string (line) & " !";
		end if;
			
		return p;
	end get_lower_end;

	
	function get_upper_end (
		line		: in type_line;
		boundaries	: in type_boundaries := boundaries_default)
		return type_point
	is
		p : type_point; -- to be returned
		b : type_boundaries := boundaries;								   
	begin
		-- If no boundaries provided, compute them.
		-- Otherwise use provided boundaries as they are:
		if b = boundaries_default then
			b := get_boundaries (line, zero); -- a polygon line has zero width
		end if;
		
		if b.greatest_y = get_x (line.start_point) then
			p := line.start_point;
			
		elsif b.greatest_y = get_x (line.end_point) then
			p := line.end_point;

		else
			-- If boundaries where provided and neither start nor end point
			-- of line matches, issue exception:
			raise constraint_error with to_string (b) 
				& " invalid for line" & to_string (line) & " !";
		end if;
			
		return p;
	end get_upper_end;


	--function get_segments_on_corner_point (
		--polygon	: in type_polygon_base;
		--corner	: in type_point)
		--return type_polygon_segments
	--is
		--use pac_polygon_lines;
		--use pac_polygon_arcs;
		
		--result : type_polygon_segments;

		--procedure query_line (c : in pac_polygon_lines.cursor) is
			--line : constant type_polygon_line := element (c);
		--begin
			--if line.start_point = corner then
				--result.lines.append (line);
				
			--elsif line.end_point = corner then
				--result.lines.append (line);
			--end if;
		--end query_line;

		--procedure query_arc (c : in pac_polygon_arcs.cursor) is
			--arc : constant type_polygon_arc := element (c);
		--begin
			--if arc.start_point = corner then
				--result.arcs.append (arc);
				
			--elsif arc.end_point = corner then
				--result.arcs.append (arc);
			--end if;
		--end query_arc;
		
	--begin
		--polygon.segments.lines.iterate (query_line'access);
		--polygon.segments.arcs.iterate (query_arc'access);
		
		---- circles are not tested, because a circle does not have corners
		
		--return result;
	--end get_segments_on_corner_point;

	
	procedure load_segments (
		polygon		: in out type_polygon_base;
		segments	: in type_polygon_segments)
	is begin
		polygon.contours := segments;
	end load_segments;

	
	procedure delete_segments (polygon : in out type_polygon_base) 
	is begin
		polygon.contours := (others => <>);
	end delete_segments;			

	
	procedure append_segment (
		polygon	: in out type_polygon_base;
		segment	: in type_polygon_segment)
	is begin
		-- CS check discriminant and issue helpful error message ?

		-- CS length check for line:
		--if get_length (line) > type_distance'small then
			--return line;
		--else
			--raise constraint_error with "Line has zero length !";
		--end if;

		-- CS length check for arc:		
		
		polygon.contours.segments.append (segment);			
	end append_segment;

	
	procedure set_circle (
		polygon	: in out type_polygon_base;
		circle	: in type_circle'class)
	is begin
		-- CS check discriminant and issue helpful error message ?
		
		polygon.contours.circle := type_circle (circle);
	end set_circle;
				
	
	function get_segments (
		polygon : in type_polygon_base) 
		return type_polygon_segments
	is begin
		return polygon.contours;
	end get_segments;


	function get_segments_total (
		polygon : in type_polygon_base)
		return count_type
	is begin
		if polygon.contours.circular then
			return 1;
		else
			return length (polygon.contours.segments);
		end if;
	end get_segments_total;

	--function get_dimensions (
		--polygon : in type_polygon_base)
		--return type_dimensions
	--is
		--result : type_dimensions;

		--procedure update_greatest_x (p : in type_point) is 
			--d : type_distance := X (p);
		--begin
			--if d > X (result.greatest) then
				----put_line ("X" & to_string (d));
				--set (axis => X, value => d, point => result.greatest);
			--end if;
		--end update_greatest_x;
			
		--procedure update_greatest_y (p : in type_point) is 
			--d : type_distance := Y (p);
		--begin
			--if d > Y (result.greatest) then
				--set (axis => Y, value => d, point => result.greatest);
			--end if;
		--end update_greatest_y;

		--procedure update_smallest_x (p : in type_point) is 
			--d : type_distance := X (p);
		--begin
			--if d < X (result.smallest) then
				--set (axis => X, value => d, point => result.smallest);
			--end if;
		--end update_smallest_x;
			
		--procedure update_smallest_y (p : in type_point) is 
			--d : type_distance := Y (p);
		--begin
			--if d < Y (result.smallest) then
				--set (axis => Y, value => d, point => result.smallest);
			--end if;
		--end update_smallest_y;

		--use pac_polygon_segments;

		--procedure query_segment (c : in pac_polygon_segments.cursor) is 
		--begin
			--case element (c).shape is
				--when LINE =>
					--update_greatest_x (element (c).segment_line.start_point);
					--update_greatest_y (element (c).segment_line.start_point);
					--update_smallest_x (element (c).segment_line.start_point);
					--update_smallest_y (element (c).segment_line.start_point);

					--update_greatest_x (element (c).segment_line.end_point);
					--update_greatest_y (element (c).segment_line.end_point);
					--update_smallest_x (element (c).segment_line.end_point);
					--update_smallest_y (element (c).segment_line.end_point);

				--when ARC =>
					--null; -- CS

			--end case;
		--end query_segment;
		
	--begin
		--if polygon.contours.circular then
			--null; -- CS
		--else
			--iterate (polygon.contours.segments, query_segment'access);
		--end if;
		
		--return result;
	--end get_dimensions;

	
	procedure transpose_polygon (
		polygon	: in out type_polygon_base'class;
		offset	: in type_distance)
	is 
		procedure move (point : in out type_point) is
			new_y : type_distance;
		begin
			new_y := offset - get_y (point);
			set (Y, new_y, point);
		end move;

		
		procedure move_segment (c : in pac_polygon_segments.cursor) is

			procedure do_line (s : in out type_polygon_segment) is begin 
				move (s.segment_line.start_point);
				move (s.segment_arc.end_point);
			end;
			
			procedure do_arc (s : in out type_polygon_segment) is begin
				move (s.segment_arc.start_point);
				move (s.segment_arc.end_point); 
				move (s.segment_arc.center); 
			end;

		begin -- move_segment
			case element (c).shape is
				
				when LINE =>
					update_element (
						container	=> polygon.contours.segments,
						position	=> c,
						process		=> do_line'access);

				when ARC =>
					update_element (
						container	=> polygon.contours.segments,
						position	=> c,
						process		=> do_arc'access);

			end case;
		end move_segment;

		
	begin -- transpose_polygon			
		if polygon.contours.circular then

			-- move the single circle that forms the polygon:
			move (polygon.contours.circle.center);
		else
			-- move lines and arcs:
			polygon.contours.segments.iterate (move_segment'access);
		end if;
		
	end transpose_polygon;

	
	function to_polygon (
		arguments : in type_fields_of_line) -- line 0 0 line 160 0 line 160 80 line 0 80
		return type_polygon_base'class
	is
		-- CS: to do:
		-- - length check for lines and arc segments
		-- - merge neigboring line segments that run in the same direction
		
		result : type_polygon; -- will be converted back to class type on return
		-- NOTE: The default discriminant if the result makes it a
		-- polygon consisting of lines an arcs.
		-- If a circle is to be added to the polygon then the discriminant
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
		polygon_start_point : type_point; -- start point of polygon

		end_point_previous : type_point;

		
		procedure update_end_point (s : in out type_polygon_segment) is begin
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
				-- polygon segments will cause a exception.
				result.contours := (
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
							container	=> result.contours.segments,
							position	=> result.contours.segments.last,
							process		=> update_end_point'access);
					
					else
						-- register the start point of the polygon
						polygon_start_point := l.start_point;
						start_point_set := true;
					end if;
					
					result.contours.segments.append (new_item => (LINE, l));

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
							container	=> result.contours.segments,
							position	=> result.contours.segments.last,
							process		=> update_end_point'access);
					
					else
						-- register the start point of the polygon
						polygon_start_point := a.start_point;
						start_point_set := true;
					end if;

					-- assign direction of arc
					a.direction := to_direction (f (p + 5));
					
					result.contours.segments.append (new_item => (ARC, a));

					-- fast forward p to next shape:						
					p := p + 6;

					
					
				when CIRCLE => -- circle 40 40 10

					-- assign center of circle
					c.center := type_point (set (
							x => to_distance (f (p + 1)),
							y => to_distance (f (p + 2))));

					-- assigne radius of circle
					c.radius := to_distance (f (p + 3));

					result.contours.circle := c;
					
					-- NOTE: No further shape must follow.
					-- So we do not move p further forward 
					-- and abort this loop:
					exit;
					
			end case;
		end loop;

		
		-- If the polygon consists of lines and/or arcs then 
		-- the end point of the last segment is where the polygon
		-- has started:
		if shape /= CIRCLE then
			
			end_point_previous := polygon_start_point;
			
			-- Assign the end point of the last segment:
			update_element (
				container	=> result.contours.segments,
				position	=> result.contours.segments.last,
				process		=> update_end_point'access);

		end if;
			
		return type_polygon_base (result);

		-- CS exception handler required for invalid fields:
		
		--exception when event: others =>
			--put_line (exception_message);
			--return p;
	
	end to_polygon;
	

	
	function get_boundaries (
		polygon		: in type_polygon_base;
		line_width	: in type_distance_positive)
		return type_boundaries 
	is
		result : type_boundaries; -- to be returned

		half_width : constant type_distance_positive := line_width * 0.5;
		

		procedure query_segment (c : in pac_polygon_segments.cursor) is begin
			case element (c).shape is
				when LINE =>
					union (result, get_boundaries (element (c).segment_line, zero));

				when ARC =>
					union (result, get_boundaries (element (c).segment_arc, zero));
			end case;						
		end query_segment;

		
	begin -- get_boundaries
		if polygon.contours.circular then

			-- Get the boundaries of the single circle:
			union (result, get_boundaries (polygon.contours.circle, zero));
			
		else
			-- Iterate lines and arcs:
			polygon.contours.segments.iterate (query_segment'access);
		end if;

					
		-- Extend the boundaries by half the line width;
		result.smallest_x := result.smallest_x - half_width;
		result.smallest_y := result.smallest_y - half_width;

		result.greatest_x := result.greatest_x + half_width;
		result.greatest_y := result.greatest_y + half_width;
		
		return result;
	end get_boundaries;
	
	
	function to_string (
		gaps : in pac_polygon_gaps.list) 
		return string 
	is
		use pac_polygon_gaps;
		use ada.strings.unbounded;
		use ada.characters.latin_1;
		
		result : unbounded_string;
		
		procedure query_gap (g : in pac_polygon_gaps.cursor) is 
			scratch : unbounded_string;
		begin
			if next (g) /= pac_polygon_gaps.no_element then
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
		polygon	: in type_polygon_base)
		return type_polygon_status 
	is
		-- Goes false once a gap has been detected:
		closed : boolean := true;

		-- The point where the polyon outline starts:
		start_point		: type_point;

		-- The end point of a segment. Once the last segment has been processed,
		-- this point must match the start point:
		last_end_point	: type_point;

		-- Goes true once a start point has been set:
		started : boolean := false;

		-- Here we collect the points where a gap begins:
		use pac_polygon_gaps;
		gaps : pac_polygon_gaps.list;

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

		procedure query_segment (c : in pac_polygon_segments.cursor) is begin
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
		
		if polygon.contours.circular then
			closed := true; -- because this is a single circle
		else
			-- Iterate lines and arcs:
			polygon.contours.segments.iterate (query_segment'access);
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
		polygon	: in out type_polygon_base;
		offset	: in type_distance_relative) 
	is

		procedure move_segment (c : in pac_polygon_segments.cursor) is

			procedure do_line (s : in out type_polygon_segment) is begin 
				move_by (s.segment_line, offset);
			end;
			
			procedure do_arc (s : in out type_polygon_segment) is begin
				move_by (s.segment_arc, offset);
			end;

		begin -- move_segment
			case element (c).shape is
				
				when LINE =>
					update_element (
						container	=> polygon.contours.segments,
						position	=> c,
						process		=> do_line'access);

				when ARC =>
					update_element (
						container	=> polygon.contours.segments,
						position	=> c,
						process		=> do_arc'access);

			end case;
		end move_segment;
		
	begin -- move_by
		if polygon.contours.circular then

			-- move the single circle that forms the polygon:
			move_by (polygon.contours.circle, offset);
		else
			-- move lines and arcs:
			polygon.contours.segments.iterate (move_segment'access);
		end if;
	end move_by;

	
	procedure mirror (
		polygon	: in out type_polygon_base;
		axis	: in type_axis_2d) 
	is

		procedure mirror_segment (c : in pac_polygon_segments.cursor) is

			procedure do_line (s : in out type_polygon_segment) is begin 
				mirror (s.segment_line, axis);
			end;
			
			procedure do_arc (s : in out type_polygon_segment) is begin
				mirror (s.segment_arc, axis);
			end;

		begin -- move_segment
			case element (c).shape is
				
				when LINE =>
					update_element (
						container	=> polygon.contours.segments,
						position	=> c,
						process		=> do_line'access);

				when ARC =>
					update_element (
						container	=> polygon.contours.segments,
						position	=> c,
						process		=> do_arc'access);

			end case;
		end mirror_segment;
		
	begin -- mirror
		if polygon.contours.circular then

			-- mirror the single circle that forms the polygon:
			mirror (polygon.contours.circle, axis);
		else
			-- move lines and arcs:
			polygon.contours.segments.iterate (mirror_segment'access);
		end if;
	end mirror;

	
	procedure rotate_by (
		polygon		: in out type_polygon_base;
		rotation	: in type_rotation) 
	is

		procedure rotate_segment (c : in pac_polygon_segments.cursor) is

			procedure do_line (s : in out type_polygon_segment) is begin 
				rotate_by (s.segment_line, rotation);
			end;
			
			procedure do_arc (s : in out type_polygon_segment) is begin
				rotate_by (s.segment_arc, rotation);
			end;

		begin -- rotate_segment
			case element (c).shape is
				
				when LINE =>
					update_element (
						container	=> polygon.contours.segments,
						position	=> c,
						process		=> do_line'access);

				when ARC =>
					update_element (
						container	=> polygon.contours.segments,
						position	=> c,
						process		=> do_arc'access);

			end case;
		end rotate_segment;

		
	begin -- rotate_by
		if polygon.contours.circular then

			-- rotate the single circle that forms the polygon:
			rotate_by (polygon.contours.circle.center, rotation);
		else
			-- rotate lines and arcs:
			polygon.contours.segments.iterate (rotate_segment'access);
		end if;			
	end rotate_by;

	
	function to_string (scale : in type_polygon_scale) return string is begin
		return type_polygon_scale'image (scale);
	end to_string;

	function to_scale (scale : in string) return type_polygon_scale is begin
		return type_polygon_scale'value (scale);
	end to_scale;



	
	
	procedure scale_polygon (
		polygon	: in out type_polygon_base;
		scale	: in type_polygon_scale) 
	is
		
		function scale_point (point	: in type_point) return type_point is
			x_new : type_distance := get_x (point) * scale;
			y_new : type_distance := get_y (point) * scale;
		begin
			return type_point (set (x_new, y_new));
		end scale_point;

		
		
		procedure do_segment (c : in pac_polygon_segments.cursor) is

			
			procedure do_line (s : in out type_polygon_segment) is begin 
				s.segment_line.start_point	:= scale_point (s.segment_line.start_point);
				s.segment_line.end_point	:= scale_point (s.segment_line.end_point);
			end;

			
			procedure do_arc (s : in out type_polygon_segment) is begin
				null; -- CS
			end;

			
		begin
			case element (c).shape is
				
				when LINE =>
					update_element (
						container	=> polygon.contours.segments,
						position	=> c,
						process		=> do_line'access);

				when ARC =>
					update_element (
						container	=> polygon.contours.segments,
						position	=> c,
						process		=> do_arc'access);

			end case;
		end do_segment;

		
	begin

		if polygon.contours.circular then

			-- scale the single circle that forms the polygon:
			-- CS change radius of  polygon.contours.circle
			null;
		else
			polygon.contours.segments.iterate (do_segment'access);
		end if;
		
	end scale_polygon;

	


	
-- 		function to_corner_easing (easing : in string) return type_corner_easing is begin
-- 			return type_corner_easing'value (easing);
-- 		end;
-- 
-- 		function to_string (easing : in type_corner_easing) return string is begin
-- 			return to_lower (type_corner_easing'image (easing));
-- 		end to_string;


-- 		procedure move (
-- 			polygon : in out type_polygon_base;
-- 			offset	: in type_point) is
-- 		begin
-- 			-- CS move segments of polygon
-- 			null;
-- 		end;


	
	function "<" (left, right : in type_probe_line_intersection)
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
	

	
	function to_string (status : in type_location) return string is begin
		return type_location'image (status);
	end to_string;

	
	
	
	
	function to_string (
		i : in type_point_to_polygon_status)
		return string
	is
		use ada.strings.unbounded;
		use pac_probe_line_intersections;

		result : unbounded_string;
		
		procedure query_intersection (c : pac_probe_line_intersections.cursor) is begin
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
		polygon		: in type_polygon_base;	
		point		: in type_point)
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
		result_intersections : pac_probe_line_intersections.list;
		result_distance : type_distance_polar;
		result_edge : pac_polygon_segments.cursor;
		result_neigboring_edges : type_neigboring_edges;

		
		line_pre : constant type_line := (
				start_point	=> point,
				end_point	=> type_point (set (get_x (point) + 1.0, get_y (point))));
		
		probe_line : constant type_line_vector := to_line_vector (line_pre);

		-- For segments that end or start exactly on the Y value of the probe line
		-- we define a threshold:
		y_threshold : constant type_distance := get_y (point);

		
		-- This is the variable for the number of intersections detected.
		-- From this number we will later deduce the position of the given point,
		-- means whether it is inside or outside the polygon:
		it : count_type := 0;

		use pac_probe_line_intersections;

		
		-- This procedure collects the intersection in the return value.
		procedure collect_intersection (
			intersection: in et_geometry_2.type_intersection; -- incl. point and angle
			segment		: in type_intersected_segment;
			center		: in type_point := origin;
			radius		: in type_distance_positive := zero)
		is 
			xi : constant type_float_internal := get_x (intersection.vector);
		begin
			-- The intersection will be collected if it is ON or
			-- AFTER the given start point. If it is before the start
			-- point then we ignore it:
			if xi >= type_float_internal (get_x (point)) then
				
				append (result_intersections, (
					x_position	=> xi,
					angle		=> intersection.angle,
					segment		=> segment));

			end if;
		end collect_intersection;

		
		procedure query_line (l : in type_line) is 
			-- Find out whether there is an intersection of the probe line
			-- and the candidate line of the polygon.
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
			ordered_intersections : type_ordered_line_circle_intersections;

			
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
						start_point		=> point,
						intersections	=> i);

					count_two;
										
			end case;
		end query_arc;

		
		
		procedure query_segment (c : in pac_polygon_segments.cursor) is begin
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
			ordered_intersections : type_ordered_line_circle_intersections;
		begin				
			case i.status is
				when NONE_EXIST | ONE_EXISTS => null;
					-- NOTE: If the probe line is a tangent to the
					-- circle, then we threat this NOT as intersection.
				
				when TWO_EXIST =>
					-- The probe line intersects the circle at two points:

					-- Order the intersections by their distance to the start point:
					ordered_intersections := order_intersections (
						start_point		=> point,
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
				pac_probe_line_intersections.generic_sorting;
			
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
		
		if polygon.contours.circular then
			query_circle (polygon.contours.circle);
		else
			polygon.contours.segments.iterate (query_segment'access);
		end if;

		
		-- The x-values are not sorted yet. We need them sorted with the
		-- smallest x first
		-- CS: and redundant x-positions removed: -- no longer required
		sort_x_values;

		-- get the total number of intersections
		it := pac_probe_line_intersections.length (result_intersections);
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

			if result_edge /= pac_polygon_segments.no_element then
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



	function get_point_to_polygon_status_2 (
		polygon		: in type_polygon_base;	
		point		: in type_vector) -- CS rename to vector ?
		return type_point_to_polygon_status_2 
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
		result_intersections : pac_probe_line_intersections.list;
		result_distance : type_float_internal := 0.0;
		result_edge : pac_polygon_segments.cursor;
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

		use pac_probe_line_intersections;

		
		-- This procedure collects the intersection in the return value.
		procedure collect_intersection (
			intersection: in et_geometry_2.type_intersection; -- incl. point and angle
			segment		: in type_intersected_segment;
			center		: in type_point := origin;
			radius		: in type_distance_positive := zero)
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
						start_point		=> point,
						intersections	=> i);

					count_two;
										
			end case;
		end query_arc;

		
		
		procedure query_segment (c : in pac_polygon_segments.cursor) is begin
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
						start_point		=> point,
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
				pac_probe_line_intersections.generic_sorting;
			
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
		
		if polygon.contours.circular then
			query_circle (polygon.contours.circle);
		else
			polygon.contours.segments.iterate (query_segment'access);
		end if;

		
		-- The x-values are not sorted yet. We need them sorted with the
		-- smallest x first
		-- CS: and redundant x-positions removed: -- no longer required
		sort_x_values;

		-- get the total number of intersections
		it := pac_probe_line_intersections.length (result_intersections);
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

			if result_edge /= pac_polygon_segments.no_element then
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
		
	end get_point_to_polygon_status_2;


	

	function get_lower_left_corner (
		polygon	: in type_polygon_base)
		return type_lower_left_corner
	is
		result : type_lower_left_corner;

		boundaries : constant type_boundaries := get_boundaries (polygon, zero);
	begin
		-- compose the lower left corner point:
		result.point := type_point (set (boundaries.smallest_x, boundaries.smallest_y));

		-- figure out whether the point is real or virtual:
		case get_point_to_polygon_status (polygon, result.point).location is
			when INSIDE =>
				result.status := REAL;
				
			when OUTSIDE | ON_EDGE | ON_VERTEX =>
				result.status := VIRTUAL;
		end case;
		
		return result;
	end get_lower_left_corner;
	


	function is_vertex (
		polygon	: in type_polygon_base;
		point	: in type_point)
		return boolean
	is
		proceed : aliased boolean := true;

		procedure query_segment (c : in pac_polygon_segments.cursor) is begin
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
		iterate (
			segments	=> polygon.contours.segments,
			process		=> query_segment'access,
			proceed		=> proceed'access);

		return not proceed;
	end is_vertex;


	function is_vertex (
		polygon	: in type_polygon_base;
		point	: in type_vector)
		return boolean
	is
		proceed : aliased boolean := true;

		procedure query_segment (c : in pac_polygon_segments.cursor) is begin
			case element (c).shape is
				when LINE =>
					if to_vector (element (c).segment_line.start_point) = point then
						proceed := false;
					end if;
					
				when ARC =>
					if to_vector (element (c).segment_arc.start_point) = point then
						proceed := false;
					end if;
			
			end case;
		end query_segment;
		
	begin
		iterate (
			segments	=> polygon.contours.segments,
			process		=> query_segment'access,
			proceed		=> proceed'access);

		return not proceed;
	end is_vertex;

	
	procedure toggle_direction (
		d : in out type_intersection_direction) 
	is begin
		case d is
			when ENTERING => d := LEAVING;
			when LEAVING => d := ENTERING;
		end case;
	end toggle_direction;



	function get_direction (
		polygon	: in type_polygon_base;
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
				get_point_to_polygon_status_2 (polygon, SP_before).location;

			PPS_after : constant type_location := 
				get_point_to_polygon_status_2 (polygon, SP_after).location;
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
		polygon	: in type_polygon_base;
		line	: in type_line)
		return type_line_to_polygon_status
	is
		result : type_line_to_polygon_status;

		line_center : type_point;
		
		procedure set_line_start is 
			PPS : constant type_point_to_polygon_status := 
				get_point_to_polygon_status (polygon, line.start_point);
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
				get_point_to_polygon_status (polygon, line.end_point);
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
			
			procedure query_segment (c : in pac_polygon_segments.cursor) is begin
				case element (c).shape is
					when polygons.LINE =>
						declare
							I2L : constant type_intersection_of_two_lines := get_intersection (
								element (c).segment_line, line);

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
								--or IP = element (c).segment_line.start_point	
								--or IP = element (c).segment_line.end_point
								then
									null; -- skip this intersection point entirely
								else
									-- Collect this intersection point if it has
									-- not already been collected yet:

									I_rounded := round (
										vector		=> I2L.intersection.vector, 
										accuracy	=> 17);
									
									--if not contains (result.intersections, I2L.intersection.vector) then
										--result.intersections.append ((
											--position => I2L.intersection.vector, edge => c, others => <>));
										---- The direction will be set later.
									--end if;

									if not contains (result.intersections, I_rounded) then
										result.intersections.append ((
											position => I_rounded, edge => c, others => <>));
										-- The direction will be set later.
									end if;

									
								end if;
							end if;
						end;
						
					when ARC =>
						null; -- CS
						
				end case;
			end query_segment;

		begin
			polygon.contours.segments.iterate (query_segment'access);
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
			
			case get_point_to_polygon_status (polygon, line_center).location is
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

	

	function all_vertices_of_A_inside_B (
		polygon_A	: in type_polygon'class; -- the clipped polygon
		polygon_B	: in type_polygon'class) -- the clipping polygon
		return boolean
	is
		proceed : aliased boolean := true;

		-- Query the start point of segment of polygon A.
		-- The segment is indicated by cursor c.
		-- Aborts the iteration on the first vertex that is
		-- outside polygon B.
		procedure query_segment (c : in pac_polygon_segments.cursor) is begin
			case element (c).shape is
				when LINE =>
					declare
						IPQ : constant type_point_to_polygon_status :=
							get_point_to_polygon_status (polygon_B, element (c).segment_line.start_point);
					begin
						if IPQ.location = OUTSIDE then
							proceed := false; -- abort iteration
						end if;
					end;
					
				when ARC =>
					declare
						IPQ : constant type_point_to_polygon_status := 
							get_point_to_polygon_status (polygon_B, element (c).segment_arc.start_point);
					begin
						if IPQ.location = OUTSIDE then
							proceed := false; -- abort iteration
						end if;
					end;
			end case;
		end query_segment;
		
	begin
		--if polygon_A.contours.circular then
			--null; -- CS
		--else
			iterate (
				segments	=> polygon_A.contours.segments,
				process		=> query_segment'access,
				proceed		=> proceed'access);
			
			--end if;
			
		return proceed;
	end all_vertices_of_A_inside_B;

	
end et_geometry_2.polygons;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

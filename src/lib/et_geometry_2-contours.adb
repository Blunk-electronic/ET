------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      GEOMETRY 2 / CONTOURS                               --
--                                                                          --
--                               S p e c                                    --
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

--   For correct displaying set tab width in your edtior to 4.

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


	
package body et_geometry_2.contours is
	
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


	function to_contour (
		segments : in string)
		return type_contour'class
	is
		s_fields : constant type_fields_of_line := 
			read_line (line => segments, comment_mark => "#");

	begin
		return to_contour (s_fields);
	end to_contour;


	
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
					if element (c).segment_line.on_line (point) then
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

		-- CS exception message if contour consists of just a circle.
		return result;
	end get_segment;



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

		-- CS exception message if contour consists of just a circle.
		return result;
	end get_neigboring_segments;



	function get_shortest_distance (
		contour	: in type_contour;
		point	: in type_point)
		return type_float_internal_positive
	is
		--result : type_distance_polar := to_polar (type_float_internal'last, 0.0);
		result : type_float_internal_positive := type_float_internal'last;
		
		--procedure update (d : in type_distance_polar) is begin
			----put_line (to_string (d));
			--if get_absolute (d) < get_absolute (result) then
				--result := d;
			--end if;
		--end update;

		procedure update (d : in type_float_internal_positive) is begin
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
					--update (get_shortest_distance (point, s.segment_line));
					update (s.segment_line.get_shortest_distance (point));

				when ARC =>
					--put_line (to_string (s.segment_arc));
					--update (get_shortest_distance (point, s.segment_arc));
					update (get_absolute (s.segment_arc.get_shortest_distance (point)));
					
			end case;
		end query_segment;

		
	begin -- get_shortest_distance
		if contour.contour.circular then
			--result := get_shortest_distance (point, contour.contour.circle);
			result := get_absolute (contour.contour.circle.get_shortest_distance (point));
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



	procedure transpose_contour (
		contour	: in out type_contour'class;
		offset	: in type_distance)
	is 
		procedure move (point : in out type_point) is
			new_y : type_distance;
		begin
			new_y := offset - get_y (point);
			--point.set (Y, new_y);
			set (point, Y, new_y);
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
					--l.start_point := type_point (set (
							--x => to_distance (f (p + 1)),
							--y => to_distance (f (p + 2))));

					l.start_point := type_point (to_point (
							x => f (p + 1),
							y => f (p + 2)));

					
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
					--a.center := type_point (set (
							--x => to_distance (f (p + 1)),
							--y => to_distance (f (p + 2))));

					a.center := type_point (to_point (
							x => f (p + 1),
							y => f (p + 2)));

					
					-- assign start point of arc
					--a.start_point := type_point (set (
							--x => to_distance (f (p + 3)),
							--y => to_distance (f (p + 4))));

					a.start_point := type_point (to_point (
							x => f (p + 3),
							y => f (p + 4)));

					
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
					--c.center := type_point (set (
							--x => to_distance (f (p + 1)),
							--y => to_distance (f (p + 2))));

					c.center := type_point (to_point (
							x => f (p + 1),
							y => f (p + 2)));

					
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

		half_width : constant type_float_internal_positive := type_float_internal (line_width) * 0.5;
		

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

		
		-- Start point and end point of contour outline must match:
		if last_end_point /= start_point then
			closed := false;
			append (gaps, last_end_point);
		end if;
		
		-- Return the contour status:
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



	function get_lower_left_corner (
		contour	: in type_contour)
		return type_lower_left_corner
	is
		result : type_lower_left_corner;

		boundaries : constant type_boundaries := get_boundaries (contour, zero);
	begin
		-- compose the lower left corner point:
		--result.point := type_point (set (boundaries.smallest_x, boundaries.smallest_y));
		result.point := type_point (set (to_distance (boundaries.smallest_x),
										 to_distance (boundaries.smallest_y)));

		-- figure out whether the point is real or virtual:
		case get_point_to_contour_status (contour, result.point).location is
			when INSIDE =>
				result.status := REAL;
				
			when OUTSIDE | ON_EDGE | ON_VERTEX =>
				result.status := VIRTUAL;
		end case;
		
		return result;
	end get_lower_left_corner;


	function get_corner_nearest_to_origin (
		contour	: in type_contour)
		return type_point
	is
		use pac_points;
		corners : pac_points.list;
		
		use pac_contour_segments;
		
		procedure query_segment (c : in pac_contour_segments.cursor) is
			s : type_contour_segment renames element (c);
		begin
			case element (c).shape is
				when LINE => 
					corners.append (s.segment_line.start_point);
					corners.append (s.segment_line.end_point);
					
				when ARC =>
					corners.append (s.segment_arc.start_point);
					corners.append (s.segment_arc.end_point);
			end case;
		end query_segment;
		
	begin
		if contour.contour.circular then
			return contour.contour.circle.center;
		else
			contour.contour.segments.iterate (query_segment'access);
			return get_nearest (corners);
		end if;
		
	end get_corner_nearest_to_origin;
	
	
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
					--update (get_shortest_distance (point, s.segment_line));
					update (s.segment_line.get_shortest_distance (point));

				when ARC =>
					--put_line (to_string (s.segment_arc));
					--update (get_shortest_distance (point, s.segment_arc));
					update (s.segment_arc.get_shortest_distance (point));
					
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
					--& " is OUTSIDE of contour. ");

			--when INSIDE =>
				--result := to_unbounded_string ("Point" 
					--& to_string (i.start)
					--& " is INSIDE contour. ");

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
		-- intersects the contour edges are returned in a list.
		-- If a segment of the contour crosses the imaginary probe line,
		-- then it is regarded as intersection.
		-- NOTE: A line segment that runs exactly along the probe line
		-- is NOT regarded as "crossing" the probe line.
		
		-- The approach to detect whether the given point lies inside or outside 
		-- the contour area is as follows:
		-- 1. Build a probe line (starting at point) that runs at zero degrees
		--    to the right. The probe line divides the area in two: an upper half and a
		--    lower half. Special situations arise if objects start or end exactly at
		--    the probe line.
		-- 2. The number of intersections after the start point then tells us:
		--    - odd -> point is inside the contour area
		--    - zero or even -> point is outside the contour area

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
		-- means whether it is inside or outside the contour:
		it : count_type := 0;

		use pac_probe_line_intersections_contour;

		
		-- This procedure collects the intersection in the return value.
		procedure collect_intersection (
			intersection: in type_intersection; -- incl. point and angle
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
			-- and the candidate edge of the contour.
			i : constant type_intersection_of_two_lines := 
				l.get_intersection (probe_line);
			
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
			radius : constant type_float_internal_positive := get_radius_start (a_norm);
			
			-- Find out whether there is an intersection of the probe line
			-- and the candidate arc of the contour.
			i : constant type_intersection_of_line_and_circle := 
				a_norm.get_intersection (probe_line);

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
			-- and the candidate circle of the contour.
			i : constant type_intersection_of_line_and_circle := 
				c.get_intersection (probe_line);

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
		-- is inside the contour.
		-- If the total is even, then the point is outside the contour.
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
				
				-- Compute the distance of the given point to the contour.
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



	
end et_geometry_2.contours;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

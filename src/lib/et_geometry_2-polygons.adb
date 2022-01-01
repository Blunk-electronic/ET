------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              GEOMETRY 2                                  --
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
		polygon	: in type_polygon_base)
		return string
	is
		use ada.strings.unbounded;
		
		result : unbounded_string := to_unbounded_string ("polygon:");

		procedure query_segment (c : in pac_polygon_segments.cursor) is begin
			case element (c).shape is
				when LINE =>
					result := result & space & to_unbounded_string (to_string (element (c).segment_line));
					
				when ARC =>
					result := result & space & to_unbounded_string (to_string (element (c).segment_arc));
					
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

	

	procedure offset_polygon (
		polygon		: in out type_polygon_base;
		offset		: in type_distance) 
	is

		function offset_line (
			line : in type_line)
			return type_line_vector
		is
			line_new : type_line := line;
			center : type_point := get_center (line);
			line_direction : type_rotation := get_direction (line);
			dir_scratch : type_rotation;			
			test_point : type_point;
			tp_status : type_inside_polygon_query_result;

		begin
			dir_scratch := add (line_direction, +90.0);
			test_point := type_point (move (center, dir_scratch, type_distance'small));
			--put_line ("tp " & to_string (test_point));
			tp_status := in_polygon_status (polygon, test_point);

			if tp_status.status = INSIDE then
				--put_line ("inside");
				move_by (line_new, add (line_direction, -90.0), offset);
			else
				--put_line ("outside");
				move_by (line_new, add (line_direction, +90.0), offset);
			end if;

			return to_line_vector (line_new);
		end offset_line;
	

		package pac_line_vectors is new doubly_linked_lists (type_line_vector);
		use pac_line_vectors;
		line_vectors : pac_line_vectors.list;
		
		
		procedure do_segment (c : in pac_polygon_segments.cursor) is
			lv_tmp : type_line_vector;			
		begin
			case element (c).shape is
				
				when LINE =>
					lv_tmp := offset_line (element (c).segment_line);
					--put_line ("lv " & to_string (lv_tmp));
					line_vectors.append (lv_tmp);

				when ARC =>
					null; -- CS

			end case;
		end do_segment;


		polygon_segments_new : type_polygon_segments := (circular => false, others => <>);
		
		INIT, LS, LE : type_point;
		I : type_intersection_of_two_lines := (status => EXISTS, others => <>);

		
		procedure query_line (cp : in pac_line_vectors.cursor) is
			-- cp is the primary cursor that points to the current line.
			
			-- The secondary cursor that points to the line that is
			-- before the candidate line:
			cs : pac_line_vectors.cursor;

		begin
			--put_line ("lv " & to_string (element (cp)));

			if cp = line_vectors.first then
				cs := line_vectors.last;
				I := get_intersection (element (cp), element (cs));

				LS := to_point (I.intersection.vector);
				INIT := LS;

				
			else
				cs := previous (cp);
				I := get_intersection (element (cp), element (cs));

				LE := to_point (I.intersection.vector);

				-- line complete. append to new segments:
				polygon_segments_new.segments.append (
					(shape => LINE, segment_line => (LS, LE)));


				if cp = line_vectors.last then

					polygon_segments_new.segments.append (
						(shape => LINE, segment_line => (LE, INIT)));

				end if;

				
				-- The end point of this line will be the 
				-- start point of the next line (irrelevant for last line vector):
				LS := LE;
			end if;
		end query_line;

		
	begin -- offset_polygon

		if polygon.contours.circular then

			-- scale the single circle that forms the polygon:
			-- CS change radius of  polygon.contours.circle
			null;
		else
			polygon.contours.segments.iterate (do_segment'access);
		end if;

		-- Compute the intersections of the line_vectors.
		-- The intersections become the start and end points
		-- of the new line-segments:
		line_vectors.iterate (query_line'access);

		polygon.contours := polygon_segments_new;

		if not is_closed (polygon).closed then
			raise constraint_error with "Polygon NOT closed !";
		end if;
	end offset_polygon;

	
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
	

	

	
	
	
	
	function to_string (
		i : in type_inside_polygon_query_result)
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
		case i.status is
			when OUTSIDE =>
				result := to_unbounded_string ("Point" 
					& to_string (i.start) 
					& " is OUTSIDE of polygon. ");

			when INSIDE =>
				result := to_unbounded_string ("Point" 
					& to_string (i.start)
					& " is INSIDE polygon. ");
		end case;

		if is_empty (i.intersections) then
			result := result & "no intersections";
		else
			result := result & "intersection(s) x/angle:";
		end if;
		
		iterate (i.intersections, query_intersection'access);
		
		return to_string (result);
	end to_string;


	
	function in_polygon_status (
		polygon		: in type_polygon_base;	
		point		: in type_point)
		return type_inside_polygon_query_result 
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
		
		result : type_inside_polygon_query_result := (start => point, others => <>);

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
			intersection: in type_intersection; -- incl. point and angle
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
				
				append (result.intersections, (
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
			c : pac_probe_line_intersections.cursor;
		begin
			sort (result.intersections);

			-- for testing/verifying only:				
			--c := result.intersections.first;				
			--while c /= pac_probe_line_intersections.no_element loop
				--put_line (to_string (element (c).x_position));
				--next (c);
			--end loop;

			
			-- If x-positions differ by type_distance'small then we
			-- treat them as redundant.
			-- Remove redundant x-positions:
			c := result.intersections.first;
			while c /= pac_probe_line_intersections.no_element loop

				if c /= result.intersections.first then
					if abs (element (c).x_position - element (previous (c)).x_position)
						<= type_distance'small 
					then
						delete (result.intersections, c);
					end if;
				end if;
					
				next (c);
			end loop;

			-- for testing/verifying only:
			--c := result.intersections.first;				
			--while c /= pac_probe_line_intersections.no_element loop
				--put_line (to_string (element (c).x_position));
				--next (c);
			--end loop;

		end sort_x_values;

		
	begin -- in_polygon_status
		--put_line ("Y-threshold:" & to_string (y_threshold));
		
		if polygon.contours.circular then
			query_circle (polygon.contours.circle);
		else
			polygon.contours.segments.iterate (query_segment'access);
		end if;

		
		-- The x-values are not sorted yet. We need them sorted with the
		-- smallest x first and redundant x-positions removed:
		sort_x_values;

		-- get the total number of intersections
		it := pac_probe_line_intersections.length (result.intersections);
		--put_line ("intersections total:" & count_type'image (it));
		
		-- If the total number of intersections is an odd number, then the given point
		-- is inside the polygon.
		-- If the total is even, then the point is outside the polygon.
		if (it rem 2) = 1 then
			result.status := INSIDE;
			--put_line ("inside");
		else 
			result.status := OUTSIDE;
			--put_line ("outside");
		end if;

		return result;
	end in_polygon_status;

	

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
		case in_polygon_status (polygon, result.point).status is
			when INSIDE =>
				result.status := REAL;
				
			when OUTSIDE => -- or on edge of polygon
				result.status := VIRTUAL;
		end case;
		
		return result;
	end get_lower_left_corner;
	

end et_geometry_2.polygons;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

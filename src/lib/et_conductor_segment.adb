------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                         CONDUCTOR SEGMENT                                --
--                                                                          --
--                              B o d y                                     --
--                                                                          --
--         Copyright (C) 2017 - 2021 Mario Blunk, Blunk electronic          --
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
--   to do:



package body et_conductor_segment is

-- LINES
	
	function to_string (segment : in type_conductor_line_segment)
		return string
	is begin
		return ("line segment:" 
			& " edge left: " & to_string (segment.left_edge)
			& " / cap end: " & to_string (segment.cap_end)
			& " / edge right: " & to_string (segment.right_edge)
			& " / cap start: " & to_string (segment.cap_start));
	end to_string;


	function to_line_segment (line : in type_conductor_line)
		return type_conductor_line_segment
	is
		result : type_conductor_line_segment;
		direction : constant type_rotation := get_direction (line);
		distance : constant type_track_width := line.width * 0.5;		
	begin
		--log (text => "cond line" & to_string (line) & " width" & to_string (line.width));
		result.left_edge := type_line (line);
		move_by (result.left_edge, add (direction, +90.0), distance);
		--round (result.left_edge);

		result.right_edge := type_line (line);
		move_by (result.right_edge, add (direction, -90.0), distance);
		--round (result.right_edge);
		
		-- cap on the start of segment
		result.cap_start.center := line.start_point;
		result.cap_start.start_point := result.left_edge.start_point;
		result.cap_start.end_point := result.right_edge.start_point;
		result.cap_start.direction := CCW;
		--round (result.cap_start);
		
		-- cap on the end of the segment
		result.cap_end.center := line.end_point;
		result.cap_end.start_point := result.left_edge.end_point;
		result.cap_end.end_point := result.right_edge.end_point;
		result.cap_end.direction := CW;
		--round (result.cap_end);
		
		return result;
	end to_line_segment;



	
	function get_left_edge (segment : in type_conductor_line_segment)
		return type_line
	is begin
		return segment.left_edge;
	end get_left_edge;

	function get_right_edge (segment : in type_conductor_line_segment)
		return type_line
	is begin
		return segment.right_edge;
	end get_right_edge;
	
	function get_start_cap (segment : in type_conductor_line_segment)
		return type_arc
	is begin
		return segment.cap_start;
	end get_start_cap;

	function get_end_cap (segment : in type_conductor_line_segment)
		return type_arc
	is begin
		return segment.cap_end;
	end get_end_cap;	
	

	function get_shortest_distance (
		point	: in type_point;
		segment	: in type_conductor_line_segment)
		return type_distance
	is 
		result : type_distance := zero;

		type type_segment_area is new type_polygon_base with null record;
		polygon : type_segment_area;

		length_left_edge  : constant type_distance_positive := get_length (segment.left_edge);
		length_right_edge : constant type_distance_positive := get_length (segment.right_edge);

		procedure build_polygon is begin
			if length_left_edge = zero and length_right_edge = zero then
				-- rare case: the segment has no straight section between the
				-- start and end cap. It is basically a circle.
				declare
					s : type_polygon_segments := (circular => true, others => <>);
				begin
					s.circle.center := segment.cap_start.center;
					s.circle.radius := get_radius_start (segment.cap_start);
					polygon.contours := s;
				end;
			else
				-- the most common case: the segment has a straight section between
				-- its start and end cap:
				declare
					use pac_polygon_segments;
					s : type_polygon_segments := (circular => false, others => <>);
				begin
					append (s.segments, (LINE, segment.left_edge));
					append (s.segments, (ARC, segment.cap_end));
					append (s.segments, (LINE, type_line (reverse_line (segment.right_edge))));
					append (s.segments, (ARC, type_arc (reverse_arc (segment.cap_start))));
					polygon.contours := s;
				end;
			end if;			
		end build_polygon;

		distance : type_distance_polar;
	begin
		-- build a polygon from the given segment:
		build_polygon;

		--put_line (to_string (polygon));
		
		if not is_closed (polygon).closed then
			raise constraint_error with "contour of conductor segment not closed !";
		end if;

		
		distance := get_shortest_distance (polygon, point);

		--put_line ("p" & to_string (point));
		--put_line ("d" & to_string (get_absolute (distance)));

		case in_polygon_status (polygon, point).status is
			when INSIDE =>
				result := - get_absolute (distance);
				
			when OUTSIDE =>
				result := get_absolute (distance);
		end case;
		
		return result;
	end get_shortest_distance;


-- ARCS

	function to_string (segment : in type_conductor_arc_segment)
		return string
	is begin
		return ("arc segment:" 
			& " outer edge:" & to_string (segment.outer_edge)
			& " cap end" & to_string (segment.cap_end)
			& " inner edge:" & to_string (segment.inner_edge)
			& " cap start:" & to_string (segment.cap_start));
	end to_string;

	
	function to_arc_segment (arc : in type_conductor_arc)
		return type_conductor_arc_segment
	is
		arc_n : type_conductor_arc := arc;
		arc_i, arc_o : type_arc_angles;
		
		center_radius : constant type_distance_positive := get_radius_start (arc_n);
		half_width : constant type_distance_positive := arc_n.width * 0.5;
		inner_radius, outer_radius : type_distance_positive;
		result : type_conductor_arc_segment;		
	begin
		-- normalize given arc so that it runs CW:
		if arc_n.direction = CCW then
			reverse_arc (arc_n);
		end if;

		-- set radii
		inner_radius := center_radius - half_width;
		outer_radius := center_radius + half_width;


		-- set outer edge:
		arc_o := to_arc_angles (arc_n);
		arc_o.radius := type_float_internal (outer_radius);
		result.outer_edge := type_arc (to_arc (arc_o));

		-- set inner edge:
		arc_i := to_arc_angles (reverse_arc (arc_n));
		arc_i.radius := type_float_internal (inner_radius);
		result.inner_edge := type_arc (to_arc (arc_i));

		-- set start and end points of caps:
		-- cap at start point:
		result.cap_start.start_point := result.inner_edge.end_point;
		result.cap_start.end_point   := result.outer_edge.start_point;
		result.cap_start.direction := CW;
		result.cap_start.center := arc_n.start_point;
		
		-- cap at end point:
		result.cap_end.start_point := result.outer_edge.end_point;
		result.cap_end.end_point   := result.inner_edge.start_point;
		result.cap_end.direction := CW;
		result.cap_end.center := arc_n.end_point;

		return result;
	end to_arc_segment;

	
	function get_inner_edge (segment : in type_conductor_arc_segment)
		return type_arc
	is begin
		return segment.inner_edge;
	end get_inner_edge;

	function get_outer_edge (segment : in type_conductor_arc_segment)
		return type_arc
	is begin
		return segment.outer_edge;
	end get_outer_edge;

	function get_start_cap (segment : in type_conductor_arc_segment)
		return type_arc
	is begin
		return segment.cap_start;
	end get_start_cap;

	function get_end_cap (segment : in type_conductor_arc_segment)
		return type_arc
	is begin
		return segment.cap_end;
	end get_end_cap;

	
	function get_shortest_distance (
		point	: in type_point;
		segment	: in type_conductor_arc_segment)
		return type_distance
	is 
		result : type_distance := zero;

		type type_segment_area is new type_polygon_base with null record;
		polygon : type_segment_area;

		procedure build_polygon is 
			use pac_polygon_segments;
			s : type_polygon_segments := (circular => false, others => <>);
		begin
			append (s.segments, (ARC, segment.outer_edge));
			append (s.segments, (ARC, segment.cap_end));
			append (s.segments, (ARC, segment.inner_edge));
			append (s.segments, (ARC, segment.cap_start));
			polygon.contours := s;
		end build_polygon;

		distance : type_distance_polar;
	begin
		-- build a polygon from the given segment:
		build_polygon;

		distance := get_shortest_distance (polygon, point);

		case in_polygon_status (polygon, point).status is
			when INSIDE =>
				result := - get_absolute (distance);
				
			when OUTSIDE =>
				result := get_absolute (distance);
		end case;
		
		return result;
	end get_shortest_distance;
	

	
	
end et_conductor_segment;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

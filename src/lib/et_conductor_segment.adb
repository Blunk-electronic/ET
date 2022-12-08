------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                         CONDUCTOR SEGMENT                                --
--                                                                          --
--                              B o d y                                     --
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
--   to do:

with ada.strings;			use ada.strings;


package body et_conductor_segment is

	
	
-- LINES	
	
	function to_polygon (
		line 		: in type_conductor_line;
		tolerance	: in type_distance_positive)
		return type_polygon
	is begin
		return to_polygon (
			line		=> to_line_fine (line),
			linewidth	=> type_float_positive (line.width),
			tolerance	=> type_float_positive (tolerance),
			mode		=> EXPAND);

	end to_polygon;


	

	--function get_shortest_distance (
		--point	: in type_point;
		--segment	: in type_conductor_line_segment)
		--return type_distance
	--is 
		--result : type_distance := zero;

		--type type_segment_area is new type_polygon with null record;
		--polygon : type_segment_area;

		--length_left_edge  : constant type_distance_positive := get_length (segment.left_edge);
		--length_right_edge : constant type_distance_positive := get_length (segment.right_edge);

		--procedure build_polygon is begin
			--if length_left_edge = zero and length_right_edge = zero then
				---- rare case: the segment has no straight section between the
				---- start and end cap. It is basically a circle.
				--declare
					--s : type_polygon_segments := (circular => true, others => <>);
				--begin
					--s.circle.center := segment.cap_start.center;
					--s.circle.radius := get_radius_start (segment.cap_start);
					--polygon.contours := s;
				--end;
			--else
				---- the most common case: the segment has a straight section between
				---- its start and end cap:
				--declare
					--use pac_polygon_segments;
					--s : type_polygon_segments := (circular => false, others => <>);
				--begin
					--append (s.segments, (LINE, segment.left_edge));
					--append (s.segments, (ARC, segment.cap_end));
					--append (s.segments, (LINE, type_line (reverse_line (segment.right_edge))));
					--append (s.segments, (ARC, type_arc (reverse_arc (segment.cap_start))));
					--polygon.contours := s;
				--end;
			--end if;			
		--end build_polygon;

		----distance : type_distance_polar;
		----ipq : type_point_to_polygon_status;
	--begin
		---- build a polygon from the given segment:
		--build_polygon;

		----put_line (to_string (polygon));
		
		--if not is_closed (polygon).closed then
			--raise constraint_error with "contour of conductor segment not closed !";
		--end if;

		
		----distance := get_shortest_distance (polygon, point);
		--declare
			--ipq : constant type_point_to_polygon_status :=
				--get_point_to_polygon_status (polygon, to_vector (point));

		--begin
			----put_line ("p" & to_string (point));
			----put_line ("d" & to_string (get_absolute (distance)));

			--case ipq.location is
				--when INSIDE =>
					--result := - to_distance (ipq.distance);
					
				--when OUTSIDE | ON_EDGE | ON_VERTEX =>
					--result := + to_distance (ipq.distance);

			--end case;
		--end;
		
		--return result;
	--end get_shortest_distance;


	procedure line_conductor_properties (
		face			: in type_face;
		cursor			: in pac_conductor_lines.cursor;
		log_threshold 	: in type_log_level) 
	is
		line : type_conductor_line;
	begin
		line := element (cursor);
		log (text => "conductor line face" & to_string (face) & space 
			 & to_string (line)
			 & " width" & to_string (line.width), level => log_threshold);
	end line_conductor_properties;


	
-- ARCS

	
	function to_polygon (
		arc 		: in type_conductor_arc;
		tolerance	: in type_distance_positive)							
		return type_polygon
	is begin
		return to_polygon (
			arc			=> to_arc_fine (arc),
			linewidth	=> type_float_positive (arc.width),
			tolerance	=> type_float_positive (tolerance),
			mode		=> EXPAND);

	end to_polygon;

	
	
	--function get_shortest_distance (
		--point	: in type_point;
		--segment	: in type_conductor_arc_segment)
		--return type_distance
	--is 
		--result : type_distance := zero;

		--type type_segment_area is new type_polygon_base with null record;
		--polygon : type_segment_area;

		--procedure build_polygon is 
			--use pac_polygon_segments;
			--s : type_polygon_segments := (circular => false, others => <>);
		--begin
			--append (s.segments, (ARC, segment.outer_edge));
			--append (s.segments, (ARC, segment.cap_end));
			--append (s.segments, (ARC, segment.inner_edge));
			--append (s.segments, (ARC, segment.cap_start));
			--polygon.contours := s;
		--end build_polygon;

		----distance : type_distance_polar;
		----ipq : type_point_to_polygon_status;
	--begin
		---- build a polygon from the given segment:
		--build_polygon;

		----distance := get_shortest_distance (polygon, point);
		
		--declare
			--ipq : constant type_point_to_polygon_status :=
				--get_point_to_polygon_status (polygon, to_vector (point));
		--begin
			--case ipq.location is
				--when INSIDE =>
					--result := - to_distance (ipq.distance);
					
				--when OUTSIDE | ON_EDGE | ON_VERTEX =>
					--result := + to_distance (ipq.distance);
			--end case;
		--end;
		
		--return result;
	--end get_shortest_distance;
	

	procedure arc_conductor_properties (
		face			: in type_face;
		cursor			: in pac_conductor_arcs.cursor;
		log_threshold 	: in type_log_level) 
	is
		arc : type_conductor_arc;
	begin
		arc := element (cursor);
		log (text => "conductor arc face" & to_string (face) & space 
			 & to_string (arc)
			 & " width" & to_string (arc.width), level => log_threshold);
	end arc_conductor_properties;

	

	procedure circle_conductor_properties (
		face			: in type_face;
		cursor			: in pac_conductor_circles.cursor;
		log_threshold 	: in type_log_level) 
	is begin
		log (text => "conductor circle face" & to_string (face) & space 
			& to_string (type_circle (element (cursor)))
			& " width" & to_string (element (cursor).width),
			level => log_threshold);

	end circle_conductor_properties;

	
end et_conductor_segment;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

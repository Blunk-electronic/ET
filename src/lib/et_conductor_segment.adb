------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                         CONDUCTOR SEGMENT                                --
--                                                                          --
--                              B o d y                                     --
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

with ada.text_io;				use ada.text_io;
with ada.strings;				use ada.strings;
with et_contour_to_polygon;

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



	procedure iterate (
		lines	: in pac_conductor_lines.list;
		process	: not null access procedure (position : in pac_conductor_lines.cursor);
		proceed	: not null access boolean)
	is
		c : pac_conductor_lines.cursor := lines.first;
	begin
		while c /= pac_conductor_lines.no_element and proceed.all = TRUE loop
			process (c);
			next (c);
		end loop;
	end iterate;


	

	--function get_shortest_distance (
		--point	: in type_point;
		--segment	: in type_conductor_line_segment)
		--return type_distance_model
	--is 
		--result : type_distance_model := zero;

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


	procedure mirror_lines (
		lines	: in out pac_conductor_lines.list;
		axis	: in type_mirror := MIRROR_ALONG_Y_AXIS)
	is
		result : pac_conductor_lines.list;

		procedure query_line (c : in pac_conductor_lines.cursor) is
			line : type_conductor_line := element (c);
		begin
			mirror_line (line, MIRROR_ALONG_Y_AXIS);
			result.append (line);
		end;
		
	begin
		lines.iterate (query_line'access);
		lines := result;
	end mirror_lines;


	procedure rotate_lines (
		lines	: in out pac_conductor_lines.list;
		angle	: in type_rotation_model)
	is
		result : pac_conductor_lines.list;

		procedure query_line (c : in pac_conductor_lines.cursor) is
			line : type_conductor_line := element (c);
		begin
			rotate_line_by (line, angle);
			result.append (line);
		end;
		
	begin
		lines.iterate (query_line'access);
		lines := result;
	end rotate_lines;


	procedure move_lines (
		lines	: in out pac_conductor_lines.list;
		offset	: in type_vector_model)
	is
		result : pac_conductor_lines.list;

		procedure query_line (c : in pac_conductor_lines.cursor) is
			line : type_conductor_line := element (c);
		begin
			move_by (line, offset);
			result.append (line);
		end;
		
	begin
		lines.iterate (query_line'access);
		lines := result;
	end move_lines;


	function to_polygons (
		lines		: in pac_conductor_lines.list;
		tolerance	: in type_distance_positive)
		return pac_polygon_list.list
	is
		result : pac_polygon_list.list;

		procedure query_line (c : in pac_conductor_lines.cursor) is begin
			result.append (to_polygon (element (c), tolerance));
		end query_line;
		
	begin
		lines.iterate (query_line'access);
		return result;
	end to_polygons;

	
	
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
	

	
	procedure iterate (
		arcs	: in pac_conductor_arcs.list;
		process	: not null access procedure (position : in pac_conductor_arcs.cursor);
		proceed	: not null access boolean)
	is
		c : pac_conductor_arcs.cursor := arcs.first;
	begin
		while c /= pac_conductor_arcs.no_element and proceed.all = TRUE loop
			process (c);
			next (c);
		end loop;
	end iterate;

	
	--function get_shortest_distance (
		--point	: in type_point;
		--segment	: in type_conductor_arc_segment)
		--return type_distance_model
	--is 
		--result : type_distance_model := zero;

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
	

	procedure mirror_arcs (
		arcs	: in out pac_conductor_arcs.list;
		axis	: in type_mirror := MIRROR_ALONG_Y_AXIS)
	is
		result : pac_conductor_arcs.list;

		procedure query_arc (c : in pac_conductor_arcs.cursor) is
			arc : type_conductor_arc := element (c);
		begin
			mirror_arc (arc, MIRROR_ALONG_Y_AXIS);
			result.append (arc);
		end;
		
	begin
		arcs.iterate (query_arc'access);
		arcs := result;
	end mirror_arcs;


	procedure rotate_arcs (
		arcs	: in out pac_conductor_arcs.list;
		angle	: in type_rotation_model)
	is
		result : pac_conductor_arcs.list;

		procedure query_arc (c : in pac_conductor_arcs.cursor) is
			arc : type_conductor_arc := element (c);
		begin
			rotate_by (arc, angle);
			result.append (arc);
		end;
		
	begin
		arcs.iterate (query_arc'access);
		arcs := result;
	end rotate_arcs;


	procedure move_arcs (
		arcs	: in out pac_conductor_arcs.list;
		offset	: in type_vector_model)
	is
		result : pac_conductor_arcs.list;

		procedure query_arc (c : in pac_conductor_arcs.cursor) is
			arc : type_conductor_arc := element (c);
		begin
			move_by (arc, offset);
			result.append (arc);
		end;
		
	begin
		arcs.iterate (query_arc'access);
		arcs := result;
	end move_arcs;


	function to_polygons (
		arcs		: in pac_conductor_arcs.list;
		tolerance	: in type_distance_positive)
		return pac_polygon_list.list
	is
		result : pac_polygon_list.list;

		procedure query_arc (c : in pac_conductor_arcs.cursor) is begin
			result.append (to_polygon (element (c), tolerance));
		end query_arc;
		
	begin
		arcs.iterate (query_arc'access);
		return result;
	end to_polygons;


	
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



	function to_polygon_outside (
		circle 		: in type_conductor_circle;
		tolerance	: in type_distance_positive)							
		return type_polygon
	is 
		use et_contour_to_polygon;
		result : type_polygon;

		-- outer_radius : constant type_float_positive := 
		-- 	circle.radius + 0.5 * type_float_positive (circle.width);

		outer_radius : constant type_distance_positive := 
			get_radius (circle) + 0.5 * circle.width;
	begin
		result.edges := to_edges (
			circle		=> type_circle (to_circle (get_center (circle), outer_radius)),
			tolerance	=> tolerance,
			mode		=> EXPAND);

		optimize_edges (result); -- MANDATORY !!
		return result;
	end to_polygon_outside;


	
	function to_polygon_inside (
		circle 		: in type_conductor_circle;
		tolerance	: in type_distance_positive)							
		return type_polygon
	is 
		use et_contour_to_polygon;
		result : type_polygon;

		-- inner_radius : constant type_float_positive :=
		-- 	circle.radius - 0.5 * type_float_positive (circle.width);

		inner_radius : constant type_distance_positive :=
			get_radius (circle) - 0.5 * circle.width;

	begin
		result.edges := to_edges (
			circle		=> type_circle (to_circle (get_center (circle), inner_radius)),
			tolerance	=> tolerance,
			mode		=> SHRINK);

		optimize_edges (result); -- MANDATORY !!
		return result;
	end to_polygon_inside;



	procedure iterate (
		circles	: in pac_conductor_circles.list;
		process	: not null access procedure (position : in pac_conductor_circles.cursor);
		proceed	: not null access boolean)
	is
		c : pac_conductor_circles.cursor := circles.first;
	begin
		while c /= pac_conductor_circles.no_element and proceed.all = TRUE loop
			process (c);
			next (c);
		end loop;
	end iterate;


	
	procedure mirror_circles (
		circles	: in out pac_conductor_circles.list;
		axis	: in type_mirror := MIRROR_ALONG_Y_AXIS)
	is
		result : pac_conductor_circles.list;

		procedure query_circle (c : in pac_conductor_circles.cursor) is
			circle : type_conductor_circle := element (c);
		begin
			mirror_circle (circle, MIRROR_ALONG_Y_AXIS);
			result.append (circle);
		end;
		
	begin
		circles.iterate (query_circle'access);
		circles := result;
	end mirror_circles;


	
	procedure rotate_circles (
		circles	: in out pac_conductor_circles.list;
		angle	: in type_rotation_model)
	is
		result : pac_conductor_circles.list;

		procedure query_circle (c : in pac_conductor_circles.cursor) is
			circle : type_conductor_circle := element (c);
		begin
			rotate_circle_by (circle, angle);
			result.append (circle);
		end;
		
	begin
		circles.iterate (query_circle'access);
		circles := result;
	end rotate_circles;


	procedure move_circles (
		circles	: in out pac_conductor_circles.list;
		offset	: in type_vector_model)
	is
		result : pac_conductor_circles.list;

		procedure query_circle (c : in pac_conductor_circles.cursor) is
			circle : type_conductor_circle := element (c);
		begin
			move_by (circle, offset);
			result.append (circle);
		end;
		
	begin
		circles.iterate (query_circle'access);
		circles := result;
	end move_circles;


	function to_polygons_outside (
		circles		: in pac_conductor_circles.list;
		tolerance	: in type_distance_positive)
		return pac_polygon_list.list
	is
		result : pac_polygon_list.list;

		procedure query_circle (c : in pac_conductor_circles.cursor) is begin
			result.append (to_polygon_outside (element (c), tolerance));
		end query_circle;
		
	begin
		circles.iterate (query_circle'access);
		return result;
	end to_polygons_outside;


	function to_polygons_inside (
		circles		: in pac_conductor_circles.list;
		tolerance	: in type_distance_positive)
		return pac_polygon_list.list
	is
		result : pac_polygon_list.list;

		procedure query_circle (c : in pac_conductor_circles.cursor) is begin
			result.append (to_polygon_inside (element (c), tolerance));
		end query_circle;
		
	begin
		circles.iterate (query_circle'access);
		return result;
	end to_polygons_inside;

	

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

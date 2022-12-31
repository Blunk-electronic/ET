------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                            VIA RESTRICT                                  --
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


package body et_via_restrict is



-- LINES

	function to_polygon (
		line 		: in type_via_restrict_line;
		tolerance	: in type_distance_positive)
		return type_polygon
	is begin
		return to_polygon (
			line		=> to_line_fine (line),
			linewidth	=> type_float_positive (line.width),
			tolerance	=> type_float_positive (tolerance),
			mode		=> EXPAND);

	end to_polygon;


	procedure mirror_lines (
		lines	: in out pac_via_restrict_lines.list;
		axis	: in type_axis_2d := Y)
	is
		result : pac_via_restrict_lines.list;

		procedure query_line (c : in pac_via_restrict_lines.cursor) is
			line : type_via_restrict_line := element (c);
		begin
			mirror (line, Y);
			result.append (line);
		end;
		
	begin
		lines.iterate (query_line'access);
		lines := result;
	end mirror_lines;


	procedure rotate_lines (
		lines	: in out pac_via_restrict_lines.list;
		angle	: in type_rotation)
	is
		result : pac_via_restrict_lines.list;

		procedure query_line (c : in pac_via_restrict_lines.cursor) is
			line : type_via_restrict_line := element (c);
		begin
			rotate_by (line, angle);
			result.append (line);
		end;
		
	begin
		lines.iterate (query_line'access);
		lines := result;
	end rotate_lines;


	procedure move_lines (
		lines	: in out pac_via_restrict_lines.list;
		offset	: in type_distance_relative)
	is
		result : pac_via_restrict_lines.list;

		procedure query_line (c : in pac_via_restrict_lines.cursor) is
			line : type_via_restrict_line := element (c);
		begin
			move_by (line, offset);
			result.append (line);
		end;
		
	begin
		lines.iterate (query_line'access);
		lines := result;
	end move_lines;


	function to_polygons (
		lines		: in pac_via_restrict_lines.list;
		tolerance	: in type_distance_positive)
		return pac_polygon_list.list
	is
		result : pac_polygon_list.list;

		procedure query_line (c : in pac_via_restrict_lines.cursor) is begin
			result.append (to_polygon (element (c), tolerance));
		end query_line;
		
	begin
		lines.iterate (query_line'access);
		return result;
	end to_polygons;


-- ARCS

	function to_polygon (
		arc 		: in type_via_restrict_arc;
		tolerance	: in type_distance_positive)							
		return type_polygon
	is begin
		return to_polygon (
			arc			=> to_arc_fine (arc),
			linewidth	=> type_float_positive (arc.width),
			tolerance	=> type_float_positive (tolerance),
			mode		=> EXPAND);

	end to_polygon;
	


	
	procedure mirror_arcs (
		arcs	: in out pac_via_restrict_arcs.list;
		axis	: in type_axis_2d := Y)
	is
		result : pac_via_restrict_arcs.list;

		procedure query_arc (c : in pac_via_restrict_arcs.cursor) is
			arc : type_via_restrict_arc := element (c);
		begin
			mirror (arc, Y);
			result.append (arc);
		end;
		
	begin
		arcs.iterate (query_arc'access);
		arcs := result;
	end mirror_arcs;


	procedure rotate_arcs (
		arcs	: in out pac_via_restrict_arcs.list;
		angle	: in type_rotation)
	is
		result : pac_via_restrict_arcs.list;

		procedure query_arc (c : in pac_via_restrict_arcs.cursor) is
			arc : type_via_restrict_arc := element (c);
		begin
			rotate_by (arc, angle);
			result.append (arc);
		end;
		
	begin
		arcs.iterate (query_arc'access);
		arcs := result;
	end rotate_arcs;


	procedure move_arcs (
		arcs	: in out pac_via_restrict_arcs.list;
		offset	: in type_distance_relative)
	is
		result : pac_via_restrict_arcs.list;

		procedure query_arc (c : in pac_via_restrict_arcs.cursor) is
			arc : type_via_restrict_arc := element (c);
		begin
			move_by (arc, offset);
			result.append (arc);
		end;
		
	begin
		arcs.iterate (query_arc'access);
		arcs := result;
	end move_arcs;



	function to_polygons (
		arcs		: in pac_via_restrict_arcs.list;
		tolerance	: in type_distance_positive)
		return pac_polygon_list.list
	is
		result : pac_polygon_list.list;

		procedure query_arc (c : in pac_via_restrict_arcs.cursor) is begin
			result.append (to_polygon (element (c), tolerance));
		end query_arc;
		
	begin
		arcs.iterate (query_arc'access);
		return result;
	end to_polygons;



-- CIRCLES

	function to_polygon_outside (
		circle 		: in type_via_restrict_circle;
		tolerance	: in type_distance_positive)							
		return type_polygon
	is 
		use et_contour_to_polygon;
		result : type_polygon;
		outer_radius : constant type_float_positive := 
			circle.radius + 0.5 * type_float_positive (circle.width);
	begin
		result.edges := to_edges (
			circle		=> (circle.center, outer_radius),
			tolerance	=> tolerance,
			mode		=> EXPAND);

		optimize_edges (result); -- MANDATORY !!
		return result;
	end to_polygon_outside;


	function to_polygon_inside (
		circle 		: in type_via_restrict_circle;
		tolerance	: in type_distance_positive)							
		return type_polygon
	is 
		use et_contour_to_polygon;
		result : type_polygon;
		inner_radius : constant type_float_positive :=
			circle.radius - 0.5 * type_float_positive (circle.width);
	begin
		result.edges := to_edges (
			circle		=> (circle.center, inner_radius),
			tolerance	=> tolerance,
			mode		=> SHRINK);

		optimize_edges (result); -- MANDATORY !!
		return result;
	end to_polygon_inside;

	
	
	procedure mirror_circles (
		circles	: in out pac_via_restrict_circles.list;
		axis	: in type_axis_2d := Y)
	is
		result : pac_via_restrict_circles.list;

		procedure query_circle (c : in pac_via_restrict_circles.cursor) is
			circle : type_via_restrict_circle := element (c);
		begin
			mirror (circle, Y);
			result.append (circle);
		end;
		
	begin
		circles.iterate (query_circle'access);
		circles := result;
	end mirror_circles;


	procedure rotate_circles (
		circles	: in out pac_via_restrict_circles.list;
		angle	: in type_rotation)
	is
		result : pac_via_restrict_circles.list;

		procedure query_circle (c : in pac_via_restrict_circles.cursor) is
			circle : type_via_restrict_circle := element (c);
		begin
			rotate_by (circle, angle);
			result.append (circle);
		end;
		
	begin
		circles.iterate (query_circle'access);
		circles := result;
	end rotate_circles;


	procedure move_circles (
		circles	: in out pac_via_restrict_circles.list;
		offset	: in type_distance_relative)
	is
		result : pac_via_restrict_circles.list;

		procedure query_circle (c : in pac_via_restrict_circles.cursor) is
			circle : type_via_restrict_circle := element (c);
		begin
			move_by (circle, offset);
			result.append (circle);
		end;
		
	begin
		circles.iterate (query_circle'access);
		circles := result;
	end move_circles;



	function to_polygons_outside (
		circles		: in pac_via_restrict_circles.list;
		tolerance	: in type_distance_positive)
		return pac_polygon_list.list
	is
		result : pac_polygon_list.list;

		procedure query_circle (c : in pac_via_restrict_circles.cursor) is begin
			result.append (to_polygon_outside (element (c), tolerance));
		end query_circle;
		
	begin
		circles.iterate (query_circle'access);
		return result;
	end to_polygons_outside;


	function to_polygons_inside (
		circles		: in pac_via_restrict_circles.list;
		tolerance	: in type_distance_positive)
		return pac_polygon_list.list
	is
		result : pac_polygon_list.list;

		procedure query_circle (c : in pac_via_restrict_circles.cursor) is begin
			result.append (to_polygon_inside (element (c), tolerance));
		end query_circle;
		
	begin
		circles.iterate (query_circle'access);
		return result;
	end to_polygons_inside;


	
	procedure line_via_restrict_properties (
		face			: in type_face;
		cursor			: in pac_via_restrict_lines.cursor;
		log_threshold 	: in type_log_level) 
	is
		use pac_via_restrict_lines;
		line : type_via_restrict_line;
	begin
		line := element (cursor);
		log (text => "via restrict line face" & to_string (face) & space
			 & to_string (line), level => log_threshold);
	end line_via_restrict_properties;

	
	procedure arc_via_restrict_properties (
		face			: in type_face;
		cursor			: in pac_via_restrict_arcs.cursor;
		log_threshold 	: in type_log_level)
	is
		use pac_via_restrict_arcs;
		arc : type_via_restrict_arc;
	begin
		arc := element (cursor);
		log (text => "via restrict arc face" & to_string (face) & space 
			 & to_string (arc), level => log_threshold);
	end arc_via_restrict_properties;

	
	-- CS procedure circle_via_restrict_properties


	
end et_via_restrict;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

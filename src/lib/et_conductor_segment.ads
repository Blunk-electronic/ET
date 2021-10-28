------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                         CONDUCTOR SEGMENT                                --
--                                                                          --
--                              S p e c                                     --
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

with ada.containers; 			use ada.containers;

with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_doubly_linked_lists;

with et_pcb_coordinates;		use et_pcb_coordinates;
with et_geometry;				use et_geometry;
with et_board_shapes_and_text;	use et_board_shapes_and_text;
with et_design_rules;			use et_design_rules;
with et_string_processing;		use et_string_processing;

package et_conductor_segment is
	use pac_geometry_brd;

	use pac_geometry_2;
	use pac_polygons;
	use pac_text_fab;

	
-- LINES
	type type_conductor_line is new type_line with record
		width	: type_track_width;
	end record;

	
	type type_conductor_line_segment is private;

	function to_string (segment : in type_conductor_line_segment)
		return string;

	function to_line_segment (line : in type_conductor_line)
		return type_conductor_line_segment;

	
	function get_left_edge (segment : in type_conductor_line_segment)
		return type_line;

	function get_right_edge (segment : in type_conductor_line_segment)
		return type_line;

	function get_start_cap (segment : in type_conductor_line_segment)
		return type_arc;

	function get_end_cap (segment : in type_conductor_line_segment)
		return type_arc;

	-- Computes the shortest distance from a point to
	-- a conductor line segment. If the return is negative,
	-- then the point is inside the segment.
	-- If the segment contour is not closed, then an exception
	-- is raised:
	function get_shortest_distance (
		point	: in type_point;
		segment	: in type_conductor_line_segment)
		return type_distance;
	

	package pac_conductor_lines is new doubly_linked_lists (type_conductor_line);
	use pac_conductor_lines;
	
	-- Logs the properties of the given line:
	procedure line_conductor_properties (
		face			: in type_face;
		cursor			: in pac_conductor_lines.cursor;
		log_threshold 	: in type_log_level);


	

-- ARCS
	type type_conductor_arc is new type_arc with record
		width	: type_track_width;
	end record;

	
	type type_conductor_arc_segment is private;

	function to_string (segment : in type_conductor_arc_segment)
		return string;
	
	function to_arc_segment (arc : in type_conductor_arc)
		return type_conductor_arc_segment;

	function get_inner_edge (segment : in type_conductor_arc_segment)
		return type_arc;

	function get_outer_edge (segment : in type_conductor_arc_segment)
		return type_arc;

	function get_start_cap (segment : in type_conductor_arc_segment)
		return type_arc;

	function get_end_cap (segment : in type_conductor_arc_segment)
		return type_arc;


	-- Computes the shortest distance from a point to
	-- a conductor arc segment. If the return is negative,
	-- then the point is inside the segment:
	function get_shortest_distance (
		point	: in type_point;
		segment	: in type_conductor_arc_segment)
		return type_distance;

	
	package pac_conductor_arcs is new doubly_linked_lists (type_conductor_arc);
	use pac_conductor_arcs;
	
	-- Logs the properties of the given arc:
	procedure arc_conductor_properties (
		face			: in type_face;
		cursor			: in pac_conductor_arcs.cursor;
		log_threshold 	: in type_log_level);



	
-- CIRCLES
	type type_conductor_circle (
		filled		: type_filled;
		fill_style	: type_fill_style) -- don't care if filled is NO
		is new type_circle 
	with record
		case filled is
			when NO => 
				-- the line width of the circumfence:
				border_width : type_track_width := type_track_width'first;

			when YES =>
				case fill_style is
					when SOLID => null;
					when HATCHED =>
						hatching : type_conductor_hatching;
				end case;
				
		end case;
	end record;

	package pac_conductor_circles is new indefinite_doubly_linked_lists (type_conductor_circle);
	use pac_conductor_circles;
	
	-- Logs the properties of the given circle:
	procedure circle_conductor_properties (
		face			: in type_face;
		cursor			: in pac_conductor_circles.cursor;
		log_threshold 	: in type_log_level);


	
	
private

	type type_conductor_line_segment is record
		left_edge, right_edge : type_line;
		cap_start, cap_end : type_arc;
	end record;
	
	type type_conductor_arc_segment is record
		inner_edge, outer_edge : type_arc;
		cap_start, cap_end : type_arc;
	end record;

	
end et_conductor_segment;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

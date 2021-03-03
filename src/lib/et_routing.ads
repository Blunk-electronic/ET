------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                               ROUTING                                    --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2021 Mario Blunk, Blunk electronic          --
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
--                                                                          --
------------------------------------------------------------------------------

--   For correct displaying set tab width in your editor to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
-- DESCRIPTION:
-- 

with et_design_rules;			use et_design_rules;
with et_nets;					use et_nets;
with et_vias;					use et_vias;
with et_terminals;				use et_terminals;
with et_packages;				use et_packages;
with et_pcb;					use et_pcb;
with et_pcb_stack;				use et_pcb_stack;
with et_pcb_coordinates;		use et_pcb_coordinates;
with et_board_shapes_and_text;
with et_string_processing;		use et_string_processing;
with et_project.modules;		use et_project.modules;
with et_conductor_polygons;		use et_conductor_polygons;

package et_routing is
	
	use et_pcb_coordinates.pac_geometry_brd;
	use et_board_shapes_and_text.pac_shapes;


	
	function compute_fill_lines (
		module_cursor	: in pac_generic_modules.cursor;

		-- The points of intersection with the board contours:
		board_points	: in type_inside_polygon_query_result;

		-- The points of intersection with the polygon contours:
		polygon_points	: in type_inside_polygon_query_result;

		-- The clearance of the net where the polygon is
		-- connected with:
		clearance		: in type_track_clearance;

		-- The isolation of the polygon:
		isolation 		: in type_track_clearance; 

		-- The easing of the polygon:
		easing			: in type_easing
		
		-- CS x-intersections with tracks, pads, texts, ...
		)
		return pac_fill_lines.list;


	
	type type_track_observe_clearance is (
		RIGHT,
		LEFT,
		BOTH);

	track_observe_clearance_default : constant type_track_observe_clearance := BOTH;

	-- Returns the distance from start_point to the next obstacle
	-- in the given direction:
	function get_distance_to_obstacle (
		module_cursor	: in pac_generic_modules.cursor;
		start_point		: in type_point;
		direction		: in type_rotation;
		net_name		: in pac_net_name.bounded_string := no_name;
		layer			: in type_signal_layer;
		width			: in type_track_width;
		clearance		: in type_track_observe_clearance := track_observe_clearance_default;
		log_threshold	: in type_log_level)
		return type_distance;

	-- Calculates the distance in direction 0 degree from given
	-- start_point to the next obstacle.
	-- Use this function for filling conductor polygons.
	-- Objects that are regarded as obstacles are: 
	-- - pads (SMT and THT)
	-- - tracks
	-- - board outlines
	-- - route restrict lines, arcs, circles, polygons
	-- - cutout areas
	-- - the contour/edge of the conductor polygon
	-- The clearances as specified in DRU are also taken into account.
	function get_distance_to_obstacle_in_polygon (
		module_cursor	: in pac_generic_modules.cursor;
		polygon			: in type_polygon_conductor'class;
		
		-- The start point must be on the edge or inside the polygon:
		start_point		: in type_point;

		-- The net name is relevant if the polygon is connected
		-- with a net (depends on the type of the given polygon):
		net_name		: in pac_net_name.bounded_string := no_name;
		
		clearance		: in type_track_observe_clearance := track_observe_clearance_default;
		log_threshold	: in type_log_level)
		return type_distance_positive;

	
	-- CS not final. just an approach:
	function get_start_point_beyond_obstacle (
		module_cursor	: in pac_generic_modules.cursor;
		end_point		: in type_point;
		direction		: in type_rotation;
		--net_name		: in pac_net_name.bounded_string := no_name;
		layer			: in type_signal_layer;
		width			: in type_track_width;
		clearance		: in type_track_observe_clearance := track_observe_clearance_default;
		log_threshold	: in type_log_level)
		return type_point;


	
end et_routing;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

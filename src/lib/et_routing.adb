------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                               ROUTING                                    --
--                                                                          --
--                               B o d y                                    --
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

with ada.tags;					use ada.tags;

with et_vias;					use et_vias;
with et_terminals;				use et_terminals;
with et_packages;				use et_packages;
with et_pcb;					use et_pcb;
with et_pcb_stack;				use et_pcb_stack;
with et_pcb_coordinates;		use et_pcb_coordinates;
with et_board_shapes_and_text;


package body et_routing is
	
	use et_pcb_coordinates.pac_geometry_brd;
	use et_board_shapes_and_text.pac_shapes;

	function compute_fill_lines (
		module_cursor	: in pac_generic_modules.cursor;
		board			: in type_inside_polygon_query_result;
		polygon			: in type_inside_polygon_query_result)
		return pac_fill_lines.list
	is
		result : pac_fill_lines.list;
	begin

		return result;
	end compute_fill_lines;
	
	function get_distance_to_obstacle (
		module_cursor	: in pac_generic_modules.cursor;
		start_point		: in type_point;
		direction		: in type_rotation;
		net_name		: in pac_net_name.bounded_string := no_name;
		layer			: in type_signal_layer;
		width			: in type_track_width;
		clearance		: in type_track_observe_clearance := track_observe_clearance_default;
		log_threshold	: in type_log_level)
		return type_distance
	is
		distance : type_distance := zero;
	begin
		null;

		return distance;
	end get_distance_to_obstacle;
	
	function get_distance_to_obstacle_in_polygon (
		module_cursor	: in pac_generic_modules.cursor;
		polygon			: in type_polygon_conductor'class;
		start_point		: in type_point;
		net_name		: in pac_net_name.bounded_string := no_name;
		clearance		: in type_track_observe_clearance := track_observe_clearance_default;
		log_threshold	: in type_log_level)
		return type_distance_positive
	is
		use et_conductor_polygons;
		
		distance : type_distance_positive := zero;

		layer : type_signal_layer;
		width : type_track_width := polygon.width_min;

		procedure floating_solid is
			p : type_polygon_conductor_solid_floating := type_polygon_conductor_solid_floating (polygon);
		begin
			layer := p.properties.layer;
		end floating_solid;
		
		procedure floating_hatched is
			p : type_polygon_conductor_hatched_floating := type_polygon_conductor_hatched_floating (polygon);
		begin
			layer := p.properties.layer;
		end floating_hatched;
			
		procedure route_solid is
			p : type_polygon_conductor_route_solid := type_polygon_conductor_route_solid (polygon);
		begin
			layer := p.properties.layer;
		end route_solid;

		procedure route_hatched is
			p : type_polygon_conductor_route_hatched := type_polygon_conductor_route_hatched (polygon);
		begin
			layer := p.properties.layer;
		end route_hatched;

		
	begin -- get_distance_to_obstacle_in_polygon
		
		if polygon'tag = type_polygon_conductor_solid_floating'tag then
			floating_solid;

		elsif polygon'tag = type_polygon_conductor_hatched_floating'tag then
			floating_hatched;
			
		elsif polygon'tag = type_polygon_conductor_route_solid'tag then
			route_solid;

		elsif polygon'tag = type_polygon_conductor_route_hatched'tag then
			route_hatched;

		end if;

		distance := 3.0;
		--null;

		return distance;
	end get_distance_to_obstacle_in_polygon;

	
	function get_start_point_beyond_obstacle (
		module_cursor	: in pac_generic_modules.cursor;
		end_point		: in type_point;
		direction		: in type_rotation;
		--net_name		: in pac_net_name.bounded_string := no_name;
		layer			: in type_signal_layer;
		width			: in type_track_width;
		clearance		: in type_track_observe_clearance := track_observe_clearance_default;
		log_threshold	: in type_log_level)
		return type_point
	is
		point : type_point;
	begin
		null;

		return point;
	end get_start_point_beyond_obstacle;

	
end et_routing;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

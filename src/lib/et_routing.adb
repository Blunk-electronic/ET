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
		board_points	: in type_inside_polygon_query_result;
		polygon_points	: in type_inside_polygon_query_result;
		clearance		: in type_track_clearance;
		isolation 		: in type_track_clearance; 
		easing			: in type_easing;
		log_threshold	: in type_log_level
		)
		return pac_fill_lines.list
	is
		result : pac_fill_lines.list;

		type type_switch_point_status is (
			VALID,
			INVALID);
		
		type type_switch_point (status : type_switch_point_status) is record
			case status is
				when VALID =>
					x_value	: type_distance := zero;
					board	: boolean := false;
					polygon	: boolean := false;
					-- CS others (tracks, texts, ...)

				when INVALID => null;
			end case;
		end record;

		-- This position pointer advances to the right along the x-axis
		-- It starts where probe line of the board_points has started.
		-- NOTE: board_points and polygon_points (and other "points") have
		-- equal start points.
		x_position : type_distance := X (board_points.point); 

		-- or position : type_distance := X (polygon_points.point); 

		
		function get_next_switch_point (forward : in type_distance)
			return type_switch_point
		is
			use pac_distances;
			sp : type_switch_point (VALID);

			
			type type_nearest_points is array (1..2) of type_distance;
			np : type_nearest_points := (others => type_distance'last);
			
			xb, xp  : type_distance := type_distance'last;
			
			procedure query_board_point (c : in pac_distances.cursor) is
				d : constant type_distance := element (c) - x_position;
			begin
				if d < np(1) then
					np(1) := d;
				end if;
				
			end query_board_point;

			procedure query_polygon_point (c : in pac_distances.cursor) is
				d : constant type_distance := element (c) - x_position;
			begin
				if d < np(2) then
					np(2) := d;
				end if;
				
			end query_polygon_point;

			
			sp_x : type_distance := type_distance'last;
						
			procedure set_switch_point is
				d : type_distance;
			begin
				for i in type_nearest_points'first .. type_nearest_points'last loop
					d := np(i) - x_position;

					if d < sp_x then
						sp_x := d;
					end if;

				end loop;

				
			end set_switch_point;
			
		begin
			iterate (board_points.x_values, query_board_point'access);

			log (text => "nearest board point is" & to_string (np(1)),
				level => log_threshold);

			
			iterate (polygon_points.x_values, query_polygon_point'access);

			log (text => "nearest polygon point is" & to_string (np(2)),
				level => log_threshold);

			set_switch_point;
			
			log (text => "nearest is" & to_string (sp_x),
				level => log_threshold);

			
			return sp;
		end get_next_switch_point;

		
	begin -- compute_fill_lines
		log (text => "evaluating switch points after" & to_string (board_points.point),
			 level => log_threshold);

		declare
			s : type_switch_point := get_next_switch_point (forward => x_position);
		begin
			null;
		end;
		
		--while get_next_switch_point (forward => x_position).status = VALID loop

				--null;
		--end loop;
		
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

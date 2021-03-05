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

		type type_stop_go is (
			STOP,
			GO);					 

		board_line_status, polygon_line_status : type_stop_go := STOP;

		
		
		procedure toggle_status_board_line is begin
			case board_line_status is
				when STOP	=> board_line_status := GO;
				when GO		=> board_line_status := STOP;
			end case;
		end toggle_status_board_line;

		procedure toggle_status_polygon_line is begin
			case polygon_line_status is
				when STOP	=> polygon_line_status := GO;
				when GO		=> polygon_line_status := STOP;
			end case;
		end toggle_status_polygon_line;

		
		final_line_status : type_stop_go := STOP;
		
		procedure update_line_status is begin
			if board_line_status = GO and polygon_line_status = GO then
				final_line_status := GO;
			else
				final_line_status := STOP;
			end if;
		end update_line_status;

		
		
		type type_switch_point_status is (
			VALID,
			INVALID);

		type type_switches is record
			board	: boolean := false;
			polygon	: boolean := false;
			-- CS others (tracks, texts, ...)
		end record;
		
		type type_switch_point (status : type_switch_point_status) is record
			case status is
				when VALID =>
					x_value		: type_distance := zero;
					switches	: type_switches;

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

			status : type_switch_point_status := INVALID;
			switches : type_switches;
			x_value : type_distance;
			
			type type_smallest_differences is array (1..2) of type_distance;
			sdx : type_smallest_differences := (others => type_distance'last);

			procedure query_board_point (c : in pac_distances.cursor) is
				dx : constant type_distance := element (c) - forward;
			begin
				if dx < sdx(1) then
					sdx(1) := dx;
				end if;

				if dx > zero then
					status := VALID;
				end if;
				
			end query_board_point;

			procedure query_polygon_point (c : in pac_distances.cursor) is
				dx : constant type_distance := element (c) - forward;
			begin
				if dx < sdx(2) then
					sdx(2) := dx;
				end if;

				if dx > zero then
					status := VALID;
				end if;
				
			end query_polygon_point;
			
			procedure set_switch_point is
				dx : type_distance := type_distance'last;
			begin
				-- Find the smallest difference among the domains:
				for i in type_smallest_differences'first .. type_smallest_differences'last loop

					if sdx(i) < dx then
						dx := sdx(i);
					end if;

				end loop;


				-- Find the domains that have the current x_value.
				-- There can be more than one domain having the current x_value:
				for i in type_smallest_differences'first .. type_smallest_differences'last loop

					if sdx(i) = dx then
						case i is
							when 1 => switches.board := true;
								toggle_status_board_line;
							
							when 2 => switches.polygon := true;
								toggle_status_polygon_line;
						end case;
					end if;

				end loop;				

				-- The absolute position in x of the switch point is:
				x_value := forward + dx;
			end set_switch_point;
			
		begin -- get_next_switch_point

			-- Find the x-value that is nearest to the given forward-value:
			iterate (board_points.x_values, query_board_point'access);

			log (text => "nearest board point is" & to_string (sdx(1)),
				level => log_threshold);

			-- Find the x-value that is nearest to the given forward-value:			
			iterate (polygon_points.x_values, query_polygon_point'access);

			log (text => "nearest polygon point is" & to_string (sdx(2)),
				level => log_threshold);

			case status is
				when VALID => 
					set_switch_point;
				
					--log (text => "nearest is" & to_string (sp_x),
						--level => log_threshold);

					return (VALID, x_value, switches);
					
				when INVALID =>
					return (status => INVALID);
			end case;

		end get_next_switch_point;

		
	begin -- compute_fill_lines
		log (text => "evaluating switch points after" & to_string (board_points.point),
			 level => log_threshold);

		-- init line status:
		case board_points.status is
			when INSIDE		=> board_line_status := GO;
			when OUTSIDE	=> board_line_status := STOP;
		end case;

		case polygon_points.status is
			when INSIDE		=> polygon_line_status := GO;
			when OUTSIDE	=> polygon_line_status := STOP;
		end case;

		update_line_status;
		
		
		loop
			declare
				s : type_switch_point := get_next_switch_point (forward => x_position);
			begin
				null;
			end;

		end loop;
		
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

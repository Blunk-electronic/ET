------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                                NETS                                      --
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

with et_conductor_segment.boards;
with et_conductor_polygons.boards;
with et_vias;


package body et_nets is
	

	function "<" (left, right : in type_device_port) return boolean is
		use et_symbols.pac_port_name;
		use et_devices.pac_unit_name;
	begin
		-- compare device names:
		if left.device_name < right.device_name then
			return true;
			
		elsif left.device_name = right.device_name then

			
			-- compare unit names:
			if left.unit_name < right.unit_name then
				return true;
				
			elsif left.unit_name = right.unit_name then


				-- compare port names:
				if left.port_name < right.port_name then
					return true;
				else
					return false;
				end if;

			else
				return false;
			end if;

			
		else
			return false;
		end if;
	end;


	
	function "<" (left, right : in type_submodule_port) return boolean is
		use et_general.pac_module_instance_name;
		use et_net_names.pac_net_name;
	begin
		if left.module_name < right.module_name then
			return true;
		elsif left.module_name > right.module_name then
			return false;
		elsif left.port_name < right.port_name then
			return true;
		else
			return false;
		end if;
	end;


	function to_string (port : in type_device_port) return string is begin
		return "device " & to_string (port.device_name)
			& " unit " & et_devices.to_string (port.unit_name)
			& " port " & et_symbols.to_string (port.port_name);
	end to_string;

	
	procedure iterate (
		ports	: in pac_device_ports.set;
		process	: not null access procedure (position : in pac_device_ports.cursor);
		proceed	: not null access boolean)
	is
		use pac_device_ports;
		c : pac_device_ports.cursor := ports.first;
	begin
		while c /= no_element and proceed.all = TRUE loop
			process (c);
			next (c);
		end loop;
	end iterate;


	

	function to_string (segment : in pac_net_segments.cursor) return string is
		use pac_net_segments;
	begin
		return ("segment start" & 
			to_string (point => element (segment).start_point) &
			" / end" &	
			to_string (point => element (segment).end_point)
			);
	end to_string;

	
	function segment_orientation (segment : in pac_net_segments.cursor) 
		return type_net_segment_orientation is
		use pac_net_segments;
		
		result : type_net_segment_orientation;
		
		dx : constant et_coordinates.type_distance := get_x (element (segment).start_point) - get_x (element (segment).end_point);
		dy : constant et_coordinates.type_distance := get_y (element (segment).start_point) - get_y (element (segment).end_point);
	begin
		if dx = zero then 
			result := VERTICAL;
		
		elsif dy = zero then
			result := HORIZONTAL;
			
		else 
			result := SLOPING;
		end if;

		--put_line (type_net_segment_orientation'image (result));
		return result;
	end segment_orientation;


	procedure set_strand_position (strand : in out type_strand) is
		point_1, point_2 : type_point;
	
		use pac_net_segments;

		-- CS: usage of intermediate variables for x/Y of start/end points could improve performance

		procedure query_strand (cursor : in pac_net_segments.cursor) is begin
			-- Test start point of segment. 
			-- if closer to orign than point_1 keep start point
			point_2	:= type_point (element (cursor).start_point);
			if get_distance_total (point_2, origin) < get_distance_total (point_1, origin) then
				point_1 := point_2;
			end if;

			-- Test start point of segment.
			-- if closer to orign than point_1 keep end point
			point_2	:= type_point (element (cursor).end_point);
			if get_distance_total (point_2, origin) < get_distance_total (point_1, origin) then
				point_1 := point_2;
			end if;
		end query_strand;
	
	begin
		--log (text => "set strand position");
		
		-- init point_1 as the farest possible point from drawing origin
		point_1 := type_point (set (
					x => type_position_axis'last,
					y => type_position_axis'last));

		-- loop through segments and keep the nearest point to origin
		iterate (strand.segments, query_strand'access);

		-- build and assign the final strand position from point_1
		set (
			point	 => strand.position,
			position => point_1);

	end set_strand_position;


	
	function get_first_segment (
		strand_cursor	: in pac_strands.cursor)
		return pac_net_segments.cursor
	is
		use pac_strands;
		segment_cursor : pac_net_segments.cursor; -- to be returned

		procedure query_segments (strand : in type_strand) is
			use pac_net_segments;

			segment_position : type_point := far_upper_right;
			
			procedure query_segment (c : in pac_net_segments.cursor) is begin
				if element (c).start_point < segment_position then
					segment_position := element (c).start_point;
					segment_cursor := c;
				end if;

				if element (c).end_point < segment_position then
					segment_position := element (c).end_point;
					segment_cursor := c;
				end if;
			end query_segment;
			
		begin
			iterate (strand.segments, query_segment'access);
		end query_segments;
		
	begin
		query_element (
			position	=> strand_cursor,
			process		=> query_segments'access);
		
		return segment_cursor;
	end get_first_segment;




	procedure merge_nets (
		net_1	: in out type_net;
		net_2	: in type_net)
	is
		use pac_strands;
		net_2_copy : type_net := net_2;

		use et_conductor_segment.boards;
		use pac_conductor_lines;
		use pac_conductor_arcs;

		use et_vias;
		use pac_vias;

		use et_conductor_polygons.boards;
		use pac_solid_route;
		use pac_hatched_route;
		use pac_conductor_cutouts;
		
	begin
		-- SCHEMATIC
		
		-- strands:
		splice (
			target => net_1.strands, 
			before => pac_strands.no_element,
			source => net_2_copy.strands);

		-- BOARD:
		
		-- conductor lines:
		splice (
			target => net_1.route.lines, 
			before => pac_conductor_lines.no_element,
			source => net_2_copy.route.lines);

		-- conductor arcs:
		splice (
			target => net_1.route.arcs, 
			before => pac_conductor_arcs.no_element,
			source => net_2_copy.route.arcs);

		-- vias:
		splice (
			target => net_1.route.vias, 
			before => pac_vias.no_element,
			source => net_2_copy.route.vias);

		-- polygons/fill zones:
		splice (
			target => net_1.route.polygons.solid, 
			before => pac_solid_route.no_element,
			source => net_2_copy.route.polygons.solid);
		
		splice (
			target => net_1.route.polygons.hatched, 
			before => pac_hatched_route.no_element,
			source => net_2_copy.route.polygons.hatched);

		-- cutout areas:
		splice (
			target => net_1.route.cutouts, 
			before => pac_conductor_cutouts.no_element,
			source => net_2_copy.route.cutouts);

		
	end merge_nets;


	
end et_nets;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

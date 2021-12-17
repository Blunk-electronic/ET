------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              SCHEMATIC                                   --
--                                                                          --
--                               B o d y                                    --
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

with ada.characters.handling;	use ada.characters.handling;
with ada.exceptions;

package body et_schematic is

	function "<" (left, right : in type_device_port) return boolean is
		use et_symbols.pac_port_name;
	begin
		if left.device_name < right.device_name then
			return true;
			
		elsif left.device_name = right.device_name then
			
			if left.port_name < right.port_name then
				return true;
			else
				return false;
			end if;

		else
			return false;
		end if;
	end;

	function "<" (left, right : in type_submodule_port) return boolean is
		use et_general.pac_module_instance_name;
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

	function to_string (appearance : in type_net_label_appearance) return string is begin
		return to_lower (type_net_label_appearance'image (appearance));
	end;

	function to_appearance (appearance : in string) return type_net_label_appearance is begin
		return type_net_label_appearance'value (appearance);
	end;
	
	function to_string (direction : in type_net_label_direction) return string is begin
		return to_lower (type_net_label_direction'image (direction));
	end;

	function to_direction (direction : in string) return type_net_label_direction is begin
		return type_net_label_direction'value (direction);
	end;
	
	function to_string (segment : in pac_net_segments.cursor) return string is
	-- Returns a string that tells about start and end coordinates of the net segment.
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

	
	procedure set_strand_position (strand : in out type_strand) is
	-- Calculates and sets the lowest x/y position of the given strand.
	-- Leaves the sheet number of the strand as it is.
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


	procedure iterate (
		nets	: in pac_nets.map;
		process	: not null access procedure (position : in pac_nets.cursor);
		proceed	: not null access boolean)
	is
		use pac_nets;
		c : pac_nets.cursor := nets.first;
	begin
		while c /= pac_nets.no_element and proceed.all = TRUE loop
			process (c);
			next (c);
		end loop;
	end iterate;



			
	function get_first_strand_on_sheet (
		sheet		: in type_sheet;
		net_cursor	: in pac_nets.cursor)
		return pac_strands.cursor
	is
		use pac_nets;
		strand_cursor : pac_strands.cursor; -- to be returned

		strand_position : et_coordinates.type_position := greatest_position;
		
		procedure query_strands (
			net_name	: in pac_net_name.bounded_string;
			net			: in type_net)
		is
			use pac_strands;

			c : pac_strands.cursor := net.strands.first;
		begin			
			while c /= pac_strands.no_element loop

				-- Probe strands on the given sheet only:
				if et_coordinates.sheet (element (c).position) = sheet then

					if element (c).position < strand_position then
						strand_position := element (c).position;
						strand_cursor := c;
					end if;

				end if;
				
				next (c); -- advance to next strand
			end loop;
		end query_strands;

	begin
		query_element (
			position	=> net_cursor,
			process		=> query_strands'access);

		return strand_cursor;
	end get_first_strand_on_sheet;
	
	function get_first_strand (
		net_cursor	: in pac_nets.cursor)
		return pac_strands.cursor
	is
		use pac_nets;
		strand_cursor : pac_strands.cursor; -- to be returned

		strand_position : et_coordinates.type_position := greatest_position;
		
		procedure query_strands (
			net_name	: in pac_net_name.bounded_string;
			net			: in type_net)
		is
			use pac_strands;

			procedure query_strand (c : in pac_strands.cursor) is begin
				if element (c).position < strand_position then
					strand_position := element (c).position;
					strand_cursor := c;
				end if;
			end query_strand;
			
		begin			
			iterate (net.strands, query_strand'access);
		end query_strands;
			
	begin -- get_first_strand
		query_element (
			position	=> net_cursor,
			process		=> query_strands'access);
	
		return strand_cursor;
	end get_first_strand;
				
	function to_label_rotation (direction : in type_stub_direction) 
		return type_rotation is
	begin
		case direction is
			when RIGHT	=> return zero_rotation;
			when LEFT	=> return 180.0;
			when UP		=> return 90.0;
			when DOWN	=> return -90.0;
		end case;
	end to_label_rotation;
	
	function stub_direction (
		segment	: in pac_net_segments.cursor;
		point	: in pac_geometry_sch.type_point)
		return type_stub is

		use pac_net_segments;

		is_stub : boolean := true;
		direction : type_stub_direction;
		orientation : constant type_net_segment_orientation := segment_orientation (segment);
	begin
		case orientation is
			when HORIZONTAL =>
				if get_x (point) >= get_x (element (segment).start_point) and
					get_x (point) >= get_x (element (segment).end_point) then
					direction := RIGHT;
				end if;

				if get_x (point) <= get_x (element (segment).start_point) and
					get_x (point) <= get_x (element (segment).end_point) then
					direction := LEFT;
				end if;
				
			when VERTICAL =>
				if get_y (point) >= get_y (element (segment).start_point) and
					get_y (point) >= get_y (element (segment).end_point) then
					direction := UP;
				end if;

				if get_y (point) <= get_y (element (segment).start_point) and
					get_y (point) <= get_y (element (segment).end_point) then
					direction := DOWN;
				end if;
				
			when SLOPING =>
				is_stub := false;
		end case;

		if is_stub then
			return (is_stub => TRUE, direction => direction);
		else
			return (is_stub => FALSE);
		end if;

	end stub_direction;

	
	function get_ports (
		net		: in pac_nets.cursor;
		variant	: in pac_assembly_variants.cursor)
		return type_ports 
	is
		result : type_ports; -- to be returned

		use pac_nets;
		use pac_strands;
		use pac_net_segments;

		procedure query_segments (segment_cursor : in pac_net_segments.cursor) is
			use pac_device_ports;

			use et_netlists;
			use pac_netchanger_ports;
			
			use pac_submodule_ports;

			procedure query_devices (device_cursor : in pac_device_ports.cursor) is
			-- Inserts the device/port in result.devices. Skips the device/port
			-- according to the given assembly variant.
			begin
				if et_assembly_variants.is_mounted (
					device		=> element (device_cursor).device_name, -- IC4, R101
					variant		=> variant) 
				then
					insert (
						container	=> result.devices,
						new_item	=> element (device_cursor));
				end if;
			end query_devices;
			
		begin -- query_segments
			-- Collect device ports of segment according to given assembly variant.
			iterate (element (segment_cursor).ports_devices, query_devices'access);

			-- Ports of netchangers and submodules go into the result right away
			-- because they are not affected by any assembly variants.
			union (result.netchangers, element (segment_cursor).ports_netchangers);
			union (result.submodules, element (segment_cursor).ports_submodules);
		end query_segments;
		
		procedure query_strands (strand_cursor : in pac_strands.cursor) is 
		begin
			iterate (element (strand_cursor).segments, query_segments'access);
		end query_strands;
	
	begin
		iterate (element (net).strands, query_strands'access);
		return result;
	end get_ports;




	
	procedure iterate (
		devices	: in pac_devices_sch.map;
		process	: not null access procedure (position : in pac_devices_sch.cursor);
		proceed	: not null access boolean)
	is 
		use pac_devices_sch;
		c : pac_devices_sch.cursor := devices.first;
	begin
		while c /= no_element and proceed.all = TRUE loop
			process (c);
			next (c);
		end loop;
	end iterate;

	

	

	


	
	
	function to_string (
		mirror	: in type_mirror;
		verbose : in boolean)
		return string 
	is begin
		if verbose then
			return "mirrored " & to_lower (type_mirror'image (mirror));
		else
			return to_lower (type_mirror'image (mirror));
		end if;
	end to_string;

	
	function to_mirror_style (style : in string) return type_mirror is begin
		return type_mirror'value (style);
	end to_mirror_style;

	
	function to_string (unit : in pac_units.cursor) return string is
		use pac_units;
	begin
		return et_devices.to_string (key (unit)) 
			& to_string (type_point (element (unit).position));
	end to_string;

	
	function unit_positions (
		units : in pac_units.map)
		return pac_unit_positions.map
	is
		list : pac_unit_positions.map; -- to be returned
		use pac_units;
		use pac_unit_positions;
		
		procedure query_unit (cursor : pac_units.cursor) is begin
			list.insert (key (cursor), element (cursor).position);
		end;
		
	begin
		iterate (units, query_unit'access);
		return list;
	end unit_positions;



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

	
	--procedure iterate (
		--devices	: in pac_devices_non_electric.map;
		--process	: not null access procedure (position : in pac_devices_non_electric.cursor);
		--proceed	: not null access boolean)
	--is
		--use pac_devices_non_electric;
		--c : pac_devices_non_electric.cursor := devices.first;
	--begin
		--while c /= no_element and proceed.all = TRUE loop
			--process (c);
			--next (c);
		--end loop;
	--end iterate;


	
	procedure device_name_in_use (
		name	: in type_device_name;	-- IC1, MH1, ...
		by_cat	: in type_device_category)	-- electrical/non-electrical
	is begin
		case by_cat is
			when NON_ELECTRICAL =>
				log (ERROR, "Name " & enclose_in_quotes (to_string (name)) &
					 " already used by another non-electrical device !",
					 console => true);

			when ELECTRICAL =>
				log (ERROR, "Name " & enclose_in_quotes (to_string (name)) &
					 " already used by another electrical device !",
					 console => true);
		end case;
		
		raise constraint_error;
	end device_name_in_use;

		
	function show_danger (danger : in type_danger) return string is
		preamble : constant string (1..9) := " RISK OF ";
	begin
		case danger is
			when floating_input		=> return preamble & "FLOATING INPUT(S) !";
			when contention			=> return preamble & "CONTENTION !";
			when short_circuit		=> return preamble & "SHORT CIRCUIT OR OVERLOAD !";
			when no_power_supply	=> return preamble & "COMPONENT DAMAGE !";
			when not_predictable	=> return preamble & "UNPREDICTABLE HARM !";
		end case;	
	end show_danger;

	
end et_schematic;
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

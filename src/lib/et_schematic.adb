------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              SCHEMATIC                                   --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2020 Mario Blunk, Blunk electronic          --
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

	use et_devices;
	
	function "<" (left, right : in type_port_device) return boolean is
		use et_symbols.type_port_name;
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

	function "<" (left, right : in type_port_submodule) return boolean is
		use et_general.type_module_instance_name;
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
	
	function to_string (segment : in type_net_segments.cursor) return string is
	-- Returns a string that tells about start and end coordinates of the net segment.
		use et_coordinates;
		use type_net_segments;
	begin
		return ("segment start" & 
			to_string (point => element (segment).start_point) &
			" / end" &	
			to_string (point => element (segment).end_point)
			);
	end to_string;

	function segment_orientation (segment : in type_net_segments.cursor) 
		return type_net_segment_orientation is
		use et_coordinates;
		use type_net_segments;
		
		result : type_net_segment_orientation;
		
		dx : constant et_coordinates.type_distance := x (element (segment).start_point) - x (element (segment).end_point);
		dy : constant et_coordinates.type_distance := y (element (segment).start_point) - y (element (segment).end_point);
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
	-- Calculates and sets the lowest x/y position of the given strand.
	-- Leaves the sheet number of the strand as it is.
		point_1, point_2 : type_point;
	
		use type_net_segments;
		use et_string_processing;
		use et_coordinates;

		-- CS: usage of intermediate variables for x/Y of start/end points could improve performance

		procedure query_strand (cursor : in type_net_segments.cursor) is begin
			-- Test start point of segment. 
			-- if closer to orign than point_1 keep start point
			point_2	:= type_point (element (cursor).start_point);
			if distance_total (point_2, origin) < distance_total (point_1, origin) then
				point_1 := point_2;
			end if;

			-- Test start point of segment.
			-- if closer to orign than point_1 keep end point
			point_2	:= type_point (element (cursor).end_point);
			if distance_total (point_2, origin) < distance_total (point_1, origin) then
				point_1 := point_2;
			end if;
		end query_strand;
	
	begin
		--log (text => "set strand position");
		
		-- init point_1 as the farest possible point from drawing origin
		point_1 := type_point (set (
					x => type_distance_xy'last,
					y => type_distance_xy'last));

		-- loop through segments and keep the nearest point to origin
		iterate (strand.segments, query_strand'access);

		-- build and assign the final strand position from point_1
		set (
			point	 => strand.position,
			position => point_1);

	end set_strand_position;

	function to_label_rotation (direction : in type_stub_direction) 
		return et_coordinates.type_rotation is
		use et_coordinates;
	begin
		case direction is
			when RIGHT	=> return zero_rotation;
			when LEFT	=> return 180.0;
			when UP		=> return 90.0;
			when DOWN	=> return -90.0;
		end case;
	end to_label_rotation;
	
	function stub_direction (
		segment	: in type_net_segments.cursor;
		point	: in et_coordinates.pac_geometry_sch.type_point)
		return type_stub is

		use et_coordinates;
		use type_net_segments;

		is_stub : boolean := true;
		direction : type_stub_direction;
		orientation : constant type_net_segment_orientation := segment_orientation (segment);
	begin
		case orientation is
			when HORIZONTAL =>
				if x (point) >= x (element (segment).start_point) and
					x (point) >= x (element (segment).end_point) then
					direction := RIGHT;
				end if;

				if x (point) <= x (element (segment).start_point) and
					x (point) <= x (element (segment).end_point) then
					direction := LEFT;
				end if;
				
			when VERTICAL =>
				if y (point) >= y (element (segment).start_point) and
					y (point) >= y (element (segment).end_point) then
					direction := UP;
				end if;

				if y (point) <= y (element (segment).start_point) and
					y (point) <= y (element (segment).end_point) then
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
	
	function ports (
		net		: in type_nets.cursor;
		variant	: in et_assembly_variants.pac_variants.cursor)
		return type_ports is
	-- Returns the ports of devices, submodules and netchangers in
	-- the given net. The given assembly variant determines whether
	-- a device should be excluded.
	-- NOTE: If variant points to no element, then the default variant is assumend
	-- and ALL devices are returned.
		result : type_ports; -- to be returned

		use type_nets;
		use type_strands;
		use type_net_segments;
		use et_string_processing;

		procedure query_segments (segment_cursor : in type_net_segments.cursor) is
			use type_ports_device;

			use et_netlists;
			use type_ports_netchanger;
			
			use type_ports_submodule;

			procedure query_devices (device_cursor : in type_ports_device.cursor) is
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
		
		procedure query_strands (strand_cursor : in type_strands.cursor) is 
		begin
			iterate (element (strand_cursor).segments, query_segments'access);
		end query_strands;
	
	begin -- ports
		iterate (element (net).strands, query_strands'access);
		return result;
	end ports;
	
	function package_model (device : in type_devices.cursor)
		return et_packages.type_package_model_file.bounded_string is -- libraries/packages/smd/SOT23.pac
	-- Returns the name of the package model of the given device.
	-- The given device must have appearance SCH_PCB. Otherwise constraint error arises here.
		device_model		: et_devices.type_device_model_file.bounded_string;
		device_cursor_lib	: et_devices.type_devices.cursor;
		device_variant		: et_devices.type_variant_name.bounded_string; -- N, D
	begin
		-- CS: The device is located twice here. Consumes too much time.
		-- The issue may dissolve once devices are stored in a hashed map:
		
		-- load package variant of given device
		device_variant := type_devices.element (device).variant;
		
		-- load the name of the generic device model
		device_model := type_devices.element (device).model;


		
		-- locate the generic device model in the device library
		device_cursor_lib := et_devices.locate_device (device_model);
		
		return et_devices.package_model (device_cursor_lib, device_variant);
	end package_model;

	function has_real_package (device : in type_devices.cursor) return boolean is
	-- Returns true if the given device has a real package.
	-- The given device must have appearance SCH_PCB. Otherwise constraint error arises here.
		package_name : et_packages.type_package_model_file.bounded_string; -- libraries/packages/smd/SOT23.pac
	begin
		-- get the package name of the given device:
		package_name := package_model (device);

		-- ask for the package status (real or virtual) and return the result right away:
		return et_packages.is_real (package_name);
	end has_real_package;
	
	function to_string (
		mirror	: in type_mirror;
		verbose : in boolean)
		return string is
	-- returns the given mirror style as string
	begin
		if verbose then
			return "mirrored " & to_lower (type_mirror'image (mirror));
		else
			return to_lower (type_mirror'image (mirror));
		end if;
	end to_string;

	function to_mirror_style (style : in string) return type_mirror is begin
		return type_mirror'value (style);
	end to_mirror_style;

	function unit_positions (units : in type_units.map) return type_unit_positions.map is
	-- Returns a list of units and their coordinates in the schematic.
		list : type_unit_positions.map; -- to be returned
		use type_units;
		use type_unit_positions;
		
		procedure query_unit (cursor : type_units.cursor) is begin
			list.insert (key (cursor), element (cursor).position);
		end;
		
	begin
		iterate (units, query_unit'access);
		return list;
	end unit_positions;


	procedure device_name_in_use (
		name	: in et_devices.type_name;	-- IC1, MH1, ...
		by_cat	: in type_device_category)	-- electrical/non-electrical
	is 
		use et_string_processing;
	begin
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

	
-- 	function position (device : in et_libraries.type_device_name) return type_device_position is
-- 		dev_pos : type_device_position; -- to be returned
-- 		use type_devices;
-- 	begin
-- 		return dev_pos;
-- 	end position;
	
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

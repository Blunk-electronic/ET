------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              SCHEMATIC                                   --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2019 Mario Blunk, Blunk electronic                 --
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

with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;
with ada.strings; 				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;

with ada.text_io;				use ada.text_io;

with ada.exceptions;
with ada.directories;

with ada.containers;            use ada.containers;
with ada.containers.ordered_maps;

with et_general;
with et_coordinates;
with et_string_processing;
with et_export;
with et_import;
with et_csv;
with et_packages;
with et_symbols;
with et_devices;

package body et_schematic is
	use et_coordinates.geometry;




	
	function to_net_label_text_size (text : in string) return type_net_label_text_size is
	-- Converts a string to type_net_label_text_size.
	begin
		return type_net_label_text_size'value (text);
	end to_net_label_text_size;

	function "<" (left, right : in type_port_device) return boolean is
		use et_libraries;
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
		return latin_1.space & to_lower (type_net_label_appearance'image (appearance));
	end to_string;

	function to_appearance (appearance : in string) return type_net_label_appearance is begin
		return type_net_label_appearance'value (appearance);
	end to_appearance;
	
	function to_string (direction : in type_net_label_direction) return string is begin
		return latin_1.space & to_lower (type_net_label_direction'image (direction));
	end to_string;

	function to_direction (direction : in string) return type_net_label_direction is begin
		return type_net_label_direction'value (direction);
	end to_direction;
	
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
	
	procedure set_strand_position (strand : in out type_strand) is
	-- Calculates and sets the lowest x/y position of the given strand.
	-- Leaves the sheet number of the strand as it is.
		point_1, point_2 : type_point;
	
		use type_net_segments;
		use et_string_processing;
		use et_coordinates;
		use geometry;

		-- CS: usage of intermediate variables for x/Y of start/end points could improve performance

		procedure query_strand (cursor : in type_net_segments.cursor) is begin
			-- Test start point of segment. 
			-- if closer to orign than point_1 keep start point
			point_2	:= type_point (element (cursor).start_point);
			if distance (point_2, origin) < distance (point_1, origin) then
				point_1 := point_2;
			end if;

			-- Test start point of segment.
			-- if closer to orign than point_1 keep end point
			point_2	:= type_point (element (cursor).end_point);
			if distance (point_2, origin) < distance (point_1, origin) then
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
	
	function ports (
		net		: in type_nets.cursor;
		variant	: in assembly_variants.type_variants.cursor)
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

			use netlists;
			use type_ports_netchanger;
			
			use type_ports_submodule;

			procedure query_devices (device_cursor : in type_ports_device.cursor) is
			-- Inserts the device/port in result.devices. Skips the device/port
			-- according to the given assembly variant.
			begin
				if assembly_variants.is_mounted (
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
		device_variant		: et_devices.type_component_variant_name.bounded_string; -- N, D
	begin
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
			return latin_1.space & to_lower (type_mirror'image (mirror));
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
	
	procedure statistics_set (
		cat			: in type_statistics_category;
		increment	: in boolean := true;
		number 		: in count_type := 0) is 
	begin
		if increment then
			case cat is
				when COMPONENTS_TOTAL		=> statistics.components_total		:= statistics.components_total + 1;
				when COMPONENTS_VIRTUAL		=> statistics.components_virtual	:= statistics.components_virtual + 1;
				when COMPONENTS_REAL		=> statistics.components_real		:= statistics.components_real + 1;
				when COMPONENTS_MOUNTED		=> statistics.components_mounted	:= statistics.components_mounted + 1;
				
				when NETS_TOTAL				=> statistics.nets_total			:= statistics.nets_total + 1;
				when JUNCTIONS				=> statistics.junctions				:= statistics.junctions + 1;
				when PORTS_TOTAL			=> statistics.ports_total			:= statistics.ports_total + 1;
				
				when CAPACITORS				=> statistics.capacitors			:= statistics.capacitors + 1;
				when CONNECTORS				=> statistics.connectors			:= statistics.connectors + 1;
				when DIODES					=> statistics.diodes				:= statistics.diodes + 1;
				when INDUCTORS				=> statistics.inductors				:= statistics.inductors + 1;
				when INTEGRATED_CIRCUITS	=> statistics.integrated_circuits	:= statistics.integrated_circuits + 1;
				when JUMPERS				=> statistics.jumpers				:= statistics.jumpers + 1;
				when LEDS					=> statistics.leds					:= statistics.leds + 1;
-- 				when NETCHANGERS			=> statistics.netchangers			:= statistics.netchangers + 1;
				when RELAYS					=> statistics.relays				:= statistics.relays + 1;
				when RESISTORS				=> statistics.resistors				:= statistics.resistors + 1;
				when TESTPOINTS				=> statistics.testpoints			:= statistics.testpoints + 1;				
				when TRANSISTORS			=> statistics.transistors			:= statistics.transistors + 1;
			end case;
		else
			case cat is
				when COMPONENTS_TOTAL	=> statistics.components_total := number;
				when COMPONENTS_VIRTUAL	=> statistics.components_virtual := number;
				
				when others => null; -- CS
			end case;
		end if;
	end statistics_set;
		
	function statistics_query (cat : in type_statistics_category) return count_type is
	-- Returns the number objects as specified by given category.
	begin
		case cat is
			when COMPONENTS_TOTAL		=> return statistics.components_total;
			when COMPONENTS_VIRTUAL		=> return statistics.components_virtual;
			when COMPONENTS_REAL		=> return statistics.components_real;
			when COMPONENTS_MOUNTED		=> return statistics.components_mounted;
			
			when NETS_TOTAL				=> return statistics.nets_total;
			when JUNCTIONS				=> return statistics.junctions;
			when PORTS_TOTAL			=> return statistics.ports_total;
			
			when CAPACITORS				=> return statistics.capacitors;
			when CONNECTORS				=> return statistics.connectors;
			when DIODES					=> return statistics.diodes;
			when INDUCTORS				=> return statistics.inductors;
			when INTEGRATED_CIRCUITS	=> return statistics.integrated_circuits;
			when JUMPERS				=> return statistics.jumpers;
			when LEDS					=> return statistics.leds;
-- 			when NETCHANGERS			=> return statistics.netchangers;
			when RELAYS					=> return statistics.relays;
			when RESISTORS				=> return statistics.resistors;
			when TESTPOINTS				=> return statistics.testpoints;				
			when TRANSISTORS			=> return statistics.transistors;
		end case;

	end statistics_query;

	function statistics_query (cat : in type_statistics_category) return string is
	-- Returns the number objects as specified by given category.
	begin
		return count_type'image (statistics_query (cat));
	end statistics_query;

	
end et_schematic;
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

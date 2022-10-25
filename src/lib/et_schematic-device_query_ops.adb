------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                   SCHEMATIC DEVICE QUERY OPERATIONS                      --
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

package body et_schematic.device_query_ops is


	function is_real (
		device : in pac_devices_sch.cursor)
		return boolean 
	is begin
		case pac_devices_sch.element (device).appearance is
			when PCB		=> return true;
			when VIRTUAL	=> return false;
		end case;
	end is_real;


	function get_device (
		device : in pac_devices_sch.cursor)
		return pac_devices_lib.cursor
	is
		model_file : pac_device_model_file.bounded_string;
	begin
		-- The name of the device model file is THE link
		-- from device in schematic to device in library:
		model_file := pac_devices_sch.element (device).model;
		return locate_device (model_file);
	end get_device;


	function get_value (
		device : in pac_devices_sch.cursor)
		return pac_device_value.bounded_string 
	is begin
		return pac_devices_sch.element (device).value;
	end get_value;


	function get_purpose (
		device : in pac_devices_sch.cursor)
		return pac_device_purpose.bounded_string
	is begin
		return pac_devices_sch.element (device).purpose;
	end get_purpose;

	
	function get_partcode (
		device : in pac_devices_sch.cursor)
		return pac_device_partcode.bounded_string
	is begin
		return pac_devices_sch.element (device).partcode;
	end get_partcode;

	
	function get_variant (
		device : in pac_devices_sch.cursor)
		return pac_package_variant_name.bounded_string
	is begin
		return pac_devices_sch.element (device).variant;
	end get_variant;

	
	function get_package_model (
		device : in pac_devices_sch.cursor)
		return pac_package_model_file_name.bounded_string -- libraries/packages/smd/SOT23.pac
	is
		device_model		: pac_device_model_file.bounded_string;
		device_cursor_lib	: pac_devices_lib.cursor;
		device_variant		: pac_package_variant_name.bounded_string; -- N, D
	begin
		-- CS: The device is located twice here. Consumes too much time.
		-- The issue may dissolve once devices are stored in a hashed map:
		
		-- load package variant of given device
		device_variant := pac_devices_sch.element (device).variant;
		
		-- load the name of the generic device model
		device_model := pac_devices_sch.element (device).model;
		
		-- locate the generic device model in the device library
		device_cursor_lib := locate_device (device_model);
		
		return get_package_model (device_cursor_lib, device_variant);
	end get_package_model;

	
	function has_real_package (
		device : in pac_devices_sch.cursor) 
		return boolean 
	is
		package_name : pac_package_model_file_name.bounded_string; -- libraries/packages/smd/SOT23.pac
	begin
		-- get the package name of the given device:
		package_name := get_package_model (device);

		-- ask for the package status (real or virtual) and return the result right away:
		return is_real (package_name);
	end has_real_package;


	
	function get_port (
		device		: in pac_devices_sch.cursor;
		terminal	: in et_terminals.pac_terminal_name.bounded_string)
		return type_get_port_result
	is
		result : type_get_port_result;

		-- Get the cursor to the full device model in the library:
		device_model : constant pac_devices_lib.cursor := 
			locate_device (pac_devices_sch.element (device).model);

		-- This is the package variant used by the given device:
		variant_sch : constant pac_package_variant_name.bounded_string :=
			pac_devices_sch.element (device).variant; -- N, D

		
		procedure query_model (
			model	: in pac_device_model_file.bounded_string;
			device	: in type_device_lib)
		is
			use pac_variants;

			-- Locate the package variant of the given device
			-- in the device model:
			variant_lib : constant pac_variants.cursor := 
				find (device.variants, variant_sch);

			
			procedure query_terminal_port_map (
				name	: in pac_package_variant_name.bounded_string;
				variant	: in type_variant)
			is
				use pac_terminal_port_map;

				-- Locate the terminal in the terminal-port-map
				-- of the device model:
				t : constant pac_terminal_port_map.cursor :=
					find (variant.terminal_port_map, terminal);
			begin
				-- Get the port and unit name (which is what we want):
				if t /= pac_terminal_port_map.no_element then
					result := (
						linked	=> TRUE, 
						unit	=> element (t).unit, 
						port	=> element (t).name);
				else
					-- If the terminal can not be found in the map then
					-- it is not linked to any port.
					result := (linked => FALSE);
				end if;
			end query_terminal_port_map;

			
		begin
			query_element (variant_lib, query_terminal_port_map'access);
		end query_model;

		
	begin
		pac_devices_lib.query_element (device_model, query_model'access);
			
		return result;
	end get_port;

	
	


	
			
end et_schematic.device_query_ops;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

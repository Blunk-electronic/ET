------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                   SCHEMATIC DEVICE QUERY OPERATIONS                      --
--                                                                          --
--                               S p e c                                    --
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

with et_coordinates;
with et_terminals;
with et_packages;				use et_packages;
with et_pcb;
with et_pcb_coordinates;
with et_symbols;				use et_symbols;
with et_devices;				use et_devices;


package et_schematic.device_query_ops is

	--type type_device_exists is new boolean;
	--type type_terminal_exists is new boolean;

	
	--type type_get_port_result (
		--device_appearance	: type_appearance_schematic;
		--device_exists		: type_device_exists)
	--is record
		--case device_exists is
			--when TRUE =>
				--case device_appearance is
					--when PCB =>

			
		--terminal_exists		: type_terminal_exists := FALSE;
		--port				: pac_port_name.bounded_string; -- OE1, CS

			--when FALSE => null;
	--end record;

	-- Returns true if the given device is real.
	function is_real (
		device : in pac_devices_sch.cursor) 
		return boolean;


	-- Maps from schematic device to library device:
	function get_device (
		device : in pac_devices_sch.cursor)
		return pac_devices_lib.cursor;


	-- Returns the value of the given device.
	-- The device must be real. Otherwise constraint error is raised.
	function get_value (
		device : in pac_devices_sch.cursor)
		return pac_device_value.bounded_string;

	
	-- Returns the purpose of the given device.
	-- The device must be real. Otherwise constraint error is raised.
	function get_purpose (
		device : in pac_devices_sch.cursor)
		return pac_device_purpose.bounded_string;


	-- Returns the partcode of the given device.
	-- The device must be real. Otherwise constraint error is raised.
	function get_partcode (
		device : in pac_devices_sch.cursor)
		return et_material.type_partcode.bounded_string;


	-- Returns the package variant of the given device.
	-- The device must be real. Otherwise constraint error is raised.
	function get_variant (
		device : in pac_devices_sch.cursor)
		return pac_package_variant_name.bounded_string;


	-- Returns the name of the package model of the given device
	-- according to the current package variant of the device.
	-- The given device must be real. Otherwise constraint error arises here.	
	function get_package_model (
		device : in pac_devices_sch.cursor)
		return pac_package_model_file_name.bounded_string; -- libraries/packages/smd/SOT23.pac

	
	-- Returns true if the given device has a real package.
	-- The given device must have appearance SCH_PCB. 
	-- Otherwise a constraint error is raised.
	function has_real_package (
		device : in pac_devices_sch.cursor)
		return boolean;


	
	-- Maps from the given terminal to the linked port.
	-- The given device must be real. Otherwise a constraint error
	-- will be raised:
	function get_port (
		device		: in pac_devices_sch.cursor;
		terminal	: in et_terminals.pac_terminal_name.bounded_string) -- H7, 1, 14
		return pac_port_name.bounded_string;


	

	
			
end et_schematic.device_query_ops;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

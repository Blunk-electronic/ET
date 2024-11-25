------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                   DEVICE QUERY OPERATIONS IN SCHEMATIC                   --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2023                                                -- 
-- Mario Blunk / Blunk electronic                                           --
-- Buchfinkenweg 3 / 99097 Erfurt / Germany                                 --
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

with et_port_names;						use et_port_names;
with et_symbol_ports;					use et_symbol_ports;
with et_symbols;						use et_symbols;
with et_packages;						use et_packages;
with et_package_names;					use et_package_names;
with et_schematic;						use et_schematic;
with et_coordinates_2;					use et_coordinates_2;
with et_device_purpose;					use et_device_purpose;
with et_device_value;					use et_device_value;
with et_device_partcode;				use et_device_partcode;
with et_unit_name;						use et_unit_name;
with et_terminals;
with et_package_variant;				use et_package_variant;
with et_device_library;					use et_device_library;


package et_device_query_schematic is

	use pac_geometry_2;
	use pac_devices_sch;



	-- Maps from schematic device to device model (in library):
	function get_device_model (
		device : in pac_devices_sch.cursor)
		return pac_devices_lib.cursor;




	-- Returns the package variant of the given device.
	-- The device must be real. Otherwise constraint error is raised.
	function get_package_variant (
		device : in pac_devices_sch.cursor)
		return pac_package_variant_name.bounded_string;


	-- Returns the name of the package model of the given device
	-- according to the current package variant of the device.
	-- The given device must be real. Otherwise constraint error arises here.	
	function get_package_model ( -- CS rename to get_package_model_name
		device : in pac_devices_sch.cursor)
		return pac_package_model_file_name.bounded_string; -- libraries/packages/smd/SOT23.pac


	-- Returns the cursor to the package model of the given device
	-- according to the current package variant of the device.
	-- The given device must be real. Otherwise constraint error arises here.	
	function get_package_model (
		device : in pac_devices_sch.cursor)
		return pac_package_models.cursor;

	
	-- Returns true if the given device has a real package.
	-- The given device must have appearance SCH_PCB. 
	-- Otherwise a constraint error is raised.
	function has_real_package (
		device : in pac_devices_sch.cursor)
		return boolean;



	-- Maps from the given terminal to the linked port and unit.
	-- The given device must be real. Otherwise a constraint error
	-- will be raised:
	function get_port (
		device		: in pac_devices_sch.cursor;
		terminal	: in et_terminals.pac_terminal_name.bounded_string) -- H7, 1, 14
		return type_get_port_result;


	-- Maps from the given device cursor, unit and port name 
	-- to a cursor of the linked terminal.
	-- A port is always linked with a terminal.
	-- The given device must be real. Otherwise a constraint error will be raised:
	function get_terminal (
		device	: in pac_devices_sch.cursor;
		unit	: in pac_unit_name.bounded_string;
		port	: in pac_port_name.bounded_string)
		return et_terminals.pac_terminals.cursor;


			
end et_device_query_schematic;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

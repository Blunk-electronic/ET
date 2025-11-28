------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                       DEVICE LIBRARY PACKAGES                            --
--                                                                          --
--                              S p e c                                     --
--                                                                          --
-- Copyright (C) 2017 - 2025                                                --
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
-- DESCRIPTION:
--
--
--
--
-- ToDo:
--
--

with et_package_name;					use et_package_name;
with et_package_model_name;				use et_package_model_name;
with et_package_variant;				use et_package_variant;

with et_device_placeholders.packages;	use et_device_placeholders.packages;


package et_device_library.packages is



	-- Returns full information about the given package variant.
	-- If the package variant is not defined in the model, then
	-- then the result is no_element:
	function get_package_variant (
		device_cursor	: in pac_device_models.cursor;
		variant			: in pac_package_variant_name.bounded_string)  -- D, N
		return pac_package_variants.cursor;

	
	
	-- Returns true if given device provides the given package variant.								   
	-- The given device must be real. Means appearance SCH_PCB.
	function is_variant_available (
		device_cursor	: in pac_device_models.cursor;
		variant			: in pac_package_variant_name.bounded_string)  -- D, N
		return boolean;

	
	
	-- Returns a list of available variants of the given device.
	-- If the device is virtual, then an empty list will be returned.
	function get_available_variants (
		device_cursor	: in pac_device_models.cursor)
		return pac_package_variants.map;
	

	-- Returns the name of the first package variant
	-- of the given device model.
	-- The model must be a model of a real device. Otherwise
	-- an exception will be raised:
	function get_first_package_variant (
		device_cursor : in pac_device_models.cursor)
		return pac_package_variant_name.bounded_string;



		
	
	-- Returns the name of the package model of the given device according to the given variant.
	-- The given device must be real. Means appearance SCH_PCB.							  
	function get_package_model (
		device_cursor	: in pac_device_models.cursor;
		variant			: in pac_package_variant_name.bounded_string) -- D, N
		return pac_package_model_file.bounded_string; -- libraries/packages/smd/SOT23.pac

	
	-- 	function terminal_name (
-- 	-- Returns the name of the terminal name of the given device according to the given variant.
-- 	-- The given device must be real. Means appearance SCH_PCB.							  
-- 		device_cursor	: in pac_device_models.cursor;
-- 		port_name		: in pac_port_name.bounded_string;
-- 		variant			: in pac_package_variant_name.bounded_string) -- D, N
-- 		return pac_terminal_name.bounded_string; -- 14, H4

	


	-- Returns the default placeholders of the package of a device
	-- as they are specified in the package model.
	-- The package is indirectly selected by the given variant name.
	-- The given device is accessed by the given device cursor.
	function get_default_placeholders (
		device	: in pac_device_models.cursor;
		variant	: in pac_package_variant_name.bounded_string) -- N, D, S_0805
		return type_text_placeholders;



	
end et_device_library.packages;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

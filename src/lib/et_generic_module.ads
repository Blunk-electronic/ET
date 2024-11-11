------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           GENERIC MODULE                                 --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2024                                                --
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
--  ToDo: 
--  

with ada.containers;
with ada.containers.ordered_maps;

with et_schematic;
with et_assembly_variants;		use et_assembly_variants;
with et_assembly_variant_name;	use et_assembly_variant_name;
with et_module_names;			use et_module_names;
with et_meta;


package et_generic_module is

		
	-- Generic modules and submodules (which contain schematic and layout stuff)
	-- are collected here.
	-- Module names are things like "motor_driver" or "temperature_controller".
	-- Submodule names are things like "templates/clock_generator" or
	-- "$TEMPLATES/clock_generator" or "/home/user/templates/clock_generator":
	package pac_generic_modules is new ada.containers.ordered_maps (
		key_type		=> pac_module_name.bounded_string, -- motor_driver (without extension *.mod)
		"<"				=> pac_module_name."<",
		element_type	=> et_schematic.type_module,
		"="				=> et_schematic."=");


	use pac_generic_modules;


	function get_count (
		modules : in pac_generic_modules.map)
		return natural;

	
	
	generic_modules : pac_generic_modules.map;


	-- The current active module is stored here. Whenever objects of the schematic
	-- or board are to be drawn, this variable must be read.
	active_module : pac_generic_modules.cursor; -- the currently active module


	-- Returns the name of the currently active module:
	function get_active_module return string;


	
	
	-- Returns true if the module with the given name exists in container modules.
	function generic_module_exists (
		module : in pac_module_name.bounded_string) 
		return boolean;


	
	-- Locates the given module in the global container "modules".
	function locate_module (name : in pac_module_name.bounded_string) -- motor_driver (without extension *.mod)
		return pac_generic_modules.cursor;


	-- Fetches the meta information for the whole 
	-- module (both schematic and board):
	function get_meta_information (
		module : in pac_generic_modules.cursor)
		return et_meta.type_meta;


	
	-- Returns true if the given module provides the given assembly variant.
	-- If the variant is an empty string then it is about the default variant
	-- which is always provided. The return is true in that case.
	function assembly_variant_exists (
		module		: in pac_generic_modules.cursor;
		variant		: in pac_assembly_variant_name.bounded_string) -- low_cost
		return boolean;	


	
end et_generic_module;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

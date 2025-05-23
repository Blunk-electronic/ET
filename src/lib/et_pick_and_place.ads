------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                            PICK AND PLACE                                --
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
--   ToDo: 


with ada.text_io;				use ada.text_io;
with ada.strings.bounded;       use ada.strings.bounded;
with ada.containers;            use ada.containers;
with ada.containers.ordered_maps;

with et_module_names;			use et_module_names;
with et_assembly_variant_name;	use et_assembly_variant_name;
with et_string_processing;		use et_string_processing;
with et_logging;				use et_logging;
with et_board_coordinates;		use et_board_coordinates;
with et_device_name;			use et_device_name;

package et_pick_and_place is

	
	-- Whenever we deal with pick & place files this type should be used:
	file_name_length_max : constant positive := 100; -- CS: should suffice for now
	package pac_pnp_file_name is new generic_bounded_length (file_name_length_max); 

	extension_pnp : constant string := "pnp";

	function to_string (name : in pac_pnp_file_name.bounded_string) return string;
	function to_file_name (name : in string) return pac_pnp_file_name.bounded_string;

	type type_device is record
		position : et_board_coordinates.type_package_position; -- x/y, rotation and face
		-- CS value and partcode ?
	end record;

	package pac_devices is new ordered_maps (
		key_type		=> type_device_name, -- IC4
		element_type	=> type_device);

	type type_pnp_format is (
		NATIVE,
		EAGLE,
		KICAD
		-- CS others ?
		);		
	
	procedure write_pnp (
	-- Creates the P&P file (which inevitably and intentionally overwrites the previous file).
	-- Writes the content of the given container pnp in the file.
	-- - The P&P file will be named after the module name and the assembly variant.
	-- - Exports the P&P data of the given module to the export/CAM/pick_and_place directory.
		pnp				: in pac_devices.map;
		module_name		: in pac_module_name.bounded_string; -- motor_driver 
		variant_name	: in pac_assembly_variant_name.bounded_string; -- low_cost
		format			: in type_pnp_format := NATIVE;
		log_threshold	: in type_log_level);
	
end et_pick_and_place;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

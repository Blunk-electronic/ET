------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                            PICK AND PLACE                                --
--                                                                          --
--                              S p e c                                     --
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
with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded;       use ada.strings.bounded;
with ada.containers;            use ada.containers;
with ada.containers.vectors;
with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_doubly_linked_lists;
with ada.containers.ordered_maps;
with ada.containers.indefinite_ordered_maps;
with ada.containers.ordered_sets;

with et_general;				use et_general;

-- with assembly_variants;
with et_string_processing;		use et_string_processing;
-- with et_pcb;
with et_pcb_coordinates;		use et_pcb_coordinates;
-- with submodules;
-- with numbering;
with et_devices;				use et_devices;

package pick_and_place is

	
	-- Whenever we deal with pick & place files this type should be used:
	file_name_length_max : constant positive := 100; -- CS: should suffice for now
	package type_file_name is new generic_bounded_length (file_name_length_max); 

	extension_pnp : constant string := "pnp";

	function to_string (name : in type_file_name.bounded_string) return string;
	function to_file_name (name : in string) return type_file_name.bounded_string;

	type type_device is record
		position : et_pcb_coordinates.type_package_position; -- x/y, rotation and face
		-- CS value and partcode ?
	end record;

	package type_devices is new ordered_maps (
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
		pnp				: in type_devices.map;
		module_name		: in type_module_name.bounded_string; -- motor_driver 
		variant_name	: in type_variant_name.bounded_string; -- low_cost
		format			: in type_pnp_format := NATIVE;
		log_threshold	: in type_log_level);
	
end pick_and_place;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

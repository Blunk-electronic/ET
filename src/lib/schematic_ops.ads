------------------------------------------------------------------------------
--                                                                          --
--                     SYSTEM ET SCHEMATIC OPERATIONS                       --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               S p e c                                    --
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

--   The two letters "CS" indicate a "construction side" where things are not
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
with ada.characters.latin_1;
with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded;       use ada.strings.bounded;
with ada.containers;            use ada.containers;
with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_doubly_linked_lists;
with ada.containers.ordered_maps;
with ada.containers.indefinite_ordered_maps;
with ada.containers.ordered_sets;

-- with et_general;
with et_coordinates;			--use et_coordinates;
with et_libraries;				use et_libraries;
with et_string_processing;		use et_string_processing;
with et_schematic;				use et_schematic;
with et_project;				use et_project;

package schematic_ops is

	procedure delete_device (
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC45
		log_threshold	: in type_log_level);

	procedure delete_unit (
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC45
		unit_name		: in type_unit_name.bounded_string; -- A
		log_threshold	: in type_log_level);

	package type_ports is new doubly_linked_lists (
		element_type	=> et_libraries.type_port_name.bounded_string,
		"="				=> et_libraries.type_port_name."=");

	function ports_of_unit (
		device_cursor	: in et_schematic.type_devices.cursor;
		unit_name		: in type_unit_name.bounded_string)
		return type_ports.list;
	
end schematic_ops;

-- Soli Deo Gloria

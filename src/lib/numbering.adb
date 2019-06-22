------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                             NUMBERING                                    --
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

--   For correct displaying set tab with in your edtior to 4.

--   The two letters "CS" indicate a "construction side" where things are not
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
with gnat.directory_operations;

with ada.containers;            use ada.containers;

with et_general;				use et_general;
with et_string_processing;		use et_string_processing;
with et_project;				use et_project;

with et_coordinates;			use et_coordinates;
with et_libraries;				use et_libraries;
with et_schematic;				use et_schematic;
-- with schematic_ops;
-- with submodules;
-- with assembly_variants;
-- with board_ops;
with et_project;

package body numbering is
	
	function sort_by_coordinates (
		module_cursor 	: in et_project.type_modules.cursor;
		log_threshold	: in type_log_level) 
		return type_devices.map is
		devices : type_devices.map; -- to be returned

		procedure query_devices (
			module_name	: in type_module_name.bounded_string;
			module		: in type_module) is

			procedure query_units (device_cursor : in et_schematic.type_devices.cursor) is
				use et_schematic.type_units;
				device_name : type_device_name := et_schematic.type_devices.key (device_cursor); -- R1

				procedure sort (unit_cursor : in et_schematic.type_units.cursor) is 
					unit_name : type_unit_name.bounded_string := key (unit_cursor);  -- 1, C, IO_BANK1
					unit_position : et_coordinates.type_coordinates := element (unit_cursor).position;
					inserted : boolean := false;
					cursor_sort : type_devices.cursor;
				begin -- sort
					log (text => "unit " & to_string (unit_name) &
						" at " & to_string (position => unit_position),
						 level => log_threshold + 2);
					
					numbering.type_devices.insert 
						(
						container	=> devices,
						key			=> unit_position, -- sheet/x/y
						inserted	=> inserted,
						position	=> cursor_sort,
						new_item	=> (
									name => device_name, -- R1, IC3
									unit => unit_name, -- 1, C, IO_BANK1
									done => false -- not renumbered yet
									)
						);

					if not inserted then
						log (ERROR, "device " & to_string (device_name) &
							 " unit " & to_string (unit_name) &
							 " at " & to_string (position => unit_position) &
							 " sits on top of another unit !",
							 console => true);
						raise constraint_error;
					end if;
							 
				end sort;
				
			begin -- query_units
				
				log (text => "device " & to_string (device_name), -- R1, IC3
					 level => log_threshold + 1);
				
				log_indentation_up;
				
				et_schematic.type_units.iterate (
					container	=> et_schematic.type_devices.element (device_cursor).units,
					process		=> sort'access);

				log_indentation_down;
			end query_units;
			
		begin -- query_devices
			et_schematic.type_devices.iterate (
				container	=> module.devices,
				process		=> query_units'access);
		end query_devices;

	begin -- sort_by_coordinates
		log (text => "sorting devices/units by schematic coordinates ...", level => log_threshold);

		log_indentation_up;
		
		type_modules.query_element (
			position	=> module_cursor,
			process		=> query_devices'access);


		log_indentation_down;
		
		return devices;
	end sort_by_coordinates;

	function unit_positions_valid (
	-- Returns true if no unit sits on top of another.
		module_cursor 	: in et_project.type_modules.cursor;
		log_threshold	: in type_log_level)
		return boolean is
		devices : type_devices.map;
	begin
		devices := sort_by_coordinates (module_cursor, log_threshold);
		-- If a unit sits on top of another unit, sort_by_coordinates throws a
		-- constraint_error which will be catched here.

		return true;
		
		exception when event: others => 
			return false;
		
	end unit_positions_valid;

	
end numbering;
	
-- Soli Deo Gloria

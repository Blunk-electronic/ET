------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--              SCHEMATIC OPERATIONS / COPYING DEVICES                      --
--                                                                          --
--                               B o d y                                    --
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

--   For correct displaying set tab with in your edtior to 4.

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
--		- Currently the positions of placeholders are not copied from the 
--		  source unit. Instead the positions as defined by the library symbol 
--		  are taken.
--		- To fix this issue copy_device must be provided with the name of the
--		  source unit. From the source unit the positions must then be copied
--		  to the new unit.

with et_device_model;					use et_device_model;
with et_device_placeholders.symbols;	use et_device_placeholders.symbols;


separate (et_schematic_ops.units)

procedure copy_device (
	module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
	device_name		: in type_device_name; -- IC45
	destination		: in type_object_position; -- sheet/x/y/rotation
	log_threshold	: in type_log_level)
is
	module_cursor : pac_generic_modules.cursor; -- points to the module being modified

	
	procedure query_devices (
		module_name	: in pac_module_name.bounded_string;
		module		: in out type_generic_module) 
	is
		use et_device_appearance;
		use et_devices_electrical;
		use pac_devices_sch;
		device_cursor_sch : pac_devices_sch.cursor;

		use et_symbols;
		use pac_devices_lib;
		device_cursor_lib : pac_devices_lib.cursor; -- points to the device in the library
		
		-- the next available device name:
		next_name : type_device_name;
		inserted : boolean;

		placeholders : type_default_placeholders;
		
		unit_cursors : type_device_units;
		ports : pac_ports.map;

		use pac_unit_name;
		unit_name : pac_unit_name.bounded_string;

		use pac_units_internal;
		use pac_units_external;

		
		-- Add an internal unit to the schematic device.
		-- The unit to be added is accessed by unit_cursors.int.
		procedure add_unit_internal (
			device_name	: in type_device_name;
			device		: in out type_device_sch) 
		is begin
			log (text => "adding internal unit " & to_string (key (unit_cursors.int)), level => log_threshold + 2);
			
			case element (device_cursor_lib).appearance is
				when APPEARANCE_VIRTUAL =>
					pac_units.insert (
						container	=> device.units,
						key			=> key (unit_cursors.int), -- the unit name like A, B
						new_item	=> (
							appearance	=> APPEARANCE_VIRTUAL,
							position	=> destination, -- the coordinates provided by the calling unit (sheet,x,y,rotation)
							others 		=> <>)
							);
					
				when APPEARANCE_PCB =>

					-- Rotate the positions of placeholders and their rotation about
					-- their own origin according to rotation given by caller:
					placeholders := get_default_placeholders (unit_cursors.int, destination);
					
					pac_units.insert (
						container	=> device.units,
						key			=> key (unit_cursors.int), -- the unit name like A, B, VCC_IO_BANK_1
						new_item	=> (
							appearance	=> APPEARANCE_PCB,
							position	=> destination, -- the coordinates provided by the calling unit (sheet,x,y,rotation)
							placeholders => (
								name		=> placeholders.name,
								value		=> placeholders.value,
								purpose		=> placeholders.purpose),
							others 		=> <>) -- mirror
							);
			end case;
			
		end add_unit_internal;


		
		-- Add an external unit to the schematic device.
		-- The unit to be added is accessed by unit_cursors.ext.
		procedure add_unit_external (
			device_name	: in type_device_name;
			device		: in out type_device_sch) 
		is
			use pac_symbols;
			symbol_cursor : pac_symbols.cursor;
			symbol_file : pac_symbol_model_file.bounded_string; -- *.sym
		begin
			log (text => "adding external unit " & to_string (key (unit_cursors.ext)), level => log_threshold + 2);
			
			case element (device_cursor_lib).appearance is
				when APPEARANCE_VIRTUAL =>
					pac_units.insert (
						container	=> device.units,
						key			=> key (unit_cursors.ext), -- the unit name like A, B
						new_item	=> (
							appearance	=> APPEARANCE_VIRTUAL,
							position	=> destination, -- the coordinates provided by the calling unit (sheet,x,y,rotation)
							others 		=> <>)
							);
					
				when APPEARANCE_PCB =>
					-- The symbol file name is provided by unit_cursors.ext.
					symbol_file := element (unit_cursors.ext).model; -- *.sym
					
					-- Locate the external symbol in container "symbols".
					-- The key into symbols is the file name (*.sym).
					symbol_cursor := find (symbol_library, symbol_file);

					-- CS: The symbol should be there now. Otherwise symbol_cursor would assume no_element
					-- and constraint_error would arise here:

					-- Rotate the positions of placeholders and their rotation about
					-- their own origin according to rotation given by caller:
					placeholders := get_default_placeholders (symbol_cursor, destination);
					
					pac_units.insert (
						container	=> device.units,
						key			=> key (unit_cursors.ext), -- the unit name like A, B, VCC_IO_BANK_1
						new_item	=> (
							appearance	=> APPEARANCE_PCB,
							position	=> destination, -- the coordinates provided by the calling unit (sheet,x,y,rotation)
							placeholders => (
								name		=> placeholders.name,	
								value		=> placeholders.value,	
								purpose		=> placeholders.purpose),	
							others 		=> <>) -- mirror
							);
			end case;

		end add_unit_external;

		
	begin -- query_devices
		if contains (module.devices, device_name) then

			device_cursor_sch := find (module.devices, device_name); -- the device should be there

			log_indentation_up;

			-- build the next available device name:
			next_name := get_next_device_name (module_cursor, get_prefix (key (device_cursor_sch))); -- IC46
			log (text => "adding device " & to_string (next_name), level => log_threshold + 1);
			log_indentation_up;
			
			-- Create a new device. Copy lots of properties from the original device.
			-- The unit list is empty for the time being:
			case element (device_cursor_sch).appearance is
				when APPEARANCE_VIRTUAL =>
					pac_devices_sch.insert (
						container	=> module.devices,
						inserted	=> inserted,
						position	=> device_cursor_sch,
						key			=> next_name,
						new_item	=> (
							appearance 	=> APPEARANCE_VIRTUAL,
							model		=> element (device_cursor_sch).model,
							units		=> pac_units.empty_map
							));

				when APPEARANCE_PCB =>
					pac_devices_sch.insert (
						container	=> module.devices,
						inserted	=> inserted,
						position	=> device_cursor_sch,
						key			=> next_name,
						new_item	=> (
							appearance 	=> APPEARANCE_PCB,
							model		=> element (device_cursor_sch).model,
							units		=> pac_units.empty_map,
							value		=> element (device_cursor_sch).value, -- if predefined in dev. model
							partcode	=> element (device_cursor_sch).partcode,
							purpose		=> element (device_cursor_sch).purpose,
							variant		=> element (device_cursor_sch).variant,
							text_placeholders	=> element (device_cursor_sch).text_placeholders, -- layout related
							others		=> <> )); -- position in layout assumes default

			end case;

			-- locate the device in the library
			device_cursor_lib := find (device_library, element (device_cursor_sch).model);

			-- Add first available unit (according to search order specified in function first_unit)
			-- to device in schematic.
			unit_cursors := get_first_unit (device_cursor_lib);

			-- If an internal unit is available, add it to device. If no internal unit available
			-- but an external, add it to the device. So the operator will not take notice
			-- whether an internal or external unit is placed.
			if unit_cursors.int /= pac_units_internal.no_element then

				pac_devices_sch.update_element (
					container	=> module.devices,
					position	=> device_cursor_sch,
					process		=> add_unit_internal'access);

				-- fetch ports of unit and their positions relative to the unit origin
				log_indentation_up;				
				log (text => "fetching relative port positions of internal unit " &
					to_string (key (unit_cursors.int)) & " ...", level => log_threshold + 2);
				
				ports := get_ports_of_unit (
					device_cursor	=> device_cursor_sch,
					unit_name		=> key (unit_cursors.int));

				unit_name := key (unit_cursors.int);

			-- no internal unit available -> add external unit
			elsif unit_cursors.ext /= pac_units_external.no_element then
				
				pac_devices_sch.update_element (
					container	=> module.devices,
					position	=> device_cursor_sch,
					process		=> add_unit_external'access);

				-- fetch ports of unit and their positions relative to the unit origin
				log_indentation_up;
				log (text => "fetching relative port positions of external unit " &
					to_string (key (unit_cursors.ext)) & " ...", level => log_threshold + 2);

				ports := get_ports_of_unit (
					device_cursor	=> device_cursor_sch,
					unit_name		=> key (unit_cursors.ext));

				unit_name := key (unit_cursors.ext);
			else
				raise constraint_error; -- CS should never happen. function first_unit excludes this case.
			end if;

			-- Calculate the absolute positions of the unit ports. Rotate first if required:
			log (text => "calculating absolute port positions ...", level => log_threshold + 2);
			if get_rotation (destination) /= zero_rotation then
				rotate_ports (ports, get_rotation (destination));
			end if;
			
			move_ports (ports, destination);

			-- CS
			-- Insert the new unit ports in the nets (type_generic_module.nets):
			-- insert_ports (
			-- 	module			=> module_cursor,
			-- 	device			=> next_name,
			-- 	unit			=> unit_name,
			-- 	ports			=> ports,
			-- 	sheet			=> get_sheet (destination),
			-- 	log_threshold	=> log_threshold + 2);
			
			log_indentation_down;
			log_indentation_down;
			log_indentation_down;
		else
			device_not_found (device_name);
		end if;
	end query_devices;

	
begin -- copy_device
	log (text => "module " & to_string (module_name) &
		" copying " & to_string (device_name) & 
		" to" & to_string (position => destination) &
		" rotation" & to_string (get_rotation (destination)) &
		" ...", level => log_threshold);

	-- locate module
	module_cursor := locate_module (module_name);
	
	update_element (
		container	=> generic_modules,
		position	=> module_cursor,
		process		=> query_devices'access);

end copy_device;


-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

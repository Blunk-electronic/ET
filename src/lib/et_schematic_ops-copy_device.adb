------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--              SCHEMATIC OPERATIONS / COPYING DEVICES                      --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2020 Mario Blunk, Blunk electronic          --
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

separate (et_schematic_ops)

procedure copy_device (
	module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
	device_name		: in type_device_name; -- IC45
	destination		: in et_coordinates.type_position; -- sheet/x/y/rotation
	log_threshold	: in type_log_level) is

	module_cursor : pac_generic_modules.cursor; -- points to the module being modified
	
	procedure query_devices (
		module_name	: in pac_module_name.bounded_string;
		module		: in out type_module) is
		use et_schematic.type_devices;
		device_cursor_sch : et_schematic.type_devices.cursor;

		use et_symbols;
		use pac_devices_lib;
		device_cursor_lib : pac_devices_lib.cursor; -- points to the device in the library
		
		-- the next available device name:
		next_name : type_device_name;
		inserted : boolean;

		placeholders : type_rotated_placeholders;
		
		unit_cursors : type_device_units;
		ports : et_symbols.type_ports.map;
		unit_name : et_devices.pac_unit_name.bounded_string;

		use pac_units_internal;
		use pac_units_external;
		
		procedure add_unit_internal (
		-- Add an internal unit to the schematic device.
		-- The unit to be added is accessed by unit_cursors.int.
			device_name	: in type_device_name;
			device		: in out et_schematic.type_device) is
		begin
			log (text => "adding internal unit " & to_string (key (unit_cursors.int)), level => log_threshold + 2);
			
			case element (device_cursor_lib).appearance is
				when VIRTUAL =>
					type_units.insert (
						container	=> device.units,
						key			=> key (unit_cursors.int), -- the unit name like A, B
						new_item	=> (
							appearance	=> VIRTUAL,
							position	=> destination, -- the coordinates provided by the calling unit (sheet,x,y,rotation)
							others 		=> <>)
							);
					
				when PCB =>

					-- Rotate the positions of placeholders and their rotation about
					-- their own origin according to rotation given by caller:
					placeholders := rotate_placeholders (unit_cursors.int, destination);
					
					type_units.insert (
						container	=> device.units,
						key			=> key (unit_cursors.int), -- the unit name like A, B, VCC_IO_BANK_1
						new_item	=> (
							appearance	=> PCB,
							position	=> destination, -- the coordinates provided by the calling unit (sheet,x,y,rotation)
							name		=> placeholders.name,
							value		=> placeholders.value,
							purpose		=> placeholders.purpose,
							others 		=> <>) -- mirror
							);
			end case;
			
		end add_unit_internal;

		procedure add_unit_external (
		-- Add an external unit to the schematic device.
		-- The unit to be added is accessed by unit_cursors.ext.
			device_name	: in type_device_name;
			device		: in out et_schematic.type_device) is
			use type_symbols;
			symbol_cursor : type_symbols.cursor;
			symbol_file : type_symbol_model_file.bounded_string; -- *.sym
		begin
			log (text => "adding external unit " & to_string (key (unit_cursors.ext)), level => log_threshold + 2);
			
			case element (device_cursor_lib).appearance is
				when VIRTUAL =>
					type_units.insert (
						container	=> device.units,
						key			=> key (unit_cursors.ext), -- the unit name like A, B
						new_item	=> (
							appearance	=> VIRTUAL,
							position	=> destination, -- the coordinates provided by the calling unit (sheet,x,y,rotation)
							others 		=> <>)
							);
					
				when PCB =>
					-- The symbol file name is provided by unit_cursors.ext.
					symbol_file := element (unit_cursors.ext).model; -- *.sym
					
					-- Locate the external symbol in container "symbols".
					-- The key into symbols is the file name (*.sym).
					symbol_cursor := find (symbols, symbol_file);

					-- CS: The symbol should be there now. Otherwise symbol_cursor would assume no_element
					-- and constraint_error would arise here:

					-- Rotate the positions of placeholders and their rotation about
					-- their own origin according to rotation given by caller:
					placeholders := rotate_placeholders (symbol_cursor, destination);
					
					type_units.insert (
						container	=> device.units,
						key			=> key (unit_cursors.ext), -- the unit name like A, B, VCC_IO_BANK_1
						new_item	=> (
							appearance	=> PCB,
							position	=> destination, -- the coordinates provided by the calling unit (sheet,x,y,rotation)
							name		=> placeholders.name,	
							value		=> placeholders.value,	
							purpose		=> placeholders.purpose,	
							others 		=> <>) -- mirror
							);
			end case;

		end add_unit_external;
		
	begin -- query_devices
		if contains (module.devices, device_name) then

			device_cursor_sch := find (module.devices, device_name); -- the device should be there

			log_indentation_up;

			-- build the next available device name:
			next_name := next_device_name (module_cursor, prefix (key (device_cursor_sch))); -- IC46
			log (text => "adding device " & to_string (next_name), level => log_threshold + 1);
			log_indentation_up;
			
			-- Create a new device. Copy lots of properties from the original device.
			-- The unit list is empty for the time being:
			case element (device_cursor_sch).appearance is
				when VIRTUAL =>
					et_schematic.type_devices.insert (
						container	=> module.devices,
						inserted	=> inserted,
						position	=> device_cursor_sch,
						key			=> next_name,
						new_item	=> (
							appearance 	=> VIRTUAL,
							model		=> element (device_cursor_sch).model,
							units		=> type_units.empty_map
							));

				when PCB =>
					et_schematic.type_devices.insert (
						container	=> module.devices,
						inserted	=> inserted,
						position	=> device_cursor_sch,
						key			=> next_name,
						new_item	=> (
							appearance 	=> PCB,
							model		=> element (device_cursor_sch).model,
							units		=> type_units.empty_map,
							value		=> element (device_cursor_sch).value, -- if predefined in dev. model
							partcode	=> element (device_cursor_sch).partcode,
							purpose		=> element (device_cursor_sch).purpose,
							variant		=> element (device_cursor_sch).variant,
							text_placeholders	=> element (device_cursor_sch).text_placeholders, -- layout related
							others		=> <> )); -- position in layout assumes default

			end case;

			-- locate the device in the library
			device_cursor_lib := find (et_devices.devices, element (device_cursor_sch).model);

			-- Add first available unit (according to search order specified in function first_unit)
			-- to device in schematic.
			unit_cursors := first_unit (device_cursor_lib);

			-- If an internal unit is available, add it to device. If no internal unit available
			-- but an external, add it to the device. So the operator will not take notice
			-- whether an internal or external unit is placed.
			if unit_cursors.int /= pac_units_internal.no_element then

				et_schematic.type_devices.update_element (
					container	=> module.devices,
					position	=> device_cursor_sch,
					process		=> add_unit_internal'access);

				-- fetch ports of unit and their positions relative to the unit origin
				log_indentation_up;				
				log (text => "fetching relative port positions of internal unit " &
					to_string (key (unit_cursors.int)) & " ...", level => log_threshold + 2);
				
				ports := ports_of_unit (
					device_cursor	=> device_cursor_sch,
					unit_name		=> key (unit_cursors.int));

				unit_name := key (unit_cursors.int);

			-- no internal unit available -> add external unit
			elsif unit_cursors.ext /= pac_units_external.no_element then
				
				et_schematic.type_devices.update_element (
					container	=> module.devices,
					position	=> device_cursor_sch,
					process		=> add_unit_external'access);

				-- fetch ports of unit and their positions relative to the unit origin
				log_indentation_up;
				log (text => "fetching relative port positions of external unit " &
					to_string (key (unit_cursors.ext)) & " ...", level => log_threshold + 2);

				ports := ports_of_unit (
					device_cursor	=> device_cursor_sch,
					unit_name		=> key (unit_cursors.ext));

				unit_name := key (unit_cursors.ext);
			else
				raise constraint_error; -- CS should never happen. function first_unit excludes this case.
			end if;

			-- Calculate the absolute positions of the unit ports. Rotate first if required:
			log (text => "calculating absolute port positions ...", level => log_threshold + 2);
			if rot (destination) /= zero_rotation then
				rotate_ports (ports, rot (destination));
			end if;
			
			move_ports (ports, destination);
			
			-- Insert the new unit ports in the nets (type_module.nets):
			insert_ports (
				module			=> module_cursor,
				device			=> next_name,
				unit			=> unit_name,
				ports			=> ports,
				sheet			=> et_coordinates.sheet (destination),
				log_threshold	=> log_threshold + 2);
			
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
		" rotation" & to_string (rot (destination)) &
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

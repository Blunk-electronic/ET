------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--              SCHEMATIC OPERATIONS / INVOKING UNITS                       --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2022 Mario Blunk, Blunk electronic          --
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

separate (et_schematic_ops)

procedure invoke_unit (
-- Invokes a unit of a device into the schematic.
	module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
	device_name		: in type_device_name; -- IC1
	unit_name		: in pac_unit_name.bounded_string; -- A, B, IO_BANK_2
	destination		: in et_coordinates.type_position; -- sheet/x/y/rotation
	log_threshold	: in type_log_level) is

	use et_coordinates;
	
	module_cursor : pac_generic_modules.cursor; -- points to the targeted module

	procedure query_devices (
		module_name	: in pac_module_name.bounded_string;
		module		: in out type_module) is

		use et_symbols;
		use et_schematic.pac_devices_sch;
		device_cursor_sch : et_schematic.pac_devices_sch.cursor;

		procedure query_units_in_use (
			device_name	: in type_device_name;
			device		: in type_device_sch)
		is
			use et_schematic.pac_units;
		begin
			if contains (device.units, unit_name) then
				log (ERROR, to_string (device_name) &
						" unit " & to_string (unit_name) &
						" already deployed !", console => true);
				
				raise semantic_error_1 with
					"ERROR: " & to_string (device_name) &
					" unit " & to_string (unit_name) &
					" already deployed !";
				
			end if;
		end query_units_in_use;

		device_model : pac_device_model_file.bounded_string; -- ../libraries/devices/logic_ttl/7400.dev
		device_cursor_lib : pac_devices_lib.cursor;
		unit_cursors : type_device_units;

		placeholders : type_rotated_placeholders;
		
		use pac_units_external;
		use pac_units_internal;
		use pac_devices_lib;
		
		procedure add_unit_internal (
		-- Add an internal unit to the schematic device.
		-- The unit to be added is accessed by unit_cursors.int.
			device_name	: in type_device_name;
			device		: in out type_device_sch) is
			use et_symbols;
		begin
			log (text => "invoking internal unit " & to_string (key (unit_cursors.int)), level => log_threshold + 2);
			
			case element (device_cursor_lib).appearance is
				when VIRTUAL =>
					pac_units.insert (
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
					
					pac_units.insert (
						container	=> device.units,
						key			=> key (unit_cursors.int), -- the unit name like A, B, VCC_IO_BANK_1
						new_item	=> (
							appearance	=> PCB,
							position	=> destination, -- the coordinates provided by the calling unit (sheet,x,y,rotation)
							name		=> placeholders.name,
							value		=> placeholders.value,
							purpose		=> placeholders.purpose,
							others 		=> <>)
							);
			end case;
			
		end add_unit_internal;

		procedure add_unit_external (
		-- Add an external unit to the schematic device.
		-- The unit to be added is accessed by unit_cursors.ext.
			device_name	: in type_device_name;
			device		: in out type_device_sch) is
			use et_symbols;
			use et_symbols.pac_symbols;
			symbol_cursor : pac_symbols.cursor;
			symbol_file : pac_symbol_model_file.bounded_string; -- *.sym
		begin
			log (text => "invoking external unit " & to_string (key (unit_cursors.ext)), level => log_threshold + 2);
			
			case element (device_cursor_lib).appearance is
				when VIRTUAL =>
					pac_units.insert (
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
					
					pac_units.insert (
						container	=> device.units,
						key			=> key (unit_cursors.ext), -- the unit name like A, B, VCC_IO_BANK_1
						new_item	=> (
							appearance	=> PCB,
							position	=> destination, -- the coordinates provided by the calling unit (sheet,x,y,rotation)
							name		=> placeholders.name,
							value		=> placeholders.value,
							purpose		=> placeholders.purpose,
							others 		=> <>)
							);
			end case;

		end add_unit_external;

		ports : et_symbols.pac_ports.map; -- the positions of the unit ports
		
	begin -- query_devices
		if contains (module.devices, device_name) then -- device exists in schematic

			device_cursor_sch := find (module.devices, device_name);
			
			-- Test whether desired unit is already used by the device (in schematic).
			-- Abort if unit already in use.
			query_element (
				position	=> device_cursor_sch,
				process		=> query_units_in_use'access);

			-- Locate the device model in the library:
			device_model := element (device_cursor_sch).model;
			--device_cursor_lib := pac_devices_lib.find (et_devices.devices, device_model);
			device_cursor_lib := locate_device (device_model);

			-- Get cursor to the desired unit in device model.
			-- The unit can be internal or external.
			unit_cursors := any_unit (device_cursor_lib, unit_name);

			-- If the unit is internal, add it to the device in the schematic:
			if unit_cursors.int /= pac_units_internal.no_element then

				et_schematic.pac_devices_sch.update_element (
					container	=> module.devices,
					position	=> device_cursor_sch,
					process		=> add_unit_internal'access);

				-- fetch ports of unit and their positions relative to the unit origin
				log_indentation_up;				
				log (text => "fetching relative port positions of internal unit " &
					to_string (key (unit_cursors.int)) & " ...", level => log_threshold + 1);
				
				ports := get_ports_of_unit (
					device_cursor	=> device_cursor_sch,
					unit_name		=> key (unit_cursors.int));

			-- Unit is external -> add external unit to device in schematic:
			elsif unit_cursors.ext /= pac_units_external.no_element then
				
				et_schematic.pac_devices_sch.update_element (
					container	=> module.devices,
					position	=> device_cursor_sch,
					process		=> add_unit_external'access);

				-- fetch ports of unit and their positions relative to the unit origin
				log_indentation_up;
				log (text => "fetching relative port positions of external unit " &
					to_string (key (unit_cursors.ext)) & " ...", level => log_threshold + 1);

				ports := get_ports_of_unit (
					device_cursor	=> device_cursor_sch,
					unit_name		=> key (unit_cursors.ext));
				
			else
				raise constraint_error; -- CS should never happen. function any_unit excludes this case.
			end if;

			-- Calculate the absolute positions of the unit ports. Rotate first if required:
			log (text => "calculating absolute port positions ...", level => log_threshold + 1);
			if get_rotation (destination) /= zero_rotation then
				rotate_ports (ports, get_rotation (destination));
			end if;

			move_ports (ports, destination);
			
			-- Insert the new unit ports in the nets (type_module.nets):
			insert_ports (
				module			=> module_cursor,
				device			=> device_name,
				unit			=> unit_name,
				ports			=> ports,
				sheet			=> get_sheet (destination),
				log_threshold	=> log_threshold + 2);

			log_indentation_down;
		else
			device_not_found (device_name);
		end if;
	end query_devices;
	
begin -- invoke_unit
	log (text => "module " & to_string (module_name) &
		" device " & to_string (device_name) &
		" invoking unit " & to_string (unit_name) &
		" at" &
		to_string (position => destination) &
		" rotation" & to_string (get_rotation (destination)),
		level => log_threshold);

	log_indentation_up;
	
	-- locate module
	module_cursor := locate_module (module_name);

	update_element (
		container	=> generic_modules,
		position	=> module_cursor,
		process		=> query_devices'access);
	
	log_indentation_down;		
end invoke_unit;


-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

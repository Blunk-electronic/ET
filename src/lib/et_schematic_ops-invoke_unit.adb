------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--              SCHEMATIC OPERATIONS / INVOKING UNITS                       --
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

separate (et_schematic_ops)

procedure invoke_unit (
-- Invokes a unit of a device into the schematic.
	module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
	device_name		: in type_name; -- IC1
	unit_name		: in type_unit_name.bounded_string; -- A, B, IO_BANK_2
	place			: in et_coordinates.type_position; -- sheet/x/y/rotation
	log_threshold	: in type_log_level) is

	use et_coordinates;
	
	module_cursor : pac_generic_modules.cursor; -- points to the targeted module

	procedure query_devices (
		module_name	: in type_module_name.bounded_string;
		module		: in out type_module) is
		use et_schematic.type_devices;
		device_cursor_sch : et_schematic.type_devices.cursor;

		procedure query_units_in_use (
			device_name	: in type_name;
			device		: in et_schematic.type_device) is
			use et_schematic.type_units;
		begin
			if contains (device.units, unit_name) then
				log (ERROR, 
						to_string (device_name) &
						" unit " & to_string (unit_name) &
						" already in use !", console => true);
				raise constraint_error;
			end if;
		end query_units_in_use;

		device_model : type_device_model_file.bounded_string; -- ../libraries/devices/logic_ttl/7400.dev
		device_cursor_lib : et_devices.type_devices.cursor;
		unit_cursors : type_unit_cursors_lib;

		name	: et_symbols.type_text_placeholder (meaning => et_symbols.NAME);
		value	: et_symbols.type_text_placeholder (meaning => et_symbols.VALUE);
		purpose	: et_symbols.type_text_placeholder (meaning => et_symbols.PURPOSE);
			
		use pac_units_external;
		use pac_units_internal;
		use et_devices.type_devices;
		
		procedure add_unit_internal (
		-- Add an internal unit to the schematic device.
		-- The unit to be added is accessed by unit_cursors.int.
			device_name	: in type_name;
			device		: in out et_schematic.type_device) is
			use et_symbols;
		begin
			log (text => "invoking internal unit " & to_string (key (unit_cursors.int)), level => log_threshold + 2);
			
			case element (device_cursor_lib).appearance is
				when VIRTUAL =>
					type_units.insert (
						container	=> device.units,
						key			=> key (unit_cursors.int), -- the unit name like A, B
						new_item	=> (
							appearance	=> VIRTUAL,
							position	=> place, -- the coordinates provided by the calling unit (sheet,x,y,rotation)
							others 		=> <>)
							);
					
				when PCB =>

					-- rotate the placeholders according to rotation given by caller:
					name	:= element (unit_cursors.int).symbol.name;
					value	:= element (unit_cursors.int).symbol.value;
					purpose	:= element (unit_cursors.int).symbol.purpose;
					
					type_units.insert (
						container	=> device.units,
						key			=> key (unit_cursors.int), -- the unit name like A, B, VCC_IO_BANK_1
						new_item	=> (
							appearance	=> PCB,
							position	=> place, -- the coordinates provided by the calling unit (sheet,x,y,rotation)
							name		=> name,
							value		=> value,
							purpose		=> purpose,
							others 		=> <>)
							);
			end case;
			
		end add_unit_internal;

		procedure add_unit_external (
		-- Add an external unit to the schematic device.
		-- The unit to be added is accessed by unit_cursors.ext.
			device_name	: in type_name;
			device		: in out et_schematic.type_device) is
			use et_symbols;
			use et_symbols.type_symbols;
			symbol_cursor : type_symbols.cursor;
			symbol_file : type_symbol_model_file.bounded_string; -- *.sym
		begin
			log (text => "invoking external unit " & to_string (key (unit_cursors.ext)), level => log_threshold + 2);
			
			case element (device_cursor_lib).appearance is
				when VIRTUAL =>
					type_units.insert (
						container	=> device.units,
						key			=> key (unit_cursors.ext), -- the unit name like A, B
						new_item	=> (
							appearance	=> VIRTUAL,
							position	=> place, -- the coordinates provided by the calling unit (sheet,x,y,rotation)
							others 		=> <>)
							);
					
				when PCB =>
					-- The symbol file name is provided by unit_cursors.ext.
					symbol_file := element (unit_cursors.ext).file; -- *.sym
					
					-- Locate the external symbol in container "symbols".
					-- The key into symbols is the file name (*.sym).
					symbol_cursor := find (symbols, symbol_file);

					-- CS: The symbol should be there now. Otherwise symbol_cursor would assume no_element
					-- and constraint_error would arise here:

					-- rotate the placeholders according to rotation given by caller:
					name	:= element (symbol_cursor).name;
					value	:= element (symbol_cursor).value;
					purpose	:= element (symbol_cursor).purpose;

					rotate_by (name.position, rot (place));
					rotate_by (value.position, rot (place));
					rotate_by (purpose.position, rot (place));
					
					type_units.insert (
						container	=> device.units,
						key			=> key (unit_cursors.ext), -- the unit name like A, B, VCC_IO_BANK_1
						new_item	=> (
							appearance	=> PCB,
							position	=> place, -- the coordinates provided by the calling unit (sheet,x,y,rotation)
							name		=> name,
							value		=> value,
							purpose		=> purpose,
							others 		=> <>)
							);
			end case;

		end add_unit_external;

		ports : et_symbols.type_ports.map; -- the positions of the unit ports
		
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
			device_cursor_lib := et_devices.type_devices.find (et_devices.devices, device_model);

			-- Get cursor to the desired unit in device model.
			-- The unit can be internal or external.
			unit_cursors := any_unit (device_cursor_lib, unit_name);

			-- If the unit is internal, add it to the device in the schematic:
			if unit_cursors.int /= pac_units_internal.no_element then

				et_schematic.type_devices.update_element (
					container	=> module.devices,
					position	=> device_cursor_sch,
					process		=> add_unit_internal'access);

				-- fetch ports of unit and their positions relative to the unit origin
				log_indentation_up;				
				log (text => "fetching relative port positions of internal unit " &
					to_string (key (unit_cursors.int)) & " ...", level => log_threshold + 1);
				
				ports := ports_of_unit (
					device_cursor	=> device_cursor_sch,
					unit_name		=> key (unit_cursors.int));

			-- Unit is external -> add external unit to device in schematic:
			elsif unit_cursors.ext /= pac_units_external.no_element then
				
				et_schematic.type_devices.update_element (
					container	=> module.devices,
					position	=> device_cursor_sch,
					process		=> add_unit_external'access);

				-- fetch ports of unit and their positions relative to the unit origin
				log_indentation_up;
				log (text => "fetching relative port positions of external unit " &
					to_string (key (unit_cursors.ext)) & " ...", level => log_threshold + 1);

				ports := ports_of_unit (
					device_cursor	=> device_cursor_sch,
					unit_name		=> key (unit_cursors.ext));
				
			else
				raise constraint_error; -- CS should never happen. function any_unit excludes this case.
			end if;

			-- Calculate the absolute positions of the unit ports. Rotate first if required:
			log (text => "calculating absolute port positions ...", level => log_threshold + 1);
			if rot (place) /= zero_rotation then
				rotate_ports (ports, rot (place));
			end if;

			move_ports (ports, place);
			
			-- Insert the new unit ports in the nets (type_module.nets):
			insert_ports (
				module			=> module_cursor,
				device			=> device_name,
				unit			=> unit_name,
				ports			=> ports,
				sheet			=> et_coordinates.sheet (place),
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
		to_string (position => place) &
		" rotation" & to_string (rot (place)),
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

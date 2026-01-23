------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--              SCHEMATIC OPERATIONS / FETCHING UNITS                       --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2026                                                -- 
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

with et_symbol_library;
with et_symbol_name;
with et_device_appearance;				use et_device_appearance;
with et_device_placeholders.symbols;	use et_device_placeholders.symbols;


separate (et_schematic_ops_units)

procedure fetch_unit (
	module_cursor 	: in pac_generic_modules.cursor;
	device_name		: in type_device_name; -- IC1
	unit_name		: in pac_unit_name.bounded_string; -- A, B, IO_BANK_2
	destination		: in type_object_position; -- sheet/x/y/rotation
	log_threshold	: in type_log_level) 
is
		
	device_cursor_sch : pac_devices_electrical.cursor;

	
	procedure query_module (
		module_name	: in pac_module_name.bounded_string;
		module		: in out type_generic_module) 
	is
		-- CS:
		-- There is a lot of code almost the same as with
		-- copying and fetching devices and units. 
		-- See procedure et_schematic_ops-units-add_device
		-- and et_schematic_ops-units-copy_device.
		-- Move common stuff to a single procedure in et_schematic_ops-units.

		
		use et_device_appearance;
		use et_devices_electrical;
		use pac_devices_electrical;

		-- Query whehter the given unit is available:
		unit_query : constant type_unit_query := 
			get_unit_position (device_cursor_sch, unit_name);
		
		device_cursor_lib : pac_device_models.cursor;
		unit_cursors : type_device_units;

		
		use pac_units_external;
		use pac_units_internal;
		use pac_device_models;

		ports : pac_symbol_ports.map; -- the positions of the unit ports


		
		-- Adds an internal unit to the schematic device:
		procedure add_internal is

			-- The unit to be added is accessed by unit_cursors.int.
			procedure do_it (
				device_name	: in type_device_name;
				device		: in out type_device_electrical) 
			is
				placeholders : type_text_placeholders;
			begin
				log (text => "fetch internal unit " 
					 & to_string (key (unit_cursors.int)),
					 level => log_threshold + 2);

				-- Instanciate a symbol so that a unit comes into life:
				case element (device_cursor_lib).appearance is
					when APPEARANCE_VIRTUAL =>
						pac_units.insert (
							container	=> device.units,
							key			=> key (unit_cursors.int), -- the unit name like A, B
							new_item	=> (
								appearance	=> APPEARANCE_VIRTUAL,
								position	=> destination,
								others 		=> <>));
						
					when APPEARANCE_PCB =>
						-- Get the default positions and rotations of placeholders
						-- according to the rotation of the unit in the schematic:
						placeholders := get_default_placeholders (unit_cursors.int, destination);
						
						pac_units.insert (
							container	=> device.units,
							key			=> key (unit_cursors.int), -- the unit name like A, B, VCC_IO_BANK_1
							new_item	=> (
								appearance		=> APPEARANCE_PCB,
								position		=> destination,
								placeholders	=> placeholders,
								others 			=> <>));
				end case;				
			end do_it;

			
		begin
			pac_devices_electrical.update_element (
				container	=> module.devices,
				position	=> device_cursor_sch,
				process		=> do_it'access);

			-- fetch ports of unit and their positions relative to the unit origin
			log_indentation_up;				
			log (text => "fetch relative port positions of internal unit " &
				to_string (key (unit_cursors.int)), level => log_threshold + 1);
			
			ports := get_ports_of_unit (
				device_cursor	=> device_cursor_sch,
				unit_name		=> key (unit_cursors.int));

			log_indentation_down;
		end add_internal;




		
		-- Adds an external unit to the schematic device:
		procedure add_external is 

			-- The unit to be added is accessed by unit_cursors.ext.
			procedure do_it (
				device_name	: in type_device_name;
				device		: in out type_device_electrical) 
			is
				use et_symbol_name;
				use et_symbol_library;
				use pac_symbol_models;
				placeholders : type_text_placeholders;
				symbol_cursor : pac_symbol_models.cursor;
				symbol_file : pac_symbol_model_file.bounded_string; -- *.sym
			begin
				log (text => "fetch external unit " 
					 & to_string (key (unit_cursors.ext)), level => log_threshold + 2);

				-- Instanciate a symbol so that a unit comes into life:
				case element (device_cursor_lib).appearance is
					when APPEARANCE_VIRTUAL =>
						pac_units.insert (
							container	=> device.units,
							key			=> key (unit_cursors.ext), -- the unit name like A, B
							new_item	=> (
								appearance	=> APPEARANCE_VIRTUAL,
								position	=> destination,
								others 		=> <>));
						
					when APPEARANCE_PCB =>
						-- The symbol file name is provided by unit_cursors.ext.
						symbol_file := element (unit_cursors.ext).model; -- *.sym
						
						-- Locate the external symbol in the symbol library.
						-- The key into symbols is the file name (*.sym).
						symbol_cursor := find (symbol_library, symbol_file);

						-- CS: The symbol should be there now. Otherwise symbol_cursor would assume no_element
						-- and constraint_error would arise here:

						-- Get the default positions and rotations of placeholders
						-- according to the rotation of the unit in the schematic:
						placeholders := get_default_placeholders (symbol_cursor, destination);
						
						pac_units.insert (
							container	=> device.units,
							key			=> key (unit_cursors.ext), -- the unit name like A, B, VCC_IO_BANK_1
							new_item	=> (
								appearance		=> APPEARANCE_PCB,
								position		=> destination,
								placeholders	=> placeholders,
								others 			=> <>));
				end case;
			end do_it;

			
		begin			
			pac_devices_electrical.update_element (
				container	=> module.devices,
				position	=> device_cursor_sch,
				process		=> do_it'access);

			-- fetch ports of unit and their positions relative to the unit origin
			log_indentation_up;
			log (text => "fetch relative port positions of external unit " &
				to_string (key (unit_cursors.ext)), level => log_threshold + 1);

			ports := get_ports_of_unit (
				device_cursor	=> device_cursor_sch,
				unit_name		=> key (unit_cursors.ext));

			log_indentation_down;
		end add_external;


		

		procedure add_ports_to_nets is begin
			log (text => "insert_ports", level => log_threshold + 1);
			log_indentation_up;
			
			-- Calculate the absolute positions of the unit ports. Rotate first if required:
			-- log (text => "Calculate absolute port positions ...", level => log_threshold + 1);
			
			if get_rotation (destination) /= zero_rotation then
				rotate_ports (ports, get_rotation (destination));
			end if;

			move_ports (ports, destination);
			
			-- Insert the new unit ports in the nets of the module:
			insert_ports (
				module_cursor	=> module_cursor,
				device_name		=> device_name,
				unit_name		=> unit_name,
				ports			=> ports,
				sheet			=> get_sheet (destination),
				log_threshold	=> log_threshold + 2);

			log_indentation_down;
		end add_ports_to_nets;
		
		
	begin -- query_module

		-- Test whether desired unit is already used by 
		-- the device (in schematic). Abort if unit already deployed:
		if unit_query.exists then
			log (WARNING, "Unit " & to_string (unit_name) 
				& " is already deployed in the schematic at "
				& to_string (unit_query.position));
		else
			-- Locate the device model in the library:
			device_cursor_lib := get_device_model (device_cursor_sch);

			-- Get cursor to the desired unit in device model.
			-- The unit can be internal or external.
			unit_cursors := get_unit (device_cursor_lib, unit_name);

			-- If the unit is internal, then add it to the device in the schematic.
			-- If the unit is external, then add external unit to device in schematic:
			if unit_cursors.int /= pac_units_internal.no_element then
				add_internal;
				add_ports_to_nets;
				
			elsif unit_cursors.ext /= pac_units_external.no_element then
				add_external;
				add_ports_to_nets;
			else
				raise constraint_error; -- CS should never happen. 
					-- function any_unit excludes this case.
			end if;				
		end if;


	end query_module;

	
begin
	log (text => "module " & to_string (module_cursor) 
		& " device " & to_string (device_name) 
		& " fetch unit " & to_string (unit_name) & " and place at " 
		& to_string (destination),
		level => log_threshold);

	log_indentation_up;
	
	-- Locate the targeted device in the given module.
	-- If the device exists, then proceed with further actions.
	-- Otherwise abort this procedure with a warning:
	device_cursor_sch := get_electrical_device (module_cursor, device_name);
		
	if has_element (device_cursor_sch) then -- device exists in schematic
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);

	else
		log (WARNING, " Device " & to_string (device_name) & " not found !");
	end if;
	

	update_ratsnest (module_cursor, log_threshold + 1);
	
	log_indentation_down;		
	
end fetch_unit;


-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--              SCHEMATIC OPERATIONS / DEVICE / COPY DEVICE                 --
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
--   ToDo:
--		- Currently the positions of placeholders are not copied from the 
--		  source unit. Instead the positions as defined by the library symbol 
--		  are taken.
--		- To fix this issue copy_device must be provided with the name of the
--		  source unit. From the source unit the positions must then be copied
--		  to the new unit.


with et_symbol_ports;					use et_symbol_ports;
with et_symbol_model;
with et_symbol_name;
with et_symbol_library;
with et_device_model_unit_internal;		use et_device_model_unit_internal;
with et_device_model_unit_external;		use et_device_model_unit_external;
with et_device_model;					use et_device_model;
with et_device_placeholders.symbols;	use et_device_placeholders.symbols;
with et_device_appearance;				use et_device_appearance;
with et_device_library.packages;		use et_device_library.packages;
with et_device_library.units;			use et_device_library.units;
with et_board_ops_ratsnest;				use et_board_ops_ratsnest;


separate (et_schematic_ops_device)


procedure copy_device (
	module_cursor	: in pac_generic_modules.cursor;
	device_name		: in type_device_name; -- IC45
	destination		: in type_object_position; -- sheet/x/y/rotation
	log_threshold	: in type_log_level)
is
	-- The pointer to the device in the schematic:	
	device_cursor_sch : pac_devices_electrical.cursor;

	-- The pointer to the device model:		
	device_cursor_lib : pac_device_models.cursor;
		
	-- The next available device name:
	next_name : type_device_name;
	

	
	procedure query_module (
		module_name	: in pac_module_name.bounded_string;
		module		: in out type_generic_module) 
	is
		use pac_unit_name;
		
		-- CS:
		-- There is a lot of code almost the same as with
		-- copying and fetching devices and units. 
		-- See procedure et_schematic_ops-units-add_device
		-- and et_schematic_ops-units-fetch_device.
		-- Move common stuff to a single procedure in et_schematic_ops-units.
		
		
		-- This procedure creates a new device named after next_name.
		-- It copies properties from the original device.
		-- The new device is bare, means it has no units yet.
		-- Afterward the cursor device_cursor_sch will be pointing to
		-- the new device:
		procedure copy_bare_device is
			original : type_device_electrical renames element (device_cursor_sch);
			inserted : boolean;
		begin
			log (text => "copy_bare_device", level => log_threshold + 1);
			log_indentation_up;

			-- Create a new device. Copy properties from the original device.
			-- The unit list is empty for the time being:
			if is_real (original) then
				module.devices.insert (
					key			=> next_name,
					inserted	=> inserted,
					position	=> device_cursor_sch,
					new_item	=> (
						appearance 		=> APPEARANCE_PCB,
						model_cursor	=> device_cursor_lib,
						units			=> pac_units.empty_map,
						value			=> original.value,
						partcode		=> original.partcode,
						purpose			=> original.purpose,
						variant			=> original.variant,
						placeholders	=> original.placeholders, -- layout related
						others			=> <> )); 
						-- CS: The position in layout assumes default. Should be
						-- so that it is not placed on top of another device.

			else
				module.devices.insert (
					key			=> next_name,
					inserted	=> inserted,
					position	=> device_cursor_sch,
					new_item	=> (
						appearance 		=> APPEARANCE_VIRTUAL,
						model_cursor	=> device_cursor_lib,
						units			=> pac_units.empty_map));

			end if;

			-- CS test the inserted flag ?
			log_indentation_down;
		end copy_bare_device;


		
		-- When a device is added to the schematic, it is first
		-- added as a bare device without any units (see procedures above).
		--  The next step is to fetch the first available unit and add
		-- it to the bare device. Since the available unit can 
		-- be an external or inernal unit, the variable first_available_unit
		-- is a record that contains a cursor to an internal or
		-- external unit:
		first_available_unit : type_device_units;


		
		-- Add an internal unit to the schematic device.
		-- The unit to be added is accessed by first_available_unit.int.
		procedure add_unit_internal (
			device_name	: in type_device_name;
			device		: in out type_device_electrical) 
		is 

			-- This procedure composes the virtual unit and adds
			-- it to the schematic:
			procedure add_virtual is
				unit : type_unit (APPEARANCE_VIRTUAL);
			begin
				log (text => "add_virtual", level => log_threshold + 3);

				-- Compose a virtual unit:
				unit := (
					appearance	=> APPEARANCE_VIRTUAL,
					position	=> destination, -- the coordinates provided by the calling unit (sheet,x,y,rotation)
					others 		=> <>);

				-- Add the unit to the schematic:
				pac_units.insert (
					container	=> device.units,
					key			=> get_name_internal (first_available_unit), -- the unit name like A, B
					new_item	=> unit);

			end add_virtual;



			-- This procedure composes the real unit and adds
			-- it to the schematic:
			procedure add_real is
				unit : type_unit (APPEARANCE_PCB);
				placeholders : type_text_placeholders;
			begin
				log (text => "add_real", level => log_threshold + 3);

				-- Get the default placeholders as they are defined in the device model:
				placeholders := get_default_placeholders (first_available_unit.int, destination);
				
				-- Compose a real unit:
				unit := (
					appearance		=> APPEARANCE_PCB,
					position		=> destination, -- the coordinates provided by the calling unit (sheet,x,y,rotation)
					placeholders	=> placeholders,
					others 			=> <>);
							
				-- Add the unit to the schematic:
				pac_units.insert (
					container	=> device.units,
					key			=> get_name_internal (first_available_unit), -- the unit name like A, B, VCC_IO_BANK_1
					new_item	=> unit);

			end add_real;
			
			
		begin
			log (text => "add internal unit " 
				 & to_string (get_name_internal (
					first_available_unit)),
				 level => log_threshold + 2);
			
			log_indentation_up;
			
			if is_real (device_cursor_lib) then
				add_real;
			else
				add_virtual;
			end if;
			
			log_indentation_down;
		end add_unit_internal;



		
		-- Add an external unit to the schematic device.
		-- The unit to be added is accessed by unit_cursors.ext.
		procedure add_unit_external (
			device_name	: in type_device_name;
			device		: in out type_device_electrical) 
		is


			-- This procedure composes the virtual unit and adds
			-- it to the schematic:
			procedure add_virtual is 
				unit : type_unit (appearance => APPEARANCE_VIRTUAL);
			begin
				log (text => "add_virtual", level => log_threshold + 3);
				
				-- Compose a virtual unit:
				unit := (
					appearance	=> APPEARANCE_VIRTUAL,
					position	=> destination, -- the coordinates provided by the calling unit (sheet,x,y)
					others 		=> <>);

				-- Add the unit to the schematic:
				pac_units.insert (
					container	=> device.units,
					key			=> get_name_external (first_available_unit), -- the unit name like A, B
					new_item	=> unit);

			end add_virtual;


			-- This procedure composes the real unit and adds
			-- it to the schematic:
			procedure add_real is
				use et_symbol_library;
				symbol_cursor : pac_symbol_models.cursor;
				placeholders : type_text_placeholders;

				use pac_units_external;
				unit : type_unit (appearance => APPEARANCE_PCB);
			begin
				log (text => "add_real", level => log_threshold + 3);
				
				-- Map from the unit back to the symbol in the library:
				symbol_cursor := get_symbol (first_available_unit.ext);

				-- Get the default placeholders as they are defined in the device model:
				placeholders := get_default_placeholders (symbol_cursor, destination);

				-- Compose a real unit:
				unit := (
					appearance		=> APPEARANCE_PCB,
					position		=> destination, -- the coordinates provided by the calling unit (sheet,x,y)
					placeholders	=> placeholders,
					others 			=> <>);

				-- Add the unit to the schematic:
				pac_units.insert (
					container	=> device.units,
					key			=> get_name_external (first_available_unit), -- the unit name like A, B, VCC_IO_BANK_1
					new_item	=> unit);

			end add_real;
			
			
		begin
			log (text => "add external unit " 
				 & to_string (get_name_external (first_available_unit)),
				 level => log_threshold + 2);

			log_indentation_up;

			if is_real (device_cursor_lib) then
				add_real;
			else
				add_virtual;
			end if;

			log_indentation_down;
		end add_unit_external;

	

		

		-- Here we store temporarily the ports (with their positions)
		-- of the unit to be added:
		ports : pac_symbol_ports.map;
		
		-- This is the place where we temporarily keep the name
		-- of the unit to be added:
		unit_name : pac_unit_name.bounded_string;
		

		-- This procedure adds a unit to the new device. 
		-- 1. It searches first among the internal units and then among
		--    the external units that are provided by the device model.
		-- 2. It also loads the variable "ports" (see above) with the ports
		--    of the selected unit with the ports as they are defined in the 
		--    device model.
		-- 3. It loads the variable "unit_name" with the name of the selected unit.
		procedure add_unit is begin
			log (text => "add_unit", level => log_threshold + 1);
			log_indentation_up;
			
			-- Now we add the first available unit to the device in schematic.
			-- The order by which units are deployed is specified in 
			-- function get_first_unit:
			first_available_unit := get_first_unit (device_cursor_lib);

			-- If an internal unit is available, then add it to device. 
			-- If no internal unit is available but an external, then add it 
			-- to the device. So the operator will not take notice
			-- whether an internal or external unit is selected:
			if has_internal_unit (first_available_unit) then

				-- Add the internal unit to the device.
				-- NOTE: Cursor device_cursor_sch now points to the new
				-- device:
				update_element (
					container	=> module.devices,
					position	=> device_cursor_sch,
					process		=> add_unit_internal'access);

				-- Fetch the ports of the unit and their default positions 
				-- relative to the unit origin as they are defined in 
				-- the device model:
				unit_name := get_name_internal (first_available_unit);
				
				log (text => "fetch default port positions of internal unit " 
					 & to_string (unit_name), level => log_threshold + 2);
				
				ports := get_ports_of_unit (
					device_cursor	=> device_cursor_lib,
					unit_name		=> unit_name);


			-- If no internal unit is available -> add external unit:
			elsif has_external_unit (first_available_unit) then

				-- Add the external unit to the device.
				-- NOTE: Cursor device_cursor_sch now points to the new
				-- device:
				update_element (
					container	=> module.devices,
					position	=> device_cursor_sch,
					process		=> add_unit_external'access);

				-- Fetch the ports of the unit and their default positions 
				-- relative to the unit origin as they are defined in 
				-- the device model:
				unit_name := get_name_external (first_available_unit);
				
				log (text => "fetch default port positions of external unit " 
					 & to_string (unit_name), level => log_threshold + 2);

				ports := get_ports_of_unit (
					device_cursor	=> device_cursor_lib,
					unit_name		=> unit_name);

				
			else
				raise constraint_error; -- CS should never happen. function first_unit excludes this case.
			end if;

			log_indentation_down;
		end add_unit;


		
		-- The ports of the unit are stored in "ports". But they are
		-- still on their default positions as defined in the device model.
		-- This procedure rotates and moves the ports according to
		-- the rotation and destination of the unit:
		procedure add_ports_to_nets is begin
			log (text => "insert_ports", level => log_threshold + 1);
			log_indentation_up;

			-- Rotate the ports about the origin of the unit;
			rotate_ports (ports, get_rotation (destination));

			-- Move the ports to the final destination of the unit:
			move_ports (ports, destination);

			-- Insert the new unit ports in the net segments:
			insert_ports (
				module_cursor	=> module_cursor,
				device_name		=> next_name,
				unit_name		=> unit_name,
				ports			=> ports,
				sheet			=> get_sheet (destination),
				log_threshold	=> log_threshold + 2);


			-- Update the ratsnest if the added device is real:
			if is_real (device_cursor_lib) then
				update_ratsnest (module_cursor, log_threshold + 1);
			end if;
			
			log_indentation_down;			
		end add_ports_to_nets;
		
		
		
	begin		
		-- First we add a bare device to the module:
		copy_bare_device;

		-- Now we add the first available unit:
		add_unit;

		-- Insert the ports of the unit in the net segments
		add_ports_to_nets;
	end query_module;

	
	
	
begin
	log (text => "module " & to_string (module_cursor) 
		& " copy " & to_string (device_name) 
		& " to " & to_string (destination),
		 level => log_threshold);

	log_indentation_up;


	-- Locate the targeted device in the given module.
	-- If the device exists, then proceed with further actions.
	-- Otherwise abort this procedure with a warning:
	device_cursor_sch := get_electrical_device (module_cursor, device_name);
		
	if has_element (device_cursor_sch) then -- device exists in schematic

		-- Locate the device model:
		device_cursor_lib := get_device_model (device_cursor_sch);
		
		-- Build the next available device name:
		next_name := get_next_available_device_name (
			module_cursor, get_prefix (device_name), log_threshold); -- IC46

		log (text => "auto generated next device name: " & to_string (next_name),
			level => log_threshold);

		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);

	else
		log (WARNING, " Device " & to_string (device_name) & " not found !");
	end if;
	
		
	log_indentation_down;
	
end copy_device;


-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

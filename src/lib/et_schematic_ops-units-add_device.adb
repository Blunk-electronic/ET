------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--              SCHEMATIC OPERATIONS / ADDING DEVICES                       --
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
--  To Do:
--  1. The packages of new devices shall be placed in the board
--     next to each other. Currently they are placed on top of each
--     other.
--

with et_symbol_library;
with et_device_rw;
with et_device_placeholders.symbols;	use et_device_placeholders.symbols;
with et_device_model;					use et_device_model;
with et_device_appearance;				use et_device_appearance;


separate (et_schematic_ops.units)

procedure add_device (
	module_cursor	: in pac_generic_modules.cursor;
	device_model	: in pac_device_model_file.bounded_string;
	variant			: in pac_package_variant_name.bounded_string;
	destination		: in type_object_position;
	log_threshold	: in type_log_level) 
is	
	use pac_package_variant_name;
	
	use pac_devices_lib;
	device_cursor_lib : pac_devices_lib.cursor; -- points to the device in the library

	-- The next available device name:
	next_name : type_device_name;

	
	
	procedure query_module (
		module_name	: in pac_module_name.bounded_string;
		module		: in out type_generic_module) 
	is
		use pac_devices_sch;
		device_cursor_sch : pac_devices_sch.cursor;
		

		-- Adds a virtual device to the devices
		-- in the drawing. After successful inserting, the cursor
		-- device_cursor_sch points to the device in the schematic.
		-- For the moment, no units are added:
		procedure add_virtual_device is 
			inserted : boolean;
			device : type_device_sch (APPEARANCE_VIRTUAL);
		begin
			log (text => "add_virtual_device", level => log_threshold + 2);
			log_indentation_up;

			-- Compose the virtual device:
			device := (
				appearance 	=> APPEARANCE_VIRTUAL,
				model		=> get_device_model_file (device_cursor_lib),
				units		=> pac_units.empty_map); -- no units yet

			-- Insert the device in the schematic:
			pac_devices_sch.insert (
				container	=> module.devices,
				inserted	=> inserted,
				position	=> device_cursor_sch,
				key			=> next_name,
				new_item	=> device);

			-- CS test inserted flag ?
			log_indentation_down;
		end add_virtual_device;


		
		
		-- Adds a real device to the devices
		-- in the drawing. After successful inserting, the cursor
		-- device_cursor_sch points to the device in the schematic.
		-- For the moment, no units are added:
		procedure add_real_device is 
			selected_variant : pac_package_variant_name.bounded_string;

			
			-- Compose and insert the device in the schematic.
			-- For the moment the device has no units yet:
			procedure insert_device is 
				device : type_device_sch (appearance => APPEARANCE_PCB);
				inserted : boolean;
			begin
				log (text => "insert_device", level => log_threshold + 3);
				log_indentation_up;
				
				device := (
					appearance 	=> APPEARANCE_PCB,
					model 		=> get_device_model_file (device_cursor_lib),
					units		=> pac_units.empty_map, -- no units yet
					value		=> get_default_value (device_cursor_lib), -- if predefined in dev. model
					variant		=> selected_variant,

					-- Initially, the text placeholders are copies of 
					-- the placeholders as they are defined in the package model.
					-- Extract them from the device model and the package variant:
					text_placeholders	=> get_package_placeholders (device_cursor_lib, selected_variant),

					-- Use default position of the device in the layout.
					-- CS: do not place the package on top of others
					others		=> <> );

				
				-- Insert the device in the schematic:
				pac_devices_sch.insert (
					container	=> module.devices,
					inserted	=> inserted,
					position	=> device_cursor_sch,
					key			=> next_name,
					new_item	=> device);

				-- CS test the inserted flag ?
				log_indentation_down;
			end insert_device;
		
			
		begin
			log (text => "add_real_device", level => log_threshold + 2);
			log_indentation_up;
			
			-- A real device requires a package variant.

			-- If a package variant was specified, then we must
			-- make sure that the requested variant exists in the model at all.
			-- If it exists, then the device is added to the schematic
			-- with the package variant requested by the caller.
			-- If the requested package variant does not exist in the model,
			-- then an error messages is output and nothing else happens.
			-- If no package variant was given by the caller, then the first
			-- available variant will be applied:
			
			if not is_empty (variant) then
				-- A variant was given by the caller.

				if is_variant_available (device_cursor_lib, variant) then
					-- The variant exists.

					selected_variant := variant;
					insert_device;
										
				else -- The variant does not exist.
					log (ERROR, "Package variant " & to_string (variant)
						 & " does not exist in the specified device model !");
				end if;

				
			else 
				-- No package variant was specified by the caller.
				-- Select the first available variant:
				selected_variant := get_first_package_variant (device_cursor_lib);
				
				log (WARNING, "No variant specified !"
					 & " Default to first available variant " & to_string (selected_variant));
				
				insert_device;
			end if;

			log_indentation_down;
		end add_real_device;


		
		
		-- Adds a bare device (without any units deployed)
		-- to the module:
		procedure add_bare_device is begin
			log (text => "add_bare_device", level => log_threshold + 1);
			log_indentation_up;
			
			-- As the first step we add a bare device to the module.
			-- For the moment no units are deployed:
			if is_real (device_cursor_lib) then
				add_real_device;
			else
				add_virtual_device;				
			end if;

			log_indentation_down;
		end add_bare_device;
		


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
			device		: in out type_device_sch) 
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
				placeholders : type_default_placeholders;
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
				 & to_string (get_name_internal (first_available_unit)),
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
		-- The unit to be added is accessed by first_available_unit.ext.
		procedure add_unit_external (
			device_name	: in type_device_name;
			device		: in out type_device_sch) 
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
				symbol_cursor : pac_symbols.cursor;
				placeholders : type_default_placeholders;

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


		
		
		-- This procedure adds a unit to the device. 
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

				-- Add the internal unit to the device:
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

				-- Add the external unit to the device:
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
		add_bare_device;

		-- Now we add the first available unit:
		add_unit;

		-- Insert the ports of the unit in the net segments
		add_ports_to_nets;
	end query_module;

	
	
begin -- add_device
	if not is_empty (variant) then -- real device
		log (text => "module " & to_string (module_cursor) 
			 & " add real device " & to_string (device_model) 
			 & " package variant " & to_string (variant) 
			 & " at " & to_string (destination),
			level => log_threshold);
		
	else -- virtual device
		log (text => "module " & to_string (module_cursor) 
			 & " add virtual device " & to_string (device_model) 
			 & " at " & to_string (destination),
			level => log_threshold);
	end if;
		
	log_indentation_up;
	
	-- Read the device file and store it in the rig wide device library.
	-- If the device is already in the library, nothing happpens.
	et_device_rw.read_device (
		file_name		=> device_model, -- ../lbr/logic_ttl/7400.dev
		log_threshold	=> log_threshold + 1);

	-- CS add error flag output by read_device and evaluate accordingly.
	-- Wrap follwing actions in a procedure.
	-- CS use device cursor output by read_device instead 
	-- the following statement.
	
	-- locate the device in the library
	device_cursor_lib := find (device_library, device_model);


	-- Build the next available device name:
	next_name := get_next_device_name (module_cursor, get_prefix (device_cursor_lib));

	log (text => "auto generated device name: " & to_string (next_name),
		 level => log_threshold);
	
	
	update_element (
		container	=> generic_modules,
		position	=> module_cursor,
		process		=> query_module'access);
	
	log_indentation_down;

end add_device;


-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

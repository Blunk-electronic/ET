------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--              SCHEMATIC OPERATIONS / ADDING DEVICES                       --
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

procedure add_device (
	module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
	device_model	: in type_device_model_file.bounded_string; -- ../libraries/devices/logic_ttl/7400.dev
	variant			: in et_devices.type_variant_name.bounded_string; -- N, D, S_0805
	destination		: in et_coordinates.type_position; -- sheet/x/y,rotation
	log_threshold	: in type_log_level) is
	
	use et_coordinates;
	
	module_cursor : pac_generic_modules.cursor; -- points to the targeted module

	use et_devices.type_devices;
	device_cursor_lib : et_devices.type_devices.cursor; -- points to the device in the library

	use et_symbols;
	
	procedure add (
		module_name	: in type_module_name.bounded_string;
		module		: in out type_module) is
		use et_schematic.type_devices;
		device_cursor_sch : et_schematic.type_devices.cursor;
		inserted : boolean;

		-- build the next available device name:
		next_name : type_name := 
			next_device_name (module_cursor, element (device_cursor_lib).prefix);

		placeholders : type_rotated_placeholders;
		
		unit_cursors : type_unit_cursors_lib;

		use pac_units_internal;
		use pac_units_external;
		
		procedure add_unit_internal (
		-- Add an internal unit to the schematic device.
		-- The unit to be added is accessed by unit_cursors.int.
			device_name	: in type_name;
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
							others 		=> <>) -- mirror
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
			device_name	: in type_name;
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
							position	=> destination, -- the coordinates provided by the calling unit (sheet,x,y)
							others 		=> <>) -- mirror
							);
					
				when PCB =>
					-- The symbol file name is provided by unit_cursors.ext.
					symbol_file := element (unit_cursors.ext).file; -- *.sym
					
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
							position	=> destination, -- the coordinates provided by the calling unit (sheet,x,y)
							name		=> placeholders.name,	
							value		=> placeholders.value,	
							purpose		=> placeholders.purpose,	
							others 		=> <>) -- mirror
							);

			end case;

		end add_unit_external;

		ports : et_symbols.type_ports.map;
		unit_name : et_devices.type_unit_name.bounded_string;
		
	begin -- add
		log (text => "adding device " & to_string (next_name), level => log_threshold + 1);
		log_indentation_up;
		
		case element (device_cursor_lib).appearance is
			when VIRTUAL =>
				et_schematic.type_devices.insert (
					container	=> module.devices,
					inserted	=> inserted,
					position	=> device_cursor_sch,
					key			=> next_name,
					new_item	=> (
						appearance 	=> VIRTUAL,
						model		=> key (device_cursor_lib),
						units		=> type_units.empty_map
						));

				-- CS check inserted flag ?
				
			when PCB =>
				-- A real device requires a package variant.
				if et_devices.type_variant_name.length (variant) > 0 then

					if variant_available (device_cursor_lib, variant) then
						et_schematic.type_devices.insert (
							container	=> module.devices,
							inserted	=> inserted,
							position	=> device_cursor_sch,
							key			=> next_name,
							new_item	=> (
								appearance 	=> PCB,
								model		=> key (device_cursor_lib),
								units		=> type_units.empty_map,
								value		=> element (device_cursor_lib).value, -- if predefined in dev. model
								variant		=> variant,

								-- Initially, the text placeholders are copies of the placeholders 
								-- defined in the package.
								-- Extract them from the device model and the variant:
								text_placeholders	=> placeholders_of_package (device_cursor_lib, variant),
								
								others		=> <>
								));

						-- CS check inserted flag ?
						
					else -- variant not available
						log (ERROR, "package variant " & enclose_in_quotes (to_string (variant)) &
								" not available in the specified device model !", console => true);
						raise constraint_error;
					end if;
					
				else -- no variant specified
					log (ERROR, "device requires specification of package variant !",
							console => true);
					raise constraint_error;
				end if;
				
		end case;

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
	end add;
		
begin -- add_device
	if et_devices.type_variant_name.length (variant) > 0 then -- real device
		log (text => "module " & to_string (module_name) &
			" adding device " & to_string (device_model) &
			" package variant " & to_string (variant) &
			" at" &
			to_string (position => destination) &
			" rotation" & to_string (rot (destination)),
			level => log_threshold);
		
	else -- virtual device
		log (text => "module " & to_string (module_name) &
			" adding device " & to_string (device_model) &
			" at" &
			to_string (position => destination) &
			" rotation" & to_string (rot (destination)),
			level => log_threshold);
	end if;
		
	log_indentation_up;
	
	-- locate module
	module_cursor := locate_module (module_name);

	-- Read the device file and store it in the rig wide device 
	-- library et_devices.devices.
	-- If the device is already in the library, nothing happpens.
	device_rw.read_device (
		file_name		=> device_model, -- ../lbr/logic_ttl/7400.dev
		log_threshold	=> log_threshold + 1);

	-- locate the device in the library
	device_cursor_lib := find (et_devices.devices, device_model);
	
	update_element (
		container	=> generic_modules,
		position	=> module_cursor,
		process		=> add'access);

	log_indentation_down;

end add_device;


-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

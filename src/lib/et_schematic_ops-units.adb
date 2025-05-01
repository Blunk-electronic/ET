------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                    SCHEMATIC OPERATIONS ON UNITS                         --
--                                                                          --
--                               S p e c                                    --
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

--   For correct displaying set tab width in your editor to 4.

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


with et_board_ops.ratsnest;					use et_board_ops.ratsnest;
with et_port_direction;
with et_device_model;						use et_device_model;
with et_numbering;
with et_device_appearance;


package body et_schematic_ops.units is

	use pac_devices_sch;
	use pac_units;
	
	use pac_unit_name;
	use pac_text_schematic;
	

	function get_position (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC34
		port_name		: in pac_port_name.bounded_string; -- CE
		log_threshold	: in type_log_level)
		return type_object_position 
	is
		port_position : type_object_position; -- to be returned		
		
		module_cursor : pac_generic_modules.cursor; -- points to the module being inquired

		
		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			device_cursor : pac_devices_sch.cursor;
			unit_position : type_object_position;

			
			procedure query_units (
				device_name	: in type_device_name;
				device		: in type_device_sch) 
			is				
				unit_cursor : pac_units.cursor := device.units.first;
				unit_name : pac_unit_name.bounded_string;
				
				use pac_ports;
				ports : pac_ports.map;
				port_cursor : pac_ports.cursor;
			begin
				-- Locate unit in schematic device:
				while unit_cursor /= pac_units.no_element loop

					-- Load the default xy-positions of ports relative to the center of the unit.
					unit_name := key (unit_cursor);
					ports := get_ports_of_unit (device_cursor, unit_name);

					-- If the unit has a port named port_name: 
					if contains (ports, port_name) then -- port found
						
						-- calculate the port position in the schematic
						unit_position := element (unit_cursor).position; -- unit pos. in schematic

						port_cursor := find (ports, port_name);
						port_position := et_schematic_coordinates.to_position (
							sheet	=> get_sheet (unit_position), -- the sheet where the unit is
							point	=> element (port_cursor).position -- default xy pos of port
							);														 

						-- Calculate the absolute port position in schematic by
						-- first rotating port_xy, and then moving port_xy:
						
						rotate_by (
							point		=> port_position.place,
							rotation	=> get_rotation (element (unit_cursor).position));
						
						-- CS mirror ?
						
						-- Calculate the absolute port position in the schematic:
						move_by (
							point 	=> port_position.place,
							offset	=> to_distance_relative (unit_position.place));
						
						exit; -- no need to look at other units
					end if;
					
					next (unit_cursor);
				end loop;
			end query_units;

			
		begin -- query_devices
			if contains (module.devices, device_name) then
				device_cursor := find (module.devices, device_name); -- the device should be there

				log_indentation_up;
				
				pac_devices_sch.query_element (
					position	=> device_cursor,
					process		=> query_units'access);

				log_indentation_down;				
			else
				device_not_found (device_name);
			end if;
		end query_devices;

		
	begin
		log (text => "module " & to_string (module_name) &
			 " locating device " & to_string (device_name) & 
			 " port " & to_string (port_name) & " ...", level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);
		
		query_element (
			position	=> module_cursor,
			process		=> query_devices'access);
		
		return port_position;
	end get_position;

	
	


	
	procedure move_unit_placeholder (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC45
		unit_name		: in pac_unit_name.bounded_string; -- A
		coordinates		: in type_coordinates; -- relative/absolute
		point			: in type_vector_model; -- x/y
		meaning			: in type_placeholder_meaning; -- name, value, purpose
		log_threshold	: in type_log_level)
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		use pac_unit_name;

		
		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			device_cursor : pac_devices_sch.cursor;

			procedure query_units (
				device_name	: in type_device_name;
				device		: in out type_device_sch)
			is
				unit_cursor : pac_units.cursor;

				procedure move_placeholder (
					unit_name	: in pac_unit_name.bounded_string;
					unit		: in out type_unit)
				is
					-- In case absolute movement is required, calculate the
					-- new position of the placeholder relative to the unit origin:
					pos_abs : constant type_vector_model :=
						to_point (get_distance_relative (unit.position.place, point));
					
				begin -- move_placeholder
					
					-- The given meaning determines the placeholder to be moved:
					case meaning is
						when NAME =>
							case coordinates is
								when ABSOLUTE =>
									--log (text => "pos " & to_string (point));
									unit.name.position := pos_abs;

								when RELATIVE =>
									move_by (
										point	=> unit.name.position,
										offset	=> to_distance_relative (point));
							end case;
							
						when VALUE =>
							case coordinates is
								when ABSOLUTE =>
									unit.value.position := pos_abs;

								when RELATIVE =>
									move_by (
										point	=> unit.value.position,
										offset	=> to_distance_relative (point));
							end case;
							
						when PURPOSE =>
							case coordinates is
								when ABSOLUTE =>
									unit.purpose.position := pos_abs;

								when RELATIVE =>
									move_by (
										point	=> unit.purpose.position,
										offset	=> to_distance_relative (point));
							end case;

						when others =>
							raise constraint_error; -- CS no longer required
					end case;
					
					exception
						when event: others =>
							log (ERROR, "coordinates invalid !", console => true); -- CS required more details
							log (text => ada.exceptions.exception_information (event), console => true);
							raise;
					
				end move_placeholder;

				
			begin -- query_units
				if contains (device.units, unit_name) then

					-- locate unit by its name. it should be there.
					unit_cursor := find (device.units, unit_name);

					update_element (
						container	=> device.units,
						position	=> unit_cursor,
						process		=> move_placeholder'access);
					
				else
					unit_not_found (unit_name);
				end if;
			end query_units;

			
		begin -- query_devices
			if contains (module.devices, device_name) then

				-- Locate the device. It should be there.
				device_cursor := find (module.devices, device_name);

				-- locate the unit
				update_element (
					container	=> module.devices,
					position	=> device_cursor,
					process		=> query_units'access);
				
			else
				device_not_found (device_name);
			end if;
		end query_devices;

		
	begin -- move_unit_placeholder
		case coordinates is
			when ABSOLUTE =>
				log (text => "module " & enclose_in_quotes (to_string (module_name))
					& " moving " & to_string (device_name) 
					& " unit " & to_string (unit_name) 
					& " placeholder " & enclose_in_quotes (to_string (meaning))
					& " to" & to_string (point),
					level => log_threshold);

			when RELATIVE =>
				log (text => "module " & enclose_in_quotes (to_string (module_name))
					& " moving " & to_string (device_name) 
					& " unit " & to_string (unit_name) 
					& " placeholder " & enclose_in_quotes (to_string (meaning))
					& " by" & to_string (point),
					level => log_threshold);
		end case;

		-- locate module
		module_cursor := locate_module (module_name);
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_devices'access);
		
	end move_unit_placeholder;



	

	function get_default_text_positions (
		device_cursor	: in pac_devices_sch.cursor;
		unit_name		: in pac_unit_name.bounded_string)
		return et_symbols.type_default_text_positions 
	is		
		use et_symbols;
		use et_device_appearance;
		use pac_devices_lib;

		-- The positions to be returned depend on the appearance of the requested device:
		result : type_default_text_positions (element (device_cursor).appearance); -- to be returned
		
		model : pac_device_model_file.bounded_string; -- ../libraries/devices/transistor/pnp.dev
		device_cursor_lib : pac_devices_lib.cursor;
		
		use et_symbols.pac_texts;

		
		procedure query_text (c : in et_symbols.pac_texts.cursor) is 
		-- Appends a text position (x/y) the the result.
			use pac_text_positions;
		begin
			append (result.texts, element (c).position);
		end;

		
		-- Indicates whether the unit is internal or external:
		unit_status : type_unit_ext_int := EXT;

		
		procedure query_internal_units (
			model	: in pac_device_model_file.bounded_string;
			device	: in type_device_model) 
		is
			use pac_units_internal;			
			unit_cursor : pac_units_internal.cursor;
		begin
			-- locate the given unit among the internal units
			unit_cursor := find (device.units_internal, unit_name);

			-- if the unit exists among the internal units:
			if unit_cursor /= pac_units_internal.no_element then
				unit_status := INT;
				
				-- Collect the positions of texts and store them in result.text
				-- in the same order as they are listed in symbol.texts:
				iterate (element (unit_cursor).symbol.texts, query_text'access);
				-- CS: constraint_error arises here if unit can not be located.

				-- If it is about a real device, take a copy of the default 
				-- placeholders as they are specified in the symbol model:
				case result.appearance is
					when APPEARANCE_PCB =>
						result.name 	:= element (unit_cursor).symbol.name;
						result.value	:= element (unit_cursor).symbol.value;
						result.purpose	:= element (unit_cursor).symbol.purpose;
					when others => null;
				end case;

			else
				unit_status := EXT;
			end if;
		end query_internal_units;

		
		procedure query_external_units (
			model	: in pac_device_model_file.bounded_string;
			device	: in type_device_model) 
		is
			use pac_units_external;
			unit_cursor : pac_units_external.cursor;
			sym_model : pac_symbol_model_file.bounded_string; -- like /libraries/symbols/NAND.sym

			
			procedure query_symbol (
				symbol_name	: in pac_symbol_model_file.bounded_string;
				symbol		: in type_symbol ) 
			is begin
				-- Collect the positions of texts and store them in result.text
				-- in the same order as they are listed in symbol.texts:
				iterate (symbol.texts, query_text'access);

				-- If it is about a real device, take a copy of the default 
				-- placeholders as they are specified in the symbol model:
				case result.appearance is
					when APPEARANCE_PCB =>
						result.name 	:= symbol.name;
						result.value	:= symbol.value;
						result.purpose	:= symbol.purpose;
					when others => null;
				end case;
			end query_symbol;

			
		begin -- query_external_units
			-- locate the given unit among the external units
			unit_cursor := find (device.units_external, unit_name);

			-- Fetch the symbol model file of the external unit.
			-- If unit could not be located then it must be internal.
			if unit_cursor /= pac_units_external.no_element then
				unit_status := EXT;
				
				sym_model := element (unit_cursor).model;

				-- Fetch the ports of the external unit.
				-- CS: constraint_error arises here if symbol model could not be located.
				pac_symbols.query_element (
					position	=> pac_symbols.find (symbols, sym_model),
					process		=> query_symbol'access);
			else
				unit_status := INT;
			end if;
			
		end query_external_units;

		
	begin

		-- Fetch the model name of the given device. 
		model := pac_devices_sch.element (device_cursor).model;

		-- Get cursor to device in device library (the model name is the key into the device library).
		-- CS: constraint_error will arise here if no associated device exists.
		device_cursor_lib := find (device_library, model);

		-- Query external units of device (in library). It is most likely that
		-- the unit is among the external units:
		query_element (
			position	=> device_cursor_lib,
			process		=> query_external_units'access);

		-- If unit could not be found among external units then look up the internal units:
		if unit_status = INT then

			-- Query internal units of device (in library):
			query_element (
				position	=> device_cursor_lib,
				process		=> query_internal_units'access);
		end if;
		
		-- CS raise error if unit could not be located at all.
			
		return result;

		exception
			when event: others =>
				log_indentation_reset;
				log (text => ada.exceptions.exception_information (event), console => true);
				raise;
		
	end get_default_text_positions;




	

	procedure rotate_unit_placeholder (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC45
		unit_name		: in pac_unit_name.bounded_string; -- A
		rotation		: in et_text.type_rotation_documentation; -- absolute ! -- 90
		meaning			: in type_placeholder_meaning; -- name, value, purpose		
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		use et_symbols;
		use pac_unit_name;

		
		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			device_cursor : pac_devices_sch.cursor;

			procedure query_units (
				device_name	: in type_device_name;
				device		: in out type_device_sch) 
			is
				unit_cursor : pac_units.cursor;

				
				procedure rotate_placeholder (
					name	: in pac_unit_name.bounded_string; -- A
					unit	: in out type_unit) 
				is begin
					case meaning is
						when et_device_placeholders.NAME =>
							unit.name.rotation := rotation;
							
						when VALUE =>
							unit.value.rotation := rotation;
							
						when PURPOSE =>
							unit.purpose.rotation := rotation;

					end case;
				end rotate_placeholder;

				
			begin -- query_units
				if contains (device.units, unit_name) then

					-- locate unit by its name
					unit_cursor := find (device.units, unit_name);

					pac_units.update_element (
						container	=> device.units,
						position	=> unit_cursor,
						process		=> rotate_placeholder'access);
				else
					unit_not_found (unit_name);
				end if;
			end query_units;

			
		begin -- query_devices
			if contains (module.devices, device_name) then

				-- locate the device. it should be there
				device_cursor := find (module.devices, device_name);

				update_element (
					container	=> module.devices,
					position	=> device_cursor,
					process		=> query_units'access);
				
			else
				device_not_found (device_name);
			end if;
		end query_devices;
		
		
	begin
		log (text => "module " & to_string (module_name) &
			" rotating " & to_string (device_name) & " unit " &
			to_string (unit_name) & " placeholder" & to_string (meaning) & " to" &
			to_string (rotation), level => log_threshold);
		
		-- locate module
		module_cursor := locate_module (module_name);
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_devices'access);

	end rotate_unit_placeholder;




	
	
	function locate_unit (
		module	: in pac_generic_modules.cursor;
		device	: in type_device_name; -- R2
		unit	: in pac_unit_name.bounded_string)
		return pac_units.cursor
	is
		device_cursor : pac_devices_sch.cursor;
		unit_cursor : pac_units.cursor; -- to be returned

		procedure query_units (
			device_name	: in type_device_name; -- R2
			device		: in type_device_sch)
		is begin
			unit_cursor := find (device.units, unit);
		end query_units;
	
	begin -- locate_unit
		device_cursor := locate_device (module, device);

		-- locate the unit
		query_element (device_cursor, query_units'access);

		return unit_cursor;
	end locate_unit;


	

	function is_deployed (
		module	: in pac_generic_modules.cursor;
		device	: in type_device_name; -- R2
		unit	: in pac_unit_name.bounded_string)
		return boolean
	is
		unit_cursor : pac_units.cursor;
	begin
		unit_cursor := locate_unit (module, device, unit);

		if unit_cursor = pac_units.no_element then
			return false;
		else
			return true;
		end if;
	end is_deployed;

	
	

	function device_port_exists (
		module_cursor	: in pac_generic_modules.cursor; -- motor_driver
		device_name		: in type_device_name; -- IC45
		port_name		: in pac_port_name.bounded_string) -- CE
		return boolean 
	is
		result : boolean := false; -- to be returned. goes true once the target has been found

		
		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module)
		is
			use pac_unit_name;
			device_cursor : pac_devices_sch.cursor;

			
			procedure query_units (
				device_name	: in type_device_name;
				device		: in type_device_sch) 
			is
				unit_cursor : pac_units.cursor := device.units.first;
				use pac_ports;
				ports : pac_ports.map;
				use pac_port_name;
			begin
				while unit_cursor /= pac_units.no_element loop
					--log (text => "unit " & pac_unit_name.to_string (key (unit_cursor)));
					--log (text => "port " & pac_port_name.to_string (port_name));
					
					-- fetch the unit ports from the library model
					ports := get_ports_of_unit (device_cursor, key (unit_cursor));

					-- if the unit has a port named port_name then we have
					-- a match. no further search required.
					if contains (ports, port_name) then
						result := true;
						exit;
					end if;
										
					next (unit_cursor);
				end loop;
			end query_units;

			
		begin
			if contains (module.devices, device_name) then -- device found

				device_cursor := find (module.devices, device_name);
					
				query_element (
					position	=> device_cursor,
					process		=> query_units'access);

			end if;
		end query_devices;

		
	begin
		query_element (
			position	=> module_cursor,
			process		=> query_devices'access);

		return result;
	end device_port_exists;




	

	function device_unit_port_exists (
		module_cursor	: in pac_generic_modules.cursor; -- motor_driver
		device_name		: in type_device_name; -- IC45
		unit_name		: in pac_unit_name.bounded_string := to_unit_name (""); -- A
		port_name		: in pac_port_name.bounded_string := to_port_name ("")) -- CE
		return boolean 
	is
		result : boolean := false; -- to be returned, goes true once the target has been found
		
		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) is
			use pac_unit_name;
			device_cursor : pac_devices_sch.cursor;

			use et_symbols;
			
			procedure query_units (
				device_name	: in type_device_name;
				device		: in type_device_sch) is
				use pac_ports;
				ports : pac_ports.map;
				use pac_port_name;
			begin
				if contains (device.units, unit_name) then
					if length (port_name) > 0 then -- search for port in unit

						-- fetch the unit ports from the library model
						ports := get_ports_of_unit (device_cursor, unit_name);

						if contains (ports, port_name) then
							result := true;
						end if;
						
					else
						result := true;
					end if;
				end if;
			end query_units;

			
		begin -- query_devices
			if contains (module.devices, device_name) then -- device found

				-- If unit name given, search for the unit.
				if length (unit_name) > 0 then
					device_cursor := find (module.devices, device_name);
					
					query_element (
						position	=> device_cursor,
						process		=> query_units'access);

				else
					result := true;
				end if;
				
			end if;
		end query_devices;

		
	begin
		query_element (
			position	=> module_cursor,
			process		=> query_devices'access);

		return result;
	end device_unit_port_exists;


	

	
	function get_available_units (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- IC1
		log_threshold	: in type_log_level)
		return pac_unit_names.list
	is
		device_cursor_sch : pac_devices_sch.cursor;

		device_model : pac_device_model_file.bounded_string;
		device_cursor_lib : pac_devices_lib.cursor;

		use pac_unit_names;
		all_unit_names : pac_unit_names.list;
		names_of_available_units : pac_unit_names.list;

		
		procedure query_in_use (c : in pac_unit_names.cursor) is 
			use pac_unit_name;
			in_use : boolean := false;

			-- Sets the in_use flag if given unit is already in use:
			procedure query_in_use (
				device_name	: in type_device_name;
				device		: in type_device_sch) 
			is
			begin
				if contains (device.units, element (c)) then
					in_use := true;
				end if;
			end query_in_use;

			
		begin
			-- Test whether the unit is already in use.
			query_element (
				position	=> device_cursor_sch,
				process		=> query_in_use'access);

			-- If the unit is available then append it to the result:
			if not in_use then -- unit is available
				log (text => "unit " & to_string (element (c)) & " available.",
					 level => log_threshold + 2);
				
				names_of_available_units.append (element (c));
			end if;
		end query_in_use;

		
		procedure get_device_model (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module)
		is begin
			-- locate the device in the schematic:
			device_cursor_sch := find (module.devices, device_name);

			device_model := element (device_cursor_sch).model;

			log (text => "device model " & to_string (device_model),
				level => log_threshold + 1);
		end get_device_model;
		
		
	begin
		log (text => "looking up available units of " 
			 & to_string (device_name) & " ...",
			 level => log_threshold);

		log_indentation_up;

		-- get the device model:
		query_element (module_cursor, get_device_model'access);
				
		-- locate the device in the library
		device_cursor_lib := get_device_model_cursor (device_model);

		log_indentation_up;
		
		-- get the names of all units of the device
		all_unit_names := get_all_units (device_cursor_lib);

		-- extract available units
		all_unit_names.iterate (query_in_use'access);

		log_indentation_down;
		log_indentation_down;
		
		return names_of_available_units;

		--exception when event: others =>
			--put_line (exception_information (event));

			--return names_of_available_units;
	end get_available_units;



	
	
	function unit_available (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- IC1
		unit_name		: in pac_unit_name.bounded_string)
		return boolean
	is
		available : boolean := true; -- to be returned

		device_cursor_sch : pac_devices_sch.cursor;
		
		device_cursor_lib : pac_devices_lib.cursor;
		
		use pac_unit_names;
		all_unit_names : pac_unit_names.list;

		
		-- Clears the "available" flag if given unit is already in use:
		procedure query_in_use (
			device_name	: in type_device_name;
			device		: in type_device_sch) 
		is
		begin
			if contains (device.units, unit_name) then
				available := false;
			end if;
		end query_in_use;
		
		
	begin -- unit_available
		device_cursor_lib := device_model_cursor (module_cursor, device_name);

		-- get the names of all units of the device
		all_unit_names := get_all_units (device_cursor_lib);

		-- test whether the given unit is defined in the model:
		if contains (all_unit_names, unit_name) then
			
			-- locate the device in the schematic:
			device_cursor_sch := locate_device (module_cursor, device_name);

			-- Test whether the unit is already in use.
			-- If device does not exist, a constraint_error will arise here.
			query_element (
				position	=> device_cursor_sch,
				process		=> query_in_use'access);
			
		else
			raise constraint_error;
		end if;
		
		return available;
	end unit_available;




	

	function get_units_on_sheet (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- IC1
		sheet			: in type_sheet;
		log_threshold	: in type_log_level)
		return pac_unit_names.list
	is
		device_cursor_sch : pac_devices_sch.cursor;

		names_of_units : pac_unit_names.list;

		
		procedure query_units (
			device_name	: in type_device_name;
			device		: in type_device_sch)
		is 
			procedure query_unit (c : in pac_units.cursor) is 
				use pac_unit_name;
				use pac_unit_names;
			begin
				-- If the unit is on the given sheet then append it to the result:
				if get_sheet (element (c).position) = sheet then
					log (text => "unit " & to_string (key (c)) & " on sheet.",
						level => log_threshold + 2);
					
					names_of_units.append (key (c));
				end if;
			end query_unit;

			
		begin
			device.units.iterate (query_unit'access);
		end query_units;

		
	begin
		log (text => "looking up units of " 
			 & to_string (device_name) 
			 & " on sheet " & to_string (sheet) & " ...",
			 level => log_threshold);

		log_indentation_up;

		-- locate the device in the schematic:
		device_cursor_sch := locate_device (module_cursor, device_name);

		-- Test whether the unit is already in use.
		-- If device does not exist, a constraint_error will arise here.
		query_element (
			position	=> device_cursor_sch,
			process		=> query_units'access);


		log_indentation_down;
		
		return names_of_units;
	end get_units_on_sheet;




	
	
	function get_position (
		module	: in pac_generic_modules.cursor;
		device	: in type_device_name; -- R2
		unit	: in pac_unit_name.bounded_string)
		return type_object_position
	is
		device_cursor_sch : pac_devices_sch.cursor;

		unit_position : type_object_position;

		
		procedure query_unit (
			device_name	: in type_device_name;
			device		: in type_device_sch)
		is 
			unit_cursor : pac_units.cursor;
		begin
			-- locate the given unit in the given device
			unit_cursor := find (device.units, unit);

			-- get the coordinates of the unit
			unit_position := element (unit_cursor).position;
		end query_unit;

		
	begin
		-- locate the device in the schematic:
		device_cursor_sch := locate_device (module, device);

		query_element (
			position	=> device_cursor_sch,
			process		=> query_unit'access);

		return unit_position;
	end get_position;




	
	
	function get_position (
		device	: in pac_devices_sch.cursor; -- R2
		unit	: in pac_units.cursor)
		return type_object_position
	is
		unit_position : type_object_position;

		
		procedure query_unit (
			device_name	: in type_device_name;
			device		: in type_device_sch)
		is 
		begin
			-- get the coordinates of the unit
			unit_position := element (unit).position;
		end query_unit;
		
		
	begin
		query_element (
			position	=> device,
			process		=> query_unit'access);

		return unit_position;
	end get_position;



	
	
	function get_position (
		device		: in pac_devices_sch.cursor; -- R2
		unit		: in pac_units.cursor;
		category	: in type_placeholder_meaning)
		return type_vector_model
	is
		placeholder_position : type_vector_model; -- to be returned

		unit_position : type_object_position;

		
		procedure query_unit (
			device_name	: in type_device_name;
			device		: in type_device_sch)
		is 
			use et_symbols;
		begin
			-- get the coordinates of the unit
			unit_position := element (unit).position;

			-- get the coordinates of the placeholder:
			case category is
				when NAME =>
					placeholder_position := element (unit).name.position;

				when PURPOSE =>
					placeholder_position := element (unit).purpose.position;

				when VALUE =>
					placeholder_position := element (unit).value.position;
			end case;

			move_by (placeholder_position, to_distance_relative (unit_position.place));
			
		end query_unit;

		
	begin
		query_element (
			position	=> device,
			process		=> query_unit'access);

		return placeholder_position;
	end get_position;


	

	

	
	function get_sheet (
		module	: in pac_generic_modules.cursor;
		device	: in type_device_name; -- R2
		unit	: in pac_unit_name.bounded_string)
		return type_sheet
	is begin		
		return get_sheet (get_position (module, device, unit));
	end get_sheet;

	


	
	procedure fetch_unit (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC1
		unit_name		: in pac_unit_name.bounded_string; -- A, B, IO_BANK_2
		destination		: in type_object_position; -- sheet/x/y/rotation
		log_threshold	: in type_log_level) is separate;




	
	function unit_positions_valid (
		module_cursor 	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
		return boolean 
	is
		use et_numbering;
		devices : et_numbering.pac_devices.map;
	begin
		devices := sort_by_coordinates_2 (module_cursor, log_threshold);
		-- If a unit sits on top of another unit, sort_by_coordinates_2 throws a
		-- constraint_error which will be catched here.

		return true;
		
		exception when event: others => 
			return false;
		
	end unit_positions_valid;
	


	

	function get_port_properties (
		module_cursor	: in pac_generic_modules.cursor; -- motor_driver
		device_name		: in type_device_name; -- IC45
		unit_name		: in pac_unit_name.bounded_string; -- A, B, IO_BANK_2
		port_name		: in pac_port_name.bounded_string) -- CE
		return type_port_properties_access
	is
		properties : type_port_properties_access; -- to be returned
		
		terminal_name : et_terminals.pac_terminal_name.bounded_string;

		use et_port_direction;
		port_direction : type_port_direction := PASSIVE;
		port_properties_cursor : pac_ports.cursor;

		
		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			device_cursor_sch	: pac_devices_sch.cursor;
			variant 			: pac_package_variant_name.bounded_string; -- D, N
			device_cursor_lib	: pac_devices_lib.cursor;

			
			procedure query_variants (
				model	: in pac_device_model_file.bounded_string;
				device	: in type_device_model) 
			is
				variant_cursor : pac_variants.cursor;

				
				procedure query_ports (
					variant_name	: in pac_package_variant_name.bounded_string;
					variant			: in type_variant) 
				is
					use pac_terminal_port_map;
					terminal_cursor : pac_terminal_port_map.cursor := variant.terminal_port_map.first;
					use pac_port_name;
					use pac_unit_name;
				begin
					while terminal_cursor /= pac_terminal_port_map.no_element loop
						if	element (terminal_cursor).unit = unit_name and then
							element (terminal_cursor).name = port_name then
								terminal_name := key (terminal_cursor);
								exit;
						end if;
						next (terminal_cursor);
					end loop;
						
				end query_ports;

				
			begin -- query_variants
				variant_cursor := pac_variants.find (device.variants, variant);

				pac_variants.query_element (
					position	=> variant_cursor,
					process		=> query_ports'access);
				
			end query_variants;

			
			use pac_ports;

			
		begin -- query_devices
			-- locate the device in schematic (default assembly variant):
			device_cursor_sch := find (module.devices, device_name);

-- 			if device_cursor_sch /= pac_devices_sch.no_element then
			
				variant := element (device_cursor_sch).variant;

				-- get the name of the device model (or the generic name)
				device_cursor_lib := get_device_model_cursor (element (device_cursor_sch).model);

				-- Get the name of the terminal (the pin or pad) according to the device variant.
				-- Store it in variable terminal_name:
				pac_devices_lib.query_element (
					position	=> device_cursor_lib,
					process		=> query_variants'access);

				-- Get the electrical properties of the port of the current device:
				port_properties_cursor := get_properties (device_cursor_lib, port_name);

				-- Create the port where pointer "properties" is pointing at.
				-- It is created with the direction obtained from port_properties_cursor:
				properties := new type_port_properties (
					direction 	=> element (port_properties_cursor).direction);

				-- Assign the terminal name:
				properties.terminal := terminal_name;

				-- Assign electrical properties provided by port_properties_cursor:
				properties.properties := element (port_properties_cursor);

-- 			else
-- 				log (importance => ERROR, text => "Found terminal of device " & enclose_in_quotes (to_string (device_name)) &
-- 					 " , but this device does not exist !");
-- 				raise constraint_error;
-- 			end if;
			
		end query_devices;

		
	begin
		query_element (
			position	=> module_cursor,
			process		=> query_devices'access);
		
		return properties;
	end get_port_properties;



	
	

	
	function to_string (
		device_name		: in type_device_name; -- IC45
		unit_name		: in pac_unit_name.bounded_string; -- C
		query_result	: in type_unit_query)
		return string 
	is 
		use pac_unit_name;
	begin
		if query_result.exists then
			if get_length (unit_name) > 0 then
				return "Location of device " & to_string (device_name)
					& " unit " & to_string (unit_name)
					& " :" & to_string (query_result.position);
			else
				return "Location of device " & to_string (device_name)
					& " :" & to_string (query_result.position);
			end if;
		else
			return "device " & to_string (device_name)
				& " unit " & to_string (unit_name)
				& " does not exist !";
		end if;
	end to_string;


	
	
	
	procedure delete_unit (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- IC45
		unit_name		: in pac_unit_name.bounded_string; -- A
		log_threshold	: in type_log_level) 
	is
		
		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			device_cursor : pac_devices_sch.cursor;

			-- temporarily storage of unit coordinates.
			-- There will be only one unit in this container.
			position_of_unit : pac_unit_positions.map;

			ports : pac_ports.map;
			

			procedure query_units (
				device_name	: in type_device_name;
				device		: in out type_device_sch) 
			is
				unit_cursor : pac_units.cursor;
			begin
				if contains (device.units, unit_name) then
					-- locate unit by its name
					unit_cursor := find (device.units, unit_name);

					-- Load the single unit position and insert in container "position_of_unit"
					pac_unit_positions.insert (
						container	=> position_of_unit, 
						key			=> unit_name,
						new_item	=> element (unit_cursor).position);

					log_unit_positions (position_of_unit, log_threshold);
					
					-- delete the unit
					delete (device.units, unit_name);
				else
					unit_not_found (unit_name);
				end if;
			end query_units;

			
			units_invoked : boolean := true; -- goes false if no unit used anymore

			procedure query_number_of_invoked_units (
				device_name	: in type_device_name;
				device		: in type_device_sch) 
			is begin
				if length (device.units) = 0 then
					units_invoked := false;
				end if;
			end query_number_of_invoked_units;
			

		begin -- query_devices
			if contains (module.devices, device_name) then

				-- Before the actual deletion, the coordinates of the
				-- unit must be fetched. These coordinates will later assist
				-- in deleting the port names from connected net segments.
				device_cursor := find (module.devices, device_name); -- the device should be there

				-- locate the unit, load position and then delete the targeted unit
				update_element (
					container	=> module.devices,
					position	=> device_cursor,
					process		=> query_units'access);
				
				log_indentation_up;

				-- Fetch the ports of the unit to be deleted.
				ports := get_ports_of_unit (device_cursor, unit_name);
				
				-- Delete the ports of the targeted unit from module.nets
				delete_ports (
					module			=> module_cursor,
					device			=> device_name,
					ports			=> ports,
					sheets			=> position_of_unit, -- there is only one unit -> only one sheet to look at
					log_threshold	=> log_threshold);

				-- In case no more units are invoked then the device must be
				-- deleted entirely from module.devices.
				-- First we query the number of still invoked units. If none invoked,
				-- the flag units_invoked goes false.
				query_element (
					position	=> device_cursor,
					process		=> query_number_of_invoked_units'access);

				if not units_invoked then
					delete (module.devices, device_cursor);
				end if;

				update_ratsnest (module_cursor, log_threshold);
				
				log_indentation_down;				
			else
				device_not_found (device_name);
			end if;
		end query_devices;

		
	begin
		log (text => "module " & to_string (module_cursor) &
			 " deleting " & to_string (device_name) & " unit " & 
			 to_string (unit_name) & " ...", level => log_threshold);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_devices'access);

	end delete_unit;
	


	
	

	
	procedure move_unit (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- IC45
		unit_name		: in pac_unit_name.bounded_string; -- A
		coordinates		: in type_coordinates; -- relative/absolute
		sheet			: in type_sheet_relative; -- -3/0/2
		point			: in type_vector_model; -- x/y
		log_threshold	: in type_log_level) 
	is

		
		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			device_cursor : pac_devices_sch.cursor;

			-- temporarily storage of unit coordinates.
			-- There will be only one unit in this container.
			position_of_unit_old : pac_unit_positions.map;

			position_of_unit_new : type_object_position;

			ports : pac_ports.map;
			

			procedure query_units (
				device_name	: in type_device_name;
				device		: in out type_device_sch) 
			is
				unit_cursor : pac_units.cursor;

				procedure move_unit (
					unit_name	: in pac_unit_name.bounded_string;
					unit		: in out type_unit) 
				is
					use et_schematic_coordinates;
				begin
					case coordinates is
						when ABSOLUTE =>
							-- build the new position while preserving rotation:
							unit.position := to_position (
								point		=> point, 
								sheet		=> type_sheet (sheet),
								rotation	=> get_rotation (unit.position));

						when RELATIVE =>
							move (
								position	=> unit.position,
								offset		=> to_position_relative (point, sheet));
								-- rotation remains as it is
					end case;

					-- store new unit position
					position_of_unit_new := unit.position;
					
					exception
						when event: others =>
							log (ERROR, "coordinates invalid !", console => true); -- CS required more details
							log (text => ada.exceptions.exception_information (event), console => true);
							raise;
					
				end move_unit;

				
			begin -- query_units
				if contains (device.units, unit_name) then
					-- locate unit by its name
					unit_cursor := find (device.units, unit_name);

					-- load unit position and insert in container "position_of_unit_old"
					pac_unit_positions.insert (
						container	=> position_of_unit_old, 
						key			=> unit_name,
						new_item	=> element (unit_cursor).position);

					-- log old unit position
					log_unit_positions (position_of_unit_old, log_threshold + 1); -- there is only one unit
-- 					log (text => "position before " & 
-- 						 et_schematic_coordinates.to_string (
-- 							type_ports.first_element (positions)), level => log_threshold + 1);

					update_element (
						container	=> device.units,
						position	=> unit_cursor,
						process		=> move_unit'access);
					
				else
					unit_not_found (unit_name);
				end if;
			end query_units;

			
		begin -- query_devices
			if contains (module.devices, device_name) then

				-- Before the actual move, the coordinates of the
				-- unit must be fetched. These coordinates will later assist
				-- in deleting the port names from connected net segments.
				device_cursor := find (module.devices, device_name); -- the device should be there

				-- locate the unit, get its current position, set its new position
				update_element (
					container	=> module.devices,
					position	=> device_cursor,
					process		=> query_units'access);
				
				log_indentation_up;

				-- Fetch the ports of the unit to be moved 
				-- The x/y-positions of the ports are as defined in the symbol model.
				ports := get_ports_of_unit (device_cursor, unit_name);

				-- Rotate the ports according to the rotation of the unit:
				rotate_ports (ports, get_rotation (position_of_unit_new));

				
				-- Delete the old ports of the targeted unit from module.nets
				delete_ports (
					module			=> module_cursor,
					device			=> device_name,
					ports			=> ports,
					sheets			=> position_of_unit_old,
					log_threshold	=> log_threshold + 1);

				-- Calculate the new positions of the unit ports:
				move_ports (ports, position_of_unit_new);

				-- Insert the new unit ports in the nets (type_generic_module.nets):
				insert_ports (
					module			=> module_cursor,
					device			=> device_name,
					unit			=> unit_name,
					ports			=> ports,
					sheet			=> get_sheet (position_of_unit_new),
					log_threshold	=> log_threshold + 1);

				update_ratsnest (module_cursor, log_threshold + 1);
				
				log_indentation_down;				
			else
				device_not_found (device_name);
			end if;
		end query_devices;

		
	begin
		case coordinates is
			when ABSOLUTE =>
				log (text => "module " & to_string (module_cursor)
					& " moving " & enclose_in_quotes (to_string (device_name)) 
					& " unit " & enclose_in_quotes (to_string (unit_name)) 
					& " to sheet" & to_string (sheet) 
					& to_string (point),
					level => log_threshold);

			when RELATIVE =>
				log (text => "module " & to_string (module_cursor)
					& " moving " & enclose_in_quotes (to_string (device_name))
					& " unit " & enclose_in_quotes (to_string (unit_name)) 
					& " by " & relative_to_string (sheet) & " sheet(s)" 
					& to_string (point),
					level => log_threshold);
		end case;
		
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_devices'access);

	end move_unit;



	

	
	-- Drags the net segments according to the given drag_list of a unit.
	-- Changes the position of start or end points of segments.
	-- Does NOT create new connections with segments if a port
	-- lands on the start or end point of another segment.
	-- Does NOT create a new connection with a segments if a port
	-- lands between start and end point.
	procedure drag_net_segments (
		module			: in pac_generic_modules.cursor;-- the module
		drag_list		: in type_drags_of_ports.map;	-- the old and new port positions
		sheet			: in type_sheet;				-- the sheet to look at
		log_threshold	: in type_log_level) 
	is

		procedure query_nets (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is

			procedure query_net (net_cursor : in pac_nets.cursor) is
				use pac_nets;
				use et_symbols;

				procedure query_strands (
					net_name	: in pac_net_name.bounded_string;
					net			: in out type_net) is
					use et_schematic_coordinates;
					
					use pac_strands;
					strand_cursor : pac_strands.cursor;
					
					use type_drags_of_ports;
					drag_cursor : type_drags_of_ports.cursor := drag_list.first;

					drag_processed : boolean;

					-- We must keep record of segments that have been dragged already.
					-- Each time a segment has been dragged, it will be appended to
					-- this list:
					already_dragged_segments : pac_net_segments.list;

					
					procedure query_segments (strand : in out type_strand) is
						use pac_net_segments;

						
						procedure query_segment (segment_cursor : in pac_net_segments.cursor) is 

							-- Changes the position of start or end point of a segment 
							-- according to the drag point:
							procedure change_segment (segment : in out type_net_segment) is begin
								
								-- if port sits on a start point of a segment -> move start point
								if segment.start_point = element (drag_cursor).before then
									log (text => "move segment start point from" & 
										to_string (segment.start_point),
										level => log_threshold + 3);

									segment.start_point := element (drag_cursor).after;

									log (text => "to" & 
										to_string (segment.start_point),
										level => log_threshold + 3);

									-- Now the segment has been dragged. Store it
									-- in list of already dragged segments:
									already_dragged_segments.append (segment);
									
									drag_processed := true;
								end if;

								-- if port sits on an end point of a segment -> move end point
								if segment.end_point = element (drag_cursor).before then
									log (text => "move segment end point from" & 
										to_string (segment.end_point),
										level => log_threshold + 3);

									segment.end_point := element (drag_cursor).after;

									log (text => "to" & 
										to_string (segment.end_point),
										level => log_threshold + 3);

									-- Now the segment has been dragged. Store it
									-- in list of already dragged segments:
									already_dragged_segments.append (segment);
									
									drag_processed := true;
								end if;

							end change_segment;

							
						begin -- query_segment
							-- Probe only those segments which have not been dragged already:
							if not already_dragged_segments.contains (element (segment_cursor)) then
								
								log_indentation_up;
								log (text => "probing " & to_string (segment_cursor), level => log_threshold + 2);
								log_indentation_up;
								
								update_element (
									container	=> strand.segments,
									position	=> segment_cursor,
									process		=> change_segment'access);
												
								log_indentation_down;
								log_indentation_down;
								
							end if;
						end query_segment;

						
					begin -- query_segments
						-- Probe segments of this strand. Skip segments that have been
						-- dragged already:
						iterate (strand.segments, query_segment'access);

						-- Update strand position if any movement took place.
						if drag_processed then
							set_strand_position (strand); 
						end if;						
					end query_segments;

					
				begin -- query_strands
					-- loop in drag list
					while drag_cursor /= type_drags_of_ports.no_element loop
						log (text => "probing port " & to_string (key (drag_cursor)), level => log_threshold + 1);
						log_indentation_up;

						-- If the current drag point sits on a strand, this flag will go true. Other 
						-- strands will then not be looked at because the point can only sit on 
						-- one strand.
						drag_processed := false;
						
						strand_cursor := net.strands.first;
						while strand_cursor /= pac_strands.no_element loop
							
							-- We pick out only the strands on the targeted sheet:
							if get_sheet (element (strand_cursor).position) = sheet then
								log (text => "net " & to_string (key (net_cursor)), level => log_threshold + 1);

								log_indentation_up;
								log (text => "strand " & to_string (position => element (strand_cursor).position),
									level => log_threshold + 1);
							
								-- Iterate in segments of strand. If drag point sits on any segment
								-- the flag drag_processed goes true.
								update_element (
									container	=> net.strands,
									position	=> strand_cursor,
									process		=> query_segments'access);
							
								log_indentation_down;
							end if;

							-- If the drag point has been processed, there is no need to look up
							-- other strands for this port.
							if drag_processed then exit; end if;
							
							next (strand_cursor);
						end loop;

						log_indentation_down;
						next (drag_cursor);
					end loop;
						
				end query_strands;

				
			begin -- query_net
				update_element (
					container	=> module.nets,
					position	=> net_cursor,
					process		=> query_strands'access);
			end query_net;				

			
		begin -- query_nets
			pac_nets.iterate (module.nets, query_net'access);
		end query_nets;

		
	begin -- drag_net_segments
		log (text => "dragging net segments with units on sheet" & 
			 to_string (sheet) & " ...", level => log_threshold);
		log_indentation_up;

		update_element (
			container	=> generic_modules,
			position	=> module,
			process		=> query_nets'access);

		log_indentation_down;
	end drag_net_segments;



	

	
	-- Tests whether the given unit ports at their individual location are movable. 
	-- The criteria for movement are: no netchanger port, no device port, no submodule ports there.
	-- The only port allowed at an individual drag point is the port-to-be-dragged itself.
	-- CS: Becomes obsolete once ports at the same x/y position are prevented.
	procedure movable_test (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name;
		unit_name		: in pac_unit_name.bounded_string;
		location 		: in type_object_position; -- only sheet number matters
		unit_ports		: in pac_ports.map;
		log_threshold	: in type_log_level)
	is
		use pac_ports;
		port_cursor : pac_ports.cursor := unit_ports.first;

		
		procedure test_point (port_cursor : in pac_ports.cursor) is
			point : type_object_position; -- the point
			ports : type_ports;
			port : type_device_port;
			use pac_submodule_ports;
			use pac_device_ports;
			use et_netlists.pac_netchanger_ports;
		begin
			-- assemble the point to be probed
			point := to_position (
				point	=> element (port_cursor).position,
				sheet	=> get_sheet (location));
			
			-- If no net segments start or end at given point then this test won't
			-- complain. If segments are meeting this point, no other ports must be
			-- here (except the port-to-be-dragged):
			if net_segment_at_place (module_cursor, point) then

				-- There are net segments starting or ending at point.
				-- Make sure at point are no ports of devices, submodules or other 
				-- netchangers (except the unit port to be dragged):

				port := (device_name, unit_name, key (port_cursor)); -- IC12, CE
				
				-- Collect all ports of possible other devices, submodules and netchangers
				-- at given point:
				ports := ports_at_place (module_cursor, point, log_threshold + 1);

				-- If no netchanger and no submodule ports here:
				if is_empty (ports.netchangers) and is_empty (ports.submodules) then

					-- If the ONE and ONLY device/unit port is the 
					-- port-to-be-dragged then everything is fine.
					if length (ports.devices) = 1 then
						
						if contains (ports.devices, port) then
							null; -- fine -> movable test passed
						else
							-- there is another netchanger port
							dragging_not_possible (to_string (key (port_cursor)), point);
						end if;
					
					else
						-- there are more submodule ports
						dragging_not_possible (to_string (key (port_cursor)), point);
					end if;
					
				else -- device or netchanger ports here
					dragging_not_possible (to_string (key (port_cursor)), point);
				end if;
			end if;
		end test_point;

		
	begin -- movable_test
		log (text => "movable test ...", level => log_threshold);
		log_indentation_up;

		while port_cursor /= pac_ports.no_element loop
			test_point (port_cursor);
			next (port_cursor);
		end loop;
		
		log_indentation_down;
	end movable_test;






	function get_unit_position (
		module_cursor	: in pac_generic_modules.cursor; -- motor_driver
		device_name		: in type_device_name; -- IC45
		unit_name		: in pac_unit_name.bounded_string) -- C
		return type_unit_query 
	is
		exists : boolean := false;
		pos : type_object_position; -- x/y, rotation, sheet

		
		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			device_cursor : pac_devices_sch.cursor;

			
			procedure query_units (
				device_name	: in type_device_name; -- IC45
				device		: in type_device_sch) 
			is
				unit_cursor : pac_units.cursor;				
			begin
				-- If the given unit_name contains something, locate the unit
				-- by its name. If unit_name is empty, locate the first unit.
				if pac_unit_name.length (unit_name) > 0 then -- locate by name
					
					unit_cursor := pac_units.find (device.units, unit_name);

					if unit_cursor /= pac_units.no_element then -- unit exists
						exists := true;
						pos := element (unit_cursor).position;
					else
						exists := false; -- unit does not exist
					end if;
					
				else -- locate the first unit:
					unit_cursor := pac_units.first (device.units);
					-- There should be at least one unit. Otherwise raise constraint_error.

					if unit_cursor /= pac_units.no_element then -- unit exists
						exists := true;
						pos := element (unit_cursor).position;
					else
						exists := false; -- unit does not exist
						raise constraint_error; -- CS do something
					end if;
					
				end if;
			end query_units;
			
			
		begin -- query_devices
			-- locate the device:
			device_cursor := pac_devices_sch.find (module.devices, device_name);

			if device_cursor /= pac_devices_sch.no_element then -- device exists
				pac_devices_sch.query_element (device_cursor, query_units'access);
			else
				exists := false; -- device does not exist
			end if;
			
		end query_devices;
		
		
	begin -- unit_position
		query_element (module_cursor, query_devices'access);

		if exists then return (exists => true, position => pos);
		else return (exists => false);
		end if;
		
	end get_unit_position;




	
	
	function is_movable (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name;
		unit_name		: in pac_unit_name.bounded_string;
		location 		: in type_object_position; -- only sheet number matters
		unit_ports		: in pac_ports.map;
		log_threshold	: in type_log_level)
		return boolean
	is
		result : boolean := false;
		
		use et_symbols;
		use pac_ports;
		port_cursor : pac_ports.cursor := unit_ports.first;

		
		procedure test_point (port_cursor : in pac_ports.cursor) is
			point : type_object_position; -- the point
			ports : type_ports;
			port : type_device_port;
			use pac_submodule_ports;
			use pac_device_ports;
			use et_netlists.pac_netchanger_ports;
		begin
			-- assemble the point to be probed
			point := to_position (
				point	=> element (port_cursor).position,
				sheet	=> get_sheet (location));
			
			-- If no net segments start or end at given point then this test won't
			-- complain. If segments are meeting this point, no other ports must be
			-- here (except the port-to-be-dragged):
			if net_segment_at_place (module_cursor, point) then

				-- There are net segments starting or ending at point.
				-- Make sure at point are no ports of devices, submodules or other 
				-- netchangers (except the unit port to be dragged):

				port := (device_name, unit_name, key (port_cursor)); -- IC12, CE
				
				-- Collect all ports of possible other devices, submodules and netchangers
				-- at given point:
				ports := ports_at_place (module_cursor, point, log_threshold + 1);

				-- If no netchanger and no submodule ports here:
				if is_empty (ports.netchangers) and is_empty (ports.submodules) then

					-- If the ONE and ONLY device/unit port is the 
					-- port-to-be-dragged then everything is fine.
					if length (ports.devices) = 1 then
						
						if contains (ports.devices, port) then
							result := true; -- fine -> movable test passed
						else
							-- there is another netchanger port
							result := false;
						end if;
					
					else
						-- there are more submodule ports
						result := false;
					end if;
					
				else -- device or netchanger ports here
					result := false;
				end if;
			end if;
		end test_point;

		
	begin -- is_movable
		log (text => "movable test ...", level => log_threshold);
		log_indentation_up;

		while port_cursor /= pac_ports.no_element loop
			test_point (port_cursor);

			-- abort this loop as soon as a non-movable port has been detected:
			if result = false then
				exit;
			end if;
			
			next (port_cursor);
		end loop;
		
		log_indentation_down;

		return result;
	end is_movable;




	
	
	procedure drag_unit (
		module_cursor 	: in pac_generic_modules.cursor; -- points to the module being modified
		device_name		: in type_device_name; -- IC45
		unit_name		: in pac_unit_name.bounded_string; -- A
		coordinates		: in type_coordinates; -- relative/absolute
		point			: in type_vector_model; -- x/y
		log_threshold	: in type_log_level) 
	is

		
		-- Merges the two maps ports_old and ports_new to a drag list.
		-- The resulting drag list tells which port is to be moved from old to new position.
		function make_drag_list ( 
			ports_old : in pac_ports.map;
			ports_new : in pac_ports.map) 
			return type_drags_of_ports.map 
		is
			use type_drags_of_ports;
			drag_list : type_drags_of_ports.map;

			-- ports_old and ports_new are both equally long and contain 
			-- equal keys (the port names). So we use two cursors and advance them
			-- simultaneously in a loop (see below).
			use pac_ports;
			cursor_old : pac_ports.cursor := ports_old.first;
			cursor_new : pac_ports.cursor := ports_new.first;
		begin
			-- Loop in ports_old, copy the key to the drag list.
			-- Take the old position from ports_old and the new position from ports_new:
			while cursor_old /= pac_ports.no_element loop
				insert (
					container	=> drag_list,
					key			=> key (cursor_old), -- the port name
					new_item	=> (
								before	=> element (cursor_old).position, -- x/y
								after	=> element (cursor_new).position) -- x/y
					   );
				
				next (cursor_old);
				next (cursor_new);
			end loop;
			
			return drag_list;
		end make_drag_list;

		
		
		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			device_cursor : pac_devices_sch.cursor;

			-- temporarily storage of unit coordinates.
			-- There will be only one unit in this container.
			--position_of_unit_old : pac_unit_positions.map;
			position_of_unit_old : type_object_position;	
			position_of_unit_new : type_object_position;

			ports, ports_old, ports_new : pac_ports.map;

			procedure query_unit_location (
				device_name	: in type_device_name;
				device		: in type_device_sch) 
			is
				unit_cursor : pac_units.cursor;
			begin
				if contains (device.units, unit_name) then
					unit_cursor := find (device.units, unit_name); -- the unit should be there

					-- store old unit position
					position_of_unit_old := element (unit_cursor).position;
					log (text => "unit position old: " & to_string (position => position_of_unit_old), level => log_threshold + 1);
				else
					unit_not_found (unit_name);
				end if;
			end query_unit_location;

			
			procedure query_units (
				device_name	: in type_device_name;
				device		: in out type_device_sch) 
			is
				unit_cursor : pac_units.cursor;

				procedure move_unit (
					unit_name	: in pac_unit_name.bounded_string;
					unit		: in out type_unit) 
				is
					use et_schematic_coordinates;

					-- Load the current sheet number where the unit is.
					-- NOTE: The sheet number does not change in drag operations.
					sheet : type_sheet := get_sheet (unit.position);
				begin
					-- Set new x/y position. 
					-- Preserve sheet number and rotation.
					case coordinates is
						when ABSOLUTE =>

							unit.position := to_position (
								point		=> point, 
								sheet		=> sheet,
								rotation	=> get_rotation (unit.position));
							
						when RELATIVE =>
							move_by (
								point	=> unit.position.place,
								offset	=> to_distance_relative (point));
					end case;
					
					exception
						when event: others =>
							log (ERROR, "coordinates invalid !", console => true); -- CS required more details
							log (text => ada.exceptions.exception_information (event), console => true);
							raise;
					
				end move_unit;

				
			begin -- query_units
				unit_cursor := find (device.units, unit_name); -- the unit should be there

				-- move the unit
				update_element (
					container	=> device.units,
					position	=> unit_cursor,
					process		=> move_unit'access);

				-- store new unit position
				position_of_unit_new := element (unit_cursor).position;
				
				log (text => "unit position new: " & to_string (position => position_of_unit_new), level => log_threshold + 1);
			end query_units;

			
		begin -- query_devices
			if contains (module.devices, device_name) then
				device_cursor := find (module.devices, device_name); -- the device should be there

				log_indentation_up;

				-- Before the actual drag, the coordinates of the
				-- unit must be fetched. These coordinates will later assist
				-- in changing the positions of connected net segments.
				
				-- locate the unit, store old position in position_of_unit_old
				query_element (
					position	=> device_cursor,
					process		=> query_unit_location'access);
				
				-- Fetch the ports of the unit to be moved. These are the default port positions
				-- (relative to the symbol origin) as they are defined in the library model.
				ports := get_ports_of_unit (device_cursor, unit_name);
				
				-- Calculate the old and new positions of the unit ports:
				ports_old := ports;
				rotate_ports (ports_old, get_rotation (position_of_unit_old));
				move_ports (ports_old, position_of_unit_old); 
				-- ports_old now contains the absolute port positions in the schematic BEFORE the move.

				-- Test whether the ports of the unit can be dragged.
				-- CS: Might become obsolete once ports at the same x/y position are prevented.
				-- CS: Before the drag: If a port of the unit sits at the same place
				--     where a port of another unit is, then a net segment should be
				--     inserted between them ?
				movable_test (module_cursor, device_name, unit_name, 
					position_of_unit_old, ports_old, log_threshold + 1);

				-- locate the unit, move it, store new position in position_of_unit_new
				update_element (
					container	=> module.devices,
					position	=> device_cursor,
					process		=> query_units'access);
				
				ports_new := ports;
				rotate_ports (ports_new, get_rotation (position_of_unit_new));
				move_ports (ports_new, position_of_unit_new);
				-- ports_new now contains the absolute port positions in the schematic AFTER the move.
				
				-- Change net segments in the affected nets (type_generic_module.nets):
				drag_net_segments (
					module			=> module_cursor,
					drag_list		=> make_drag_list (ports_old, ports_new),
					sheet			=> get_sheet (position_of_unit_new), -- or position_of_unit_old
					log_threshold	=> log_threshold + 1);

				-- The drag operation might result in new port-to-net connections.
				-- So we must insert new ports in segments.
				-- Insert possible new unit ports in the nets (type_generic_module.nets):
				log_indentation_up;
				
				insert_ports (
					module			=> module_cursor,
					device			=> device_name,
					unit			=> unit_name,
					ports			=> ports_new,
					sheet			=> get_sheet (position_of_unit_new),
					log_threshold	=> log_threshold + 1);

				log_indentation_down;
				log_indentation_down;				
			else
				device_not_found (device_name);
			end if;
		end query_devices;

		
	begin
		case coordinates is
			when ABSOLUTE =>
				log (text => "module " & to_string (module_cursor)
					 & " dragging " & enclose_in_quotes (to_string (device_name)) 
					 & " unit " & enclose_in_quotes	(to_string (unit_name)) 
					 & " to" & to_string (point), 
					 level => log_threshold);

			when RELATIVE =>
				log (text => "module " & to_string (module_cursor)
					 & " dragging " & enclose_in_quotes (to_string (device_name)) 
					 & " unit " & enclose_in_quotes	(to_string (unit_name)) 
					 & " by" & to_string (point), 
					 level => log_threshold);
		end case;
		
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_devices'access);

		update_ratsnest (module_cursor, log_threshold + 1);
	end drag_unit;




	

	
	procedure rotate_unit (
		module_cursor	: in pac_generic_modules.cursor;
		device_name		: in type_device_name; -- IC45
		unit_name		: in pac_unit_name.bounded_string; -- A
		coordinates		: in type_coordinates; -- relative/absolute
		rotation		: in et_schematic_coordinates.type_rotation_model; -- 90
		log_threshold	: in type_log_level) 
	is

		
		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			device_cursor : pac_devices_sch.cursor;

			position_of_unit : type_object_position;
			rotation_before : et_schematic_coordinates.type_rotation_model;

			ports_lib, ports_scratch : pac_ports.map;

			
			procedure query_units (
				device_name	: in type_device_name;
				device		: in out type_device_sch)
			is
				unit_cursor : pac_units.cursor;

				
				procedure rotate_unit (
					name	: in pac_unit_name.bounded_string; -- A
					unit	: in out type_unit) 
				is
					preamble : constant string := " placeholder now at";
					
					procedure rotate_placeholders_absolute (rot : in type_rotation_model) is 

						-- Get the default positions of texts and placeholders as
						-- specified in symbol model. The default positions are
						-- later rotated by the given rotation rot.
						default_positions : et_symbols.type_default_text_positions := 
							get_default_text_positions (device_cursor, name);
						
						-- Rotates the position by the given rotation rot:
						function add_rot (p : in type_vector_model) return type_rotation_model is begin
							return get_rotation (p) + rot;
						end;

					begin
						-- The current positions of the placeholders are overwritten by
						-- the defaults as specified in the symbol mode.
						-- Then the position of placeholders around the origin of the unit
						-- are rotated.
						
						-- NAME
						-- Rotate the placeholder around its own anchor point so that it
						-- it is readable from the front or from the right.
						unit.name.rotation := snap (default_positions.name.rotation + rot);

						-- reset the placeholder anchor point to the position as specified in the symbol model
						unit.name.position := default_positions.name.position;
						
						-- rotate the placeholder anchor point around the symbol origin:
						rotate_to (unit.name.position, add_rot (default_positions.name.position));
									
						log (text => "name" & preamble & to_string (unit.name.position), 
								level => log_threshold + 2);


						-- VALUE
						-- Rotate the placeholder around its own anchor point so that it
						-- it is readable from the front or from the right.
						unit.value.rotation := snap (default_positions.value.rotation + rot);
						
						-- reset the placeholder anchor point to the position as specified in the symbol model
						unit.value.position := default_positions.value.position;
						
						-- rotate the placeholder anchor point around the symbol origin:
						rotate_to (unit.value.position, add_rot (default_positions.value.position));

						log (text => "value" & preamble & to_string (unit.value.position), 
								level => log_threshold + 2);


						-- PURPOSE
						-- Rotate the placeholder around its own anchor point so that it
						-- it is readable from the front or from the right.
						unit.purpose.rotation := snap (default_positions.purpose.rotation + rot);

						-- reset the placeholder anchor point to the position as specified in the symbol model
						unit.purpose.position := default_positions.purpose.position;
						
						-- rotate the placeholder anchor point around the symbol origin:
						rotate_to (unit.purpose.position, add_rot (default_positions.purpose.position));

						log (text => "purpose" & preamble & to_string (unit.purpose.position), 
								level => log_threshold + 2);

					end rotate_placeholders_absolute;
					
					
					procedure rotate_placeholders_relative (rot : in type_rotation_model) is begin
					-- Rotate position of placeholders around the unit origin. 
					
						-- NAME
						-- Rotate the placeholder around its own anchor point so that it
						-- it is readable from the front or from the right.
						unit.name.rotation := snap (unit.name.rotation + rot);

						-- rotate the placeholder anchor point around the symbol origin:
						rotate_by (unit.name.position, rot);

						log (text => "name" & preamble & to_string (unit.name.position), 
								level => log_threshold + 2);


						-- VALUE
						-- Rotate the placeholder around its own anchor point so that it
						-- it is readable from the front or from the right.
						unit.value.rotation := snap (unit.value.rotation + rot);

						-- rotate the placeholder anchor point around the symbol origin:
						rotate_by (unit.value.position, rot);

						log (text => "value" & preamble & to_string (unit.value.position), 
								level => log_threshold + 2);


						-- PURPOSE
						-- Rotate the placeholder around its own anchor point so that it
						-- it is readable from the front or from the right.
						unit.purpose.rotation := snap (unit.purpose.rotation + rot);

						-- rotate the placeholder anchor point around the symbol origin:
						rotate_by (unit.purpose.position, rot);

						log (text => "purpose" & preamble & to_string (unit.purpose.position), 
								level => log_threshold + 2);

					end rotate_placeholders_relative;

				begin -- rotate_unit
					case coordinates is
						when ABSOLUTE =>
							set (unit.position, rotation);
							rotate_placeholders_absolute (rotation);
							
						when RELATIVE =>
							set (unit.position, add (rotation_before, rotation));
							
							log (text => "rotation now" & to_string (get_rotation (unit.position)),
									level => log_threshold + 1);

							rotate_placeholders_relative (rotation);
					end case;
				end rotate_unit;

				
			begin -- query_units
				if contains (device.units, unit_name) then
					-- locate unit by its name
					unit_cursor := find (device.units, unit_name);

					-- load unit position and current rotation
					position_of_unit := element (unit_cursor).position;
					rotation_before := get_rotation (element (unit_cursor).position);

					-- log unit position and current rotation
					log (text => to_string (position => position_of_unit) &
							" rotation before" & to_string (rotation_before),
							level => log_threshold + 1);

					log_indentation_up;
					
					pac_units.update_element (
						container	=> device.units,
						position	=> unit_cursor,
						process		=> rotate_unit'access);

					log_indentation_down;
				else
					unit_not_found (unit_name);
				end if;
			end query_units;

			
		begin -- query_devices
			if contains (module.devices, device_name) then

				-- Before the rotation, the coordinates of the
				-- unit must be fetched. These coordinates will later assist
				-- in deleting the port names from connected net segments.
				device_cursor := find (module.devices, device_name); -- the device should be there

				-- rotate the unit
				update_element (
					container	=> module.devices,
					position	=> device_cursor,
					process		=> query_units'access);
				
				log_indentation_up;

				-- Fetch the ports of the unit to be rotated.
				-- The coordinates here are the default positions (in the library model)
				-- relative to the center of the units.
				ports_lib := get_ports_of_unit (device_cursor, unit_name);
				
				ports_scratch := ports_lib;						 

				-- Calculate the absolute positions of the unit ports in the schematic
				-- as they are BEFORE the rotation:
				rotate_ports (ports_scratch, rotation_before);
				move_ports (ports_scratch, position_of_unit);
				
				-- Delete the old ports of the targeted unit from module.nets.
				-- The unit is on a certain sheet. The procedure delete_ports however
				-- requires a list of unit positions (containing sheet numbers).
				-- So we create a list "sheets", put the unit name and position in it,
				-- and pass it to procedure delete_ports:
				declare
					sheets : pac_unit_positions.map;
				begin
					pac_unit_positions.insert (
						container	=> sheets,
						key			=> unit_name,
						new_item	=> position_of_unit);

					delete_ports (
						module			=> module_cursor,
						device			=> device_name,
						ports			=> ports_scratch,
						sheets			=> sheets, 
						log_threshold	=> log_threshold + 1);
				end;

				
				-- Calculate the new positions of the unit ports.
				case coordinates is
					when ABSOLUTE =>
						rotate_ports (ports_lib, rotation);
					when RELATIVE =>
							-- The given angle of rotation adds to the rotation_before:
						rotate_ports (ports_lib, add (rotation_before, rotation));
				end case;
				
				move_ports (ports_lib, position_of_unit);
				
				-- Insert the new unit ports in the nets (type_generic_module.nets):
				insert_ports (
					module			=> module_cursor,
					device			=> device_name,
					unit			=> unit_name,
					ports			=> ports_lib,
					sheet			=> get_sheet (position_of_unit),
					log_threshold	=> log_threshold + 1);

				update_ratsnest (module_cursor, log_threshold + 1);
				
				log_indentation_down;				
			else
				device_not_found (device_name);
			end if;
		end query_devices;

		
	begin
		case coordinates is
			when ABSOLUTE =>
				log (text => "module " & to_string (module_cursor)
					& " rotating " & enclose_in_quotes (to_string (device_name)) 
					& " unit " & enclose_in_quotes (to_string (unit_name)) 
					& " to" & to_string (rotation),
					level => log_threshold);

			when RELATIVE =>
				if rotation in type_rotation_relative then
					log (text => "module " & to_string (module_cursor)
						& " rotating " & enclose_in_quotes (to_string (device_name)) 
						& " unit " & enclose_in_quotes (to_string (unit_name))
						& " by" & to_string (rotation), 
						level => log_threshold);
				else
					relative_rotation_invalid;
				end if;
		end case;
		
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_devices'access);


		update_ratsnest (module_cursor, log_threshold + 1);
	end rotate_unit;






	function get_object_name (
		object	: in type_object_unit)
		return string
	is begin
		return get_full_name (object.device_cursor, object.unit_cursor);
	end get_object_name;

	

	


	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		unit			: in type_object_unit;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is			
			procedure query_device (
				device_name	: in type_device_name;
				device		: in out type_device_sch)
			is 

				procedure query_unit (
					unit_name	: in pac_unit_name.bounded_string;
					unit		: in out type_unit)
				is begin
					modify_status (unit, operation);
				end query_unit;
				
			begin
				device.units.update_element (unit.unit_cursor, query_unit'access);
			end query_device;
			
		begin
			module.devices.update_element (unit.device_cursor, query_device'access);
		end query_module;
		

	begin
		log (text => "module " & to_string (module_cursor)
			& " modifying status of device unit "
			& get_object_name (unit)
			& " / " & to_string (operation),
			level => log_threshold);


		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,		   
			process		=> query_module'access);

		log_indentation_down;
	end modify_status;










	
	procedure propose_units (
		module_cursor	: in pac_generic_modules.cursor;
		catch_zone		: in type_catch_zone;
		count			: in out natural;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			device_cursor : pac_devices_sch.cursor := module.devices.first;

			procedure query_device (
				device_name	: in type_device_name;
				device		: in out type_device_sch)
			is 
				unit_cursor : pac_units.cursor := device.units.first;


				procedure query_unit (
					unit_name	: in pac_unit_name.bounded_string;
					unit		: in out type_unit)
				is begin
					if in_catch_zone (unit, catch_zone, active_sheet) then
						log_indentation_up;
						log (text => to_string (unit_name), level => log_threshold + 2);
						log_indentation_down;
						
						set_proposed (unit);
						count := count + 1;
					end if;
				end query_unit;

				
			begin
				log (text => to_string (device_name), level => log_threshold + 1);
				log_indentation_up;

				-- Iterate through the units:
				while has_element (unit_cursor) loop
					device.units.update_element (unit_cursor, query_unit'access);
					next (unit_cursor);
				end loop;
				log_indentation_down;
			end query_device;

			
		begin
			-- Iterate through the devices:
			while has_element (device_cursor) loop
				module.devices.update_element (device_cursor, query_device'access);
				next (device_cursor);
			end loop;
		end query_module;
		

	begin
		log (text => "module " & to_string (module_cursor)
			& " proposing units in " & to_string (catch_zone),
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,		   
			process		=> query_module'access);

		log_indentation_down;
	end propose_units;







	procedure reset_proposed_units (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			
			procedure query_device (
				device_name	: in type_device_name;
				device		: in out type_device_sch)
			is 
				unit_cursor : pac_units.cursor := device.units.first;


				procedure query_unit (
					unit_name	: in pac_unit_name.bounded_string;
					unit		: in out type_unit)
				is begin
					log (text => to_string (unit_name), level => log_threshold + 2);
					reset_status (unit);
				end query_unit;

										 
			begin
				log (text => to_string (device_name), level => log_threshold + 1);
				log_indentation_up;

				-- Iterate through the units:
				while has_element (unit_cursor) loop
					device.units.update_element (unit_cursor, query_unit'access);
					next (unit_cursor);
				end loop;
				
				log_indentation_down;
			end query_device;

			
			device_cursor : pac_devices_sch.cursor := module.devices.first;
			
		begin
			-- Iterate through the devices:
			while has_element (device_cursor) loop
				module.devices.update_element (device_cursor, query_device'access);
				next (device_cursor);
			end loop;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " resetting proposed units", 
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end reset_proposed_units;







	function get_first_unit (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;
		log_threshold	: in type_log_level)
		return type_object_unit
	is
		result : type_object_unit;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			device_cursor : pac_devices_sch.cursor := module.devices.first;

			proceed : aliased boolean := true;


			procedure query_device (
				device_name	: in type_device_name;
				device 		: in type_device_sch)
			is 

				procedure query_unit (unit_cursor : in pac_units.cursor) is begin
					case flag is
						when PROPOSED =>
							if is_proposed (unit_cursor) then
								result.device_cursor := device_cursor;
								result.unit_cursor := unit_cursor;
								proceed := false; -- no further probing required
							end if;
		
						when SELECTED =>
							if is_selected (device_cursor) then
								result.device_cursor := device_cursor;
								result.unit_cursor := unit_cursor;
								proceed := false; -- no further probing required
							end if;
		
						when others => null; -- CS
					end case;
				end query_unit;

				
			begin
				-- Iterate through the units:
				iterate (device.units, query_unit'access, proceed'access);
			end query_device;
			
			
		begin
			-- Iterate through the devices:
			while has_element (device_cursor) and proceed loop
				pac_devices_sch.query_element (device_cursor, query_device'access);
				next (device_cursor);
			end loop;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " looking up the first unit /" & to_string (flag),
			level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;

		return result;
	end get_first_unit;




------------------------------------------------------------------------------------------

-- OBJECTS:
	

	function get_count (
		objects : in pac_objects.list)
		return natural
	is begin
		return natural (objects.length);
	end get_count;
	
	


	function get_first_object (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;								 
		log_threshold	: in type_log_level)
		return type_object
	is
		result_category : type_object_category := CAT_VOID;
		result_unit 	: type_object_unit;

	begin
		log (text => "module " & to_string (module_cursor)
			& " looking up the first object / " & to_string (flag),
			level => log_threshold);

		log_indentation_up;

		
		-- SEARCH FOR A UNIT:
		
		-- If a unit has been found, then go to the end of this procedure:
		result_unit := get_first_unit (module_cursor, flag, log_threshold + 1);

		if has_element (result_unit.unit_cursor) then
			-- A unit has been found.
			log (text => get_object_name (result_unit),
				 level => log_threshold + 1);
			
			result_category := CAT_UNIT;
		end if;
		
		-- If nothing has been found then the category is CAT_VOID.
		log_indentation_down;

		
		case result_category is
			when CAT_VOID =>
				return (cat => CAT_VOID);

			when CAT_UNIT =>
				return (CAT_UNIT, result_unit);

		end case;
	end get_first_object;





	function get_objects (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;
		log_threshold	: in type_log_level)
		return pac_objects.list
	is
		use pac_objects;

		-- Here the objects are collected:
		result : pac_objects.list;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			device_cursor : pac_devices_sch.cursor := module.devices.first;
			

			procedure query_device (
				name	: in type_device_name;
				device	: in type_device_sch) 
			is 
				unit_cursor : pac_units.cursor := device.units.first;


				-- This procedure appends the matching
				-- device and unit cursor to the result:
				procedure collect is begin
					result.append ((
						cat		=> CAT_UNIT,
						unit	=> (device_cursor, unit_cursor)));

				end collect;

				
				procedure query_unit (c : in pac_units.cursor) is begin
					case flag is
						when PROPOSED =>
							if is_proposed (c) then
								collect;
							end if;

						when SELECTED =>
							if is_selected (c) then
								collect;
							end if;

						when others => null; -- CS
					end case;					
				end query_unit;
		
				
			begin
				device.units.iterate (query_unit'access);
			end query_device;

			
		begin
			-- Iterate the units of the module:
			while has_element (device_cursor) loop
				query_element (device_cursor, query_device'access);
				next (device_cursor);
			end loop;
		
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " looking up objects / " & to_string (flag),
			level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module_cursor,
			process		=> query_module'access);
		
		log_indentation_down;

		return result;
	end get_objects;






	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level)
	is begin
		log (text => "module " & to_string (module_cursor)
			& " modifying status of object"
			& type_object_category'image (object.cat)
			& " / " & to_string (operation),
			level => log_threshold);

		log_indentation_up;
		
		case object.cat is
			when CAT_UNIT =>
				modify_status (module_cursor, object.unit, operation, log_threshold + 1);

			when CAT_VOID =>
				null; -- CS
		end case;

		log_indentation_down;
	end modify_status;






	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		object_cursor	: in pac_objects.cursor;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level)
	is 
		use pac_objects;
		object : constant type_object := element (object_cursor);
	begin
		modify_status (module_cursor, object, operation, log_threshold);
	end modify_status;

	




	procedure reset_proposed_objects (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is begin
		log (text => "module " & to_string (module_cursor) &
			" resetting proposed objects",
			level => log_threshold);

		log_indentation_up;
		reset_proposed_units (module_cursor, log_threshold + 1);
		log_indentation_down;
	end reset_proposed_objects;



	
	
end et_schematic_ops.units;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

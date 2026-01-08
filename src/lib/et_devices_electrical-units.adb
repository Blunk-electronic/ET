------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                     DEVICES ELECTRICAL / UNITS                           --
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
-- <http://www.gnu.org/licenses/>.   
------------------------------------------------------------------------------

--   For correct displaying set tab width in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with ada.text_io;					use ada.text_io;
with ada.characters.latin_1;
with ada.characters.handling;
with ada.strings.unbounded;
with ada.exceptions;

with et_devices_electrical.packages;

with et_symbol_library;
with et_symbol_text;
with et_symbol_name;



package body et_devices_electrical.units is


	
	
	function get_unit_names_deployed (
		device : in type_device_electrical)
		return pac_unit_names.list
	is 
		result : pac_unit_names.list;

		procedure query_unit (c : in pac_units.cursor) is begin
			result.append (key (c));
		end;

	begin
		device.units.iterate (query_unit'access);
		return result;
	end;

	



	
	function get_unit_count (
		device : in type_device_electrical)
		return natural
	is 
		result : natural;
		cursor : pac_device_models.cursor;
	begin
		-- Get a cursor to the device in the library.
		-- Map from schematic cursor to library cursor:
		cursor := get_device_model (device);

		-- Get the number of units from the device model:
		result := get_unit_count (cursor);

		return result;
	end;

	

	
	function get_unit_count_deployed (
		device : in type_device_electrical)
		return natural
	is begin
		return natural (device.units.length);
	end;



	

	

	function get_first_unit (
		device : in type_device_electrical)
		return pac_units.cursor
	is begin
		return device.units.first;
	end;



	
	
	

	function locate_unit (
		device	: in type_device_electrical;
		unit	: in pac_unit_name.bounded_string)
		return pac_units.cursor
	is begin
		return find (device.units, unit);
	end;


	


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





	function get_unit_names_deployed (
		device : in pac_devices_electrical.cursor)
		return pac_unit_names.list
	is begin
		return get_unit_names_deployed (element (device));
	end;


	
	

	
	function get_unit_count (
		device : in pac_devices_electrical.cursor)
		return type_unit_count
	is 
		result : type_unit_count := 1;
		
		cursor : pac_device_models.cursor;
	begin
		-- Get a cursor to the device in the library.
		-- Map from schematic cursor to library cursor:
		cursor := get_device_model (device);

		-- Get the number of units from the device model:
		result := get_unit_count (cursor);
		
		return result;
	end get_unit_count;



	

	function get_unit_count_deployed (
		device : in pac_devices_electrical.cursor)
		return natural
	is begin
		return get_unit_count_deployed (element (device));
	end;


	





	function get_full_name (
		device	: in pac_devices_electrical.cursor;
		unit	: in pac_units.cursor)
		return string
	is
		d_name : string := get_device_name (device);
		u_name : string := get_unit_name (unit);

		u_count : type_unit_count;
	begin
		u_count := get_unit_count (device);

		if u_count > 1 then
			return d_name & device_unit_separator & u_name; -- IC3,A
		else
			return d_name; -- IC3
		end if;
	end get_full_name;


	


	
	function get_full_name (
		device		: in type_device_name;
		unit		: in pac_unit_name.bounded_string;
		unit_count	: in type_unit_count)
		return string 
	is begin
		if unit_count > 1 then
			return to_string (device) 
				& device_unit_separator 
				& pac_unit_name.to_string (unit);
		else
			return to_string (device);
		end if;
	end get_full_name;









	function get_position (
		device	: in pac_devices_electrical.cursor; -- R2
		unit	: in pac_units.cursor)
		return type_object_position
	is
		unit_position : type_object_position;

		
		procedure query_unit (
			device_name	: in type_device_name;
			device		: in type_device_electrical)
		is begin
			-- get the coordinates of the unit
			unit_position := element (unit).position;
		end query_unit;
		
		
	begin
		query_element (
			position	=> device,
			process		=> query_unit'access);

		return unit_position;
	end get_position;





	function get_sheet (
		device	: in pac_devices_electrical.cursor; -- R2
		unit	: in pac_units.cursor) -- A, B, C
		return type_sheet
	is 
		position : type_object_position;
	begin
		position := get_position (device, unit);
		return get_sheet (position);
	end get_sheet;

	


	
	function get_unit_position (
		device_cursor	: in pac_devices_electrical.cursor;
		unit_name		: in pac_unit_name.bounded_string)
		return type_unit_query
	is 
		exists : boolean := false;

		position : type_object_position;
		
		
		procedure query_device (
			device_name	: in type_device_name;
			device		: in type_device_electrical)
		is 
			unit_cursor : pac_units.cursor;
		begin
			-- Locate the given unit in the device.
			-- If the unit does not exist, then
			-- unit_cursor will be pointing to it:
			unit_cursor := device.units.find (unit_name);

			-- Get the coordinates of the unit.
			if has_element (unit_cursor) then
				position := get_position (unit_cursor);
				exists := true;
			end if;
		end query_device;
		

	begin
		query_element (device_cursor, query_device'access);
		
		if exists then
			return (exists => true, position => position);
		else
			return (exists => false);
		end if;
	end get_unit_position;



	


	function get_unit_positions (
		device_cursor : in pac_devices_electrical.cursor) 
		return pac_unit_positions.map 
	is
		-- temporarily storage of unit coordinates:
		positions : pac_unit_positions.map;
		
		procedure get_positions (
			device_name : in type_device_name;
			device		: in type_device_electrical) 
		is begin
			positions := get_unit_positions (device.units);
		end;

	begin
		pac_devices_electrical.query_element (
			position	=> device_cursor,
			process		=> get_positions'access);

		return positions;
	end;

	


	
	procedure log_unit_positions (
		positions 		: in pac_unit_positions.map;
		log_threshold	: in type_log_level) 
	is
		
		procedure write (cursor : in pac_unit_positions.cursor) is 
			use pac_unit_name;
		begin
			log (text => 
				"unit " &
				to_string (pac_unit_positions.key (cursor)) & -- unit name
				to_string (position => pac_unit_positions.element (cursor)), -- sheet x y
				level => log_threshold);
		end;
		
	begin
		log (text => "location(s) in schematic:", level => log_threshold);
		log_indentation_up;
		pac_unit_positions.iterate (positions, write'access);
		log_indentation_down;
	end;





	function get_port_positions (
		device	: in pac_devices_electrical.cursor;
		unit	: in pac_units.cursor)
		return pac_points.list
	is
		-- This list of location vectors (x/y-positions) 
		-- will be returned to the caller:
		result : pac_points.list;
		
		-- The given device and unit exist in the schematic.
		-- Both provide information about the name and position
		-- of device and unit in the schematic and the associated
		-- device model. The device model in turn provides information
		-- about the ports and their default positions.
		-- So the device model is the link between schematic unit
		-- and port positions.

		use pac_device_models;

		-- This cursor points to the device model in the device library:
		device_cursor_lib : pac_device_models.cursor := get_device_model (device);
		-- CS: constraint_error will arise here if no associated device exists.

		-- In order to locate a unit inside the device model,
		-- the actual name of the unit must be known.
		-- So we translate the given unit cursor to a unit name
		-- like A, C, IO_BANK_1:
		unit_name : pac_unit_name.bounded_string := key (unit);


		
		procedure query_internal_units (
			model_name		: in pac_device_model_file.bounded_string;
			device_model	: in type_device_model) 
		is
			unit_cursor : pac_units_internal.cursor;			
		begin
			-- Locate the given unit in the device model
			-- among the internal units. If it has been found,
			-- the the unit_cursor will be set so that it points
			-- to the internal unit:
			locate_internal (device_model, unit_name, unit_cursor);

			-- Get the port positions of the internal unit.
			result := get_port_positions (unit_cursor);
		end query_internal_units;


		
		procedure query_external_units (
			model_name		: in pac_device_model_file.bounded_string;
			device_model	: in type_device_model) 
		is
			unit_cursor : pac_units_external.cursor;
		begin
			-- Locate the given unit in the device model
			-- among the external units. If it has been found,
			-- the the unit_cursor will be set so that it points
			-- to the external unit:
			locate_external (device_model, unit_name, unit_cursor);

			-- Get the port positions of the external unit.
			result := get_port_positions (unit_cursor);
		end query_external_units;


		-- Get the position of the given unit in the schematic:
		unit_position : type_object_position := get_position (unit);
		
	begin
		-- Locate the given unit among the external units 
		-- in the device model. Since it is most likely that
		-- the unit is among the external units, we do this step first:
		query_element (device_cursor_lib, query_external_units'access);

		-- If no positions found, the the given unit either does not
		-- exist or is not external. In that case the unit must
		-- be located among the internal units:
		if get_length (result) = 0 then

			-- Query internal units of device model:
			query_element (device_cursor_lib, query_internal_units'access);
		end if;

		-- Rotate the port positions by the rotation of the unit:
		rotate_points (result, get_rotation (unit_position));

		-- CS mirror ?
		
		-- Move the port positions by the position of the unit:		
		move_points (result, get_place (unit_position));
		
		return result;
	end get_port_positions;



	
	

	function get_ports_of_unit (
		device_cursor	: in pac_devices_electrical.cursor;
		unit_name		: in pac_unit_name.bounded_string)
		return pac_symbol_ports.map 
	is
		ports : pac_symbol_ports.map; -- to be returned
		device_cursor_lib : pac_device_models.cursor;
	begin
		device_cursor_lib := get_device_model (device_cursor);
		ports := get_ports_of_unit (device_cursor_lib, unit_name);
		
		return ports;
	end get_ports_of_unit;





	function get_ports_of_unit (
		device_cursor	: in pac_devices_electrical.cursor;
		unit_cursor		: in pac_units.cursor)
		return pac_symbol_ports.map
	is
		ports : pac_symbol_ports.map;

		position : type_object_position;
		rotation : type_rotation_model;
	begin
		-- Get the default positions as described in the 
		-- device model:
		ports := get_ports_of_unit (device_cursor, key (unit_cursor));

		
		-- Rotate and move the ports according to the 
		-- actual rotation and position of the unit in the schematic:
		position := get_position (unit_cursor);
		rotation := get_rotation (position);

		if rotation /= zero_rotation then
			rotate_ports (ports, rotation);
		end if;

		move_ports (ports, position);
		
		return ports;
	end get_ports_of_unit;











	function get_position (
		device		: in pac_devices_electrical.cursor; -- R2
		unit		: in pac_units.cursor;
		category	: in type_placeholder_meaning)
		return type_vector_model
	is
		placeholder_position : type_vector_model; -- to be returned

		unit_position : type_object_position;

		
		procedure query_unit (
			device_name	: in type_device_name;
			device		: in type_device_electrical)
		is begin
			-- get the coordinates of the unit
			unit_position := element (unit).position;

			-- get the coordinates of the placeholder:
			case category is
				when NAME =>
					placeholder_position := element (unit).placeholders.name.position;

				when PURPOSE =>
					placeholder_position := element (unit).placeholders.purpose.position;

				when VALUE =>
					placeholder_position := element (unit).placeholders.value.position;
			end case;

			move_by (placeholder_position, unit_position.place);
			
		end query_unit;

		
	begin
		query_element (
			position	=> device,
			process		=> query_unit'access);

		return placeholder_position;
	end get_position;





-- 	procedure add_first_available_unit (
-- 		device			: in pac_devices_electrical.cursor; -- IC2
-- 		log_threshold	: in type_log_level)
-- 	is
-- 		--- The pointer to the device model:		
-- 		device_cursor_lib : pac_device_models.cursor;
-- 
-- 
-- 	begin
-- 		log (text => "Add first available unit to device " 
-- 			 & to_string (get_device_name (device),
-- 			  level => log_threshold);
-- 			
-- 		-- Locate the device model:
-- 		device_cursor_lib := get_device_model (device_cursor_sch);
-- 
-- 	end add_first_available_unit;

	


	
	






-- PLACEHOLDERS
	


	function get_default_text_positions (
		device_cursor	: in pac_devices_electrical.cursor;
		unit_name		: in pac_unit_name.bounded_string)
		return type_default_text_positions 
	is		
		use pac_device_models;

		-- The positions to be returned depend on the appearance of the requested device:
		result : type_default_text_positions (element (device_cursor).appearance); -- to be returned
		
		model : pac_device_model_file.bounded_string; -- ../libraries/devices/transistor/pnp.dev
		device_cursor_lib : pac_device_models.cursor;

		use et_symbol_text;
		use pac_symbol_texts;

		
		procedure query_text (c : in pac_symbol_texts.cursor) is 
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
						result.placeholders.name	:= element (unit_cursor).symbol.placeholders.name;
						result.placeholders.value	:= element (unit_cursor).symbol.placeholders.value;
						result.placeholders.purpose	:= element (unit_cursor).symbol.placeholders.purpose;
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
			use et_symbol_library;
			use et_symbol_name;
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
						result.placeholders.name 	:= symbol.placeholders.name;
						result.placeholders.value	:= symbol.placeholders.value;
						result.placeholders.purpose	:= symbol.placeholders.purpose;
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
				pac_symbol_models.query_element (
					position	=> pac_symbol_models.find (symbol_library, sym_model),
					process		=> query_symbol'access);
			else
				unit_status := INT;
			end if;
			
		end query_external_units;

		
	begin

		-- Fetch the model name of the given device. 
		model := pac_devices_electrical.element (device_cursor).model;

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




	

	

	






-- PROPERTIES QUERIES:
	
	

	function get_unit_properties (
		unit_cursor	: in pac_units.cursor;
		level		: in type_properties_level;
		linebreaks	: in boolean := false)
		return string
	is
		use et_coordinates_formatting;
		use ada.strings.unbounded;
		result : unbounded_string;


		-- If linebreaks are requested by the caller, then
		-- this function returns a linefeed character on each call.
		-- If no linefeeds are requested, then the return is an empty string:
		function ins_LF return string is 
			use ada.characters.latin_1;
		begin
			if linebreaks then
				return "" & LF;
			else
				return "";
			end if;
		end;
		
		
		unit : type_unit renames element (unit_cursor);

	begin
		case level is
			when DEVICE_PROPERTIES_LEVEL_1 =>
				result := result & get_unit_name (unit_cursor) & ins_LF;

			when others =>
				result := result & get_unit_name (unit_cursor)
						  & " position: " & to_string (get_position (unit), FORMAT_2) & ins_LF;

			-- CS symbol model
		end case;
				
		return to_string (result);
	end;




	
	
	function get_device_properties (
		device		: in type_device_electrical;
		level		: in type_properties_level;
		linebreaks	: in boolean := false)
		return string
	is
		use et_devices_electrical.packages;
		
		use ada.strings.unbounded;
		result : unbounded_string;


		-- If linebreaks are requested by the caller, then
		-- this function returns a linefeed character on each call.
		-- If no linefeeds are requested, then the return is an empty string:
		function ins_LF return string is 
			use ada.characters.latin_1;
		begin
			if linebreaks then
				return "" & LF;
			else
				return "";
			end if;
		end;
		

		-- CS: Refinement required about what
		-- should be output on which level.

		
		procedure get_info_1 is begin
			if is_real (device) then
				if has_value (device) then
					result := to_unbounded_string (" value: " 
						& get_value (device) & ins_LF);
				end if;

				if has_purpose (device) then
					result := result & " purpose: " 
						& get_purpose (device) & ins_LF;
				end if;
			end if;
		end;


		procedure get_info_2 is begin
			if is_real (device) then
				if has_partcode (device) then
					result := result & " partcode: " 
						& get_partcode (device) & ins_LF;
				end if;

				-- CS write interactive status true/false
				-- CS package variant
			end if;
		end;


		procedure get_info_3 is begin
			result := result & " device model: " 
				& to_string (get_device_model_file (device)) & ins_LF;

			if is_real (device) then
				result := result & " package model: " 
					& to_string (get_package_model_name (device)) & ins_LF; 
			end if;
		end;


		
	begin
		case level is
			when DEVICE_PROPERTIES_LEVEL_1 =>
				get_info_1;

			when DEVICE_PROPERTIES_LEVEL_2 =>
				get_info_1; 
				get_info_2; 

			when DEVICE_PROPERTIES_LEVEL_3 =>
				get_info_1; 
				get_info_2; 
				get_info_3;
		end case;

		return to_string (result);
	end get_device_properties;


	





	
	
	-- function get_unit_properties (
	-- 	device		: in type_device_electrical;
	-- 	level		: in type_properties_level;
	-- 	all_units	: in boolean := true;
	-- 	unit		: in pac_unit_name.bounded_string := unit_name_default)
	-- 	return string
	-- is
	-- begin
	-- 	return "";
	-- end;


	

	function get_properties (
		device_cursor	: in pac_devices_electrical.cursor;
		level			: in type_properties_level;
		all_units		: in boolean := true;
		unit_cursor		: in pac_units.cursor := pac_units.no_element;
		linebreaks		: in boolean := false)
		return string
	is
		use ada.strings.unbounded;
		units_info : unbounded_string;

		device : type_device_electrical renames element (device_cursor);


		-- If linebreaks are requested by the caller, then
		-- this function returns a linefeed character on each call.
		-- If no linefeeds are requested, then the return is an empty string:
		function ins_LF return string is 
			use ada.characters.latin_1;
		begin
			if linebreaks then
				return "" & LF;
			else
				return "";
			end if;
		end;

		

		-- This procedure iterates through the units and collects
		-- information in units_info:
		procedure get_units_info is

			procedure query_device (
				device_name	: in type_device_name;
				device		: in type_device_electrical)
			is 
				unit_cursor : pac_units.cursor := device.units.first;
			begin
				-- Iterate through the units and collect properties:
				while has_element (unit_cursor) loop					
					units_info := units_info & " unit: " 
						& get_unit_properties (unit_cursor, level, linebreaks) & ins_LF;
					
					next (unit_cursor);
				end loop;
			end query_device;
	
		begin
			query_element (device_cursor, query_device'access);
		end get_units_info;
		
		
	begin
		if all_units then
			
			-- Collect information of all units:
			get_units_info;

			-- Get general properties of the device.
			-- Append the properties of the units:
			return "device: " & get_device_name (device_cursor) & ins_LF
				& get_device_properties (device, level, linebreaks) & ins_LF
				& to_string (units_info);
		else

			-- Get general properties of the device.
			-- Append the properties of the requested unit:
			return "device: " & get_device_name (device_cursor) & ins_LF
				& get_device_properties (device, level, linebreaks) & ins_LF
				& " unit: " & get_unit_properties (unit_cursor, level, linebreaks);

		end if;
	end get_properties;



	
	

	function get_port (
		device		: in pac_devices_electrical.cursor;
		terminal	: in pac_terminal_name.bounded_string)
		return type_get_port_result
	is		
		-- CS: 
		-- simplify header as in function get_terminal
		-- use function et_devices.get_unit_and_port
		
		result : type_get_port_result;

		-- Get the cursor to the full device model in the library:
		device_model : constant pac_device_models.cursor := 
			get_device_model (pac_devices_electrical.element (device).model);

		-- This is the package variant used by the given device:
		variant_sch : constant pac_package_variant_name.bounded_string :=
			pac_devices_electrical.element (device).variant; -- N, D


		use et_device_model;
		use et_device_model_names;

		
		procedure query_model (
			model	: in pac_device_model_file.bounded_string;
			device	: in type_device_model)
		is
			use pac_package_variants;

			-- Locate the package variant of the given device
			-- in the device model:
			variant_lib : constant pac_package_variants.cursor := 
				find (device.variants, variant_sch);

			
			procedure query_terminal_port_map (
				name	: in pac_package_variant_name.bounded_string;
				variant	: in type_package_variant)
			is
				use pac_terminal_port_map;

				-- Locate the terminal in the terminal-port-map
				-- of the device model:
				t : constant pac_terminal_port_map.cursor :=
					find (variant.terminal_port_map, terminal);
			begin
				-- Get the port and unit name (which is what we want):
				if t /= pac_terminal_port_map.no_element then
					result := (
						linked	=> TRUE, 
						unit	=> element (t).unit, 
						port	=> element (t).name);
				else
					-- If the terminal can not be found in the map then
					-- it is not linked to any port.
					result := (linked => FALSE);
				end if;
			end query_terminal_port_map;

			
		begin
			query_element (variant_lib, query_terminal_port_map'access);

			-- CS result := get_unit_and_port (variant_lib, terminal);
		end query_model;

		
	begin
		pac_device_models.query_element (device_model, query_model'access);
			
		return result;
	end get_port;

	
	

	



	function to_string (
		mode : in type_device_search_mode)
		return string
	is begin
		return type_device_search_mode'image (mode);
	end;



	

	procedure select_unit (
		device		: in out type_device_electrical;
		all_units	: in boolean;
		unit_name	: in pac_unit_name.bounded_string)
	is
		unit_cursor : pac_units.cursor := device.units.first;

		
		procedure query_unit (
			unit_name	: in pac_unit_name.bounded_string;
			unit		: in out type_unit)
		is begin
			set_selected (unit);
		end query_unit;

		
	begin
		if all_units then
			-- Iterate though all units and set each of them
			-- as selected:
			while has_element (unit_cursor) loop
				device.units.update_element (unit_cursor, query_unit'access);
				next (unit_cursor);
			end loop;

		else
			-- Set only the given unit as selected:
			unit_cursor := locate_unit (device, unit_name);
			device.units.update_element (unit_cursor, query_unit'access);
		end if;
	end select_unit;

	
	
end et_devices_electrical.units;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

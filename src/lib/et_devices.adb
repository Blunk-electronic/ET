------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                         DEVICES IN LIBRARIES                             --
--                                                                          --
--                              B o d y                                     --
--                                                                          --
-- Copyright (C) 2017 - 2024                                                --
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

with ada.text_io;				use ada.text_io;

with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;

with ada.strings; 				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;

with et_string_processing;		use et_string_processing;
with et_exceptions;				use et_exceptions;


package body et_devices is

	
	function to_full_name (
		device_name	: in type_device_name; -- IC34
		symbol_name	: in pac_unit_name.bounded_string; -- PWR
		unit_count	: in type_unit_count) -- the total number of units
		return string is -- IC34.PWR
	begin
		if unit_count > 1 then
			return to_string (device_name) 
				& device_unit_separator 
				& to_string (symbol_name);
		else
			return to_string (device_name);
		end if;
	end to_full_name;

	

	
	
	function to_string (package_variant : in pac_package_variant_name.bounded_string) return string is begin
		return pac_package_variant_name.to_string (package_variant);
	end;


	
	function to_variant_name (variant_name : in string) 
		return pac_package_variant_name.bounded_string
	is begin
		return pac_package_variant_name.to_bounded_string (variant_name);
	end;


	
	procedure check_variant_name_length (variant_name : in string) is
	-- tests if the given variant name is not longer than allowed
	begin
		if variant_name'length > variant_name_length_max then
			log (WARNING, "variant name too long. Max. length is" 
				 & positive'image (variant_name_length_max) & " !");
		end if;
	end check_variant_name_length;


	
	procedure check_variant_name_characters (
		variant		: in pac_package_variant_name.bounded_string;
		characters	: in character_set := variant_name_characters) 
	is
		invalid_character_position : natural := 0;
	begin
		-- Test given variant name and get position of possible invalid characters.
		invalid_character_position := index (
			source => variant,
			set => characters,
			test => outside);

		-- Evaluate position of invalid character.
		if invalid_character_position > 0 then
			log (WARNING, "invalid character in variant name " 
				& to_string (variant) & " at position" & natural'image (invalid_character_position));
		end if;
	end check_variant_name_characters;


	

	function get_unit_and_port (
		variant		: in pac_variants.cursor;
		terminal	: in pac_terminal_name.bounded_string)
		return type_get_port_result
	is
		result : type_get_port_result;

		use pac_variants;
		
		procedure query_terminal_port_map (
			name	: in pac_package_variant_name.bounded_string;
			variant	: in type_variant)
		is
			-- Locate in the given package variant the given terminal:
			use pac_terminal_port_map;
			c : pac_terminal_port_map.cursor := 
				find (variant.terminal_port_map, terminal);
		begin
			if c /= pac_terminal_port_map.no_element then -- terminal exists
				result := (
					linked	=> TRUE,
					unit	=> element (c).unit,
					port	=> element (c).name);
			else
				-- If the terminal can not be found in the map then
				-- it is not linked to any port:
				result := (linked => FALSE);
			end if;
		end query_terminal_port_map;
								
	begin
		query_element (variant, query_terminal_port_map'access);
		return result;
	end get_unit_and_port;

	
	
	function get_terminal (
		variant	: in pac_variants.cursor;
		unit	: in pac_unit_name.bounded_string;
		port	: in pac_port_name.bounded_string)
		return pac_terminal_name.bounded_string
	is
		use pac_terminal_name;
		result : pac_terminal_name.bounded_string;

		use pac_variants;
		
		procedure query_terminal_port_map (
			name	: in pac_package_variant_name.bounded_string;
			variant	: in type_variant)
		is
			use pac_port_name;
			use pac_terminal_port_map;
			c : pac_terminal_port_map.cursor := variant.terminal_port_map.first;
		begin
			while c /= pac_terminal_port_map.no_element loop
				if element (c).unit = unit and then element (c).name = port then 
					result := key (c);
					exit;
				end if;
				next (c);
			end loop;
		end query_terminal_port_map;
								
	begin
		query_element (variant, query_terminal_port_map'access);

		-- Raise exception if no terminal has been found:
		if length (result) = 0 then
			raise semantic_error_1 with "No terminal found !";
			-- CS output variant name, unit and port
		end if;
		
		return result;
	end get_terminal;

	
	
	function to_string (
		terminal	: in type_terminal;
		show_unit	: in boolean := false;
		preamble	: in boolean := true)
		return string is
	-- Returns the given terminal as string. 
	-- If show_unit is true, the unit name is output.
	-- If preamble is true, each property of the terminal is headed by a short preamble.
	begin
		case preamble is
			when true =>
				case show_unit is
					when true =>
						return (" port " & to_string (port => terminal.port) 
							& " unit " & to_string (terminal.unit)
							& " terminal " & to_string (terminal.name)
							& latin_1.space);
						
					when false =>
						return (" port " & to_string (port => terminal.port) 
							& " terminal " & to_string (terminal.name)
							& latin_1.space);
				end case;
						
			when false =>
				case show_unit is
					when true =>
						return (latin_1.space & to_string (port => terminal.port) 
							& latin_1.space & to_string (terminal.unit)
							& latin_1.space & to_string (terminal.name)
							& latin_1.space);
						
					when false =>
						return (latin_1.space & to_string (port => terminal.port) 
							& latin_1.space & to_string (terminal.name)
							& latin_1.space);
				end case;
		end case;
		
	end to_string;



	
	function rotate_placeholders (
		symbol_cursor	: in pac_units_internal.cursor;
		destination		: in et_coordinates_2.type_position)
		return type_rotated_placeholders
	is
		use pac_units_internal;
		use et_coordinates_2.pac_geometry_sch;

		r : type_rotated_placeholders; -- to be returned
	begin
		r.name		:= element (symbol_cursor).symbol.name;
		r.value		:= element (symbol_cursor).symbol.value;
		r.purpose	:= element (symbol_cursor).symbol.purpose;

		-- rotate the positions of placeholders according to rotation given by caller:
		rotate (r, get_rotation (destination));
		
		return r;
	end rotate_placeholders;




	--function hash_device_model (
		--model	: in pac_device_model_file.bounded_string)
		--return hash_type
	--is 
	--begin
		--return ada.strings.hash (to_string (model));
	--end hash_device_model;


	--function equivalent_models (
		--d1, d2 : in type_device_lib)
		--return boolean
	--is begin
		--return d1 = d2;
	--end equivalent_models;


	function is_real (
		device_cursor : in pac_devices_lib.cursor)
		return boolean
	is 
		use pac_devices_lib;
	begin
		case element (device_cursor).appearance is
			when APPEARANCE_VIRTUAL => return false;
			when APPEARANCE_PCB => return true;
		end case;
	end is_real;

	

	
	function provides_unit (
		device_cursor	: in pac_devices_lib.cursor;
		unit_name		: in pac_unit_name.bounded_string)
		return boolean
	is
		found : boolean := false;
		
		procedure query_internal (
			model	: in pac_device_model_file.bounded_string;
			device	: in type_device_lib)
		is begin
			if device.units_internal.contains (unit_name) then
				found := true;
			end if;
		end query_internal;

		procedure query_external (
			model	: in pac_device_model_file.bounded_string;
			device	: in type_device_lib)
		is begin
			if device.units_external.contains (unit_name) then
				found := true;
			end if;
		end query_external;
		
	begin -- provided_unit
		-- First search among internal units.
		-- If not found there, search among external units.
		
		pac_devices_lib.query_element (device_cursor, query_internal'access);

		if not found then
			pac_devices_lib.query_element (device_cursor, query_external'access);
		end if;
		
		return found;
	end provides_unit;

	

	
	function first_unit (
		device_cursor : in pac_devices_lib.cursor) 
		return type_device_units
	is
		cursors : type_device_units; -- to be returned
		
		use pac_devices_lib;
		use pac_units_internal;
		use pac_units_external;

		
		procedure query_units (
			device_name	: in pac_device_model_file.bounded_string;
			device		: in type_device_lib) is

			function first_internal (add_level : in type_add_level) 
				return pac_units_internal.cursor is
			-- Searches for a unit with given add_level. Returns the cursor of that unit.
			-- If no suitable unit found, returns cursor with no_element.
				cursor : pac_units_internal.cursor := device.units_internal.first;
			begin
				while cursor /= pac_units_internal.no_element loop
					if element (cursor).add_level = add_level then
						return cursor; -- unit found, no further search required. exit prematurely.
					end if;
					next (cursor);
				end loop;
				-- no unit found. return no_element:
				return pac_units_internal.no_element;
			end;

			function first_external (add_level : in type_add_level) 
				return pac_units_external.cursor is
			-- Searches for a unit with given add_level. Returns the cursor of that unit.
			-- If no suitable unit found, returns cursor with no_element.
				cursor : pac_units_external.cursor := device.units_external.first;
			begin
				while cursor /= pac_units_external.no_element loop
					if element (cursor).add_level = add_level then
						return cursor; -- unit found, no further search required. exit prematurely.
					end if;
					next (cursor);
				end loop;
				-- no unit found. return no_element:
				return pac_units_external.no_element;
			end;
			
		begin -- query_units
			-- First search among the internal units for a MUST-unit:
			cursors.int := first_internal (MUST);

			-- if no MUST-unit found, search for an ALWAYS-unit:
			if cursors.int = pac_units_internal.no_element then
				cursors.int := first_internal (ALWAYS);

				-- if no ALWAYS-unit found, search for a NEXT-unit:
				if cursors.int = pac_units_internal.no_element then
					cursors.int := first_internal (NEXT);

					-- if no NEXT-unit found, search for a REQUEST-unit
					if cursors.int = pac_units_internal.no_element then
						cursors.int := first_internal (REQUEST);

						-- if no REQUEST-unit found, search for a CAN-unit
						if cursors.int = pac_units_internal.no_element then
							cursors.int := first_internal (et_unit_add_level.CAN);
						end if;
					end if;					
				end if;
			end if;

			-- if no suitable internal unit found, search among the external units:
			if cursors.int = pac_units_internal.no_element then

				-- search among the external units for a MUST-unit
				cursors.ext := first_external (MUST);

				-- if no MUST-unit found, search for an ALWAYS-unit:
				if cursors.ext = pac_units_external.no_element then
					cursors.ext := first_external (ALWAYS);

					-- if no ALWAYS-unit found, search for a NEXT-unit:
					if cursors.ext = pac_units_external.no_element then
						cursors.ext := first_external (NEXT);

						-- if no NEXT-unit found, search for a REQUEST-unit
						if cursors.ext = pac_units_external.no_element then
							cursors.ext := first_external (REQUEST);

							-- if no REQUEST-unit found, search for a CAN-unit
							if cursors.ext = pac_units_external.no_element then
								cursors.ext := first_external (et_unit_add_level.CAN);
							end if;
						end if;					
					end if;
				end if;
			
				-- if no suitable external unit found, we have a problem:
				if cursors.ext = pac_units_external.no_element then
					log (ERROR, " Device model has no units !", console => true);
					raise constraint_error;
				end if;

			end if;
			
		end query_units;
		
	begin -- first_unit
		query_element (
			position	=> device_cursor,
			process		=> query_units'access);
		
		return cursors;
	end first_unit;

	

	function first_unit (
		device_cursor : in pac_devices_lib.cursor) 
		return pac_unit_name.bounded_string
	is
		fu : constant type_device_units := first_unit (device_cursor);

		use pac_units_internal;
		use pac_units_external;
		
		unit_name : pac_unit_name.bounded_string; -- to be returned
	begin
		-- The first unit is either internal or external.
		
		-- If first unit is an internal one:
		if fu.int /= pac_units_internal.no_element then

			unit_name := key (fu.int);
			
		-- If first unit is an external one:
		elsif fu.ext /= pac_units_external.no_element then
			
			unit_name := key (fu.ext);
			
		else
			raise constraint_error; -- CS should never happen. function first_unit excludes this case.
		end if;

		return unit_name;
	end first_unit;


	
	function any_unit (
		device_cursor	: in pac_devices_lib.cursor;
		unit_name		: in pac_unit_name.bounded_string)
		return type_device_units is

		cursors : type_device_units; -- to be returned
		
		use pac_devices_lib;
		use pac_units_internal;
		use pac_units_external;

		
		procedure query_units (
			device_name	: in pac_device_model_file.bounded_string;
			device		: in type_device_lib) is
		begin -- query_units
			-- First search among the internal units:
			cursors.int := device.units_internal.first;

			while cursors.int /= pac_units_internal.no_element loop
				if key (cursors.int) = unit_name then
					exit; -- unit found, no further search required. exit prematurely.
				end if;
				next (cursors.int);
			end loop;

			-- if no suitable internal unit found, search among the external units:
			if cursors.int = pac_units_internal.no_element then

				cursors.ext := device.units_external.first;

				while cursors.ext /= pac_units_external.no_element loop
					if key (cursors.ext) = unit_name then
						exit; -- unit found, no further search required. exit prematurely.
					end if;
					next (cursors.ext);
				end loop;
				
				-- if no suitable external unit found, we have a problem:
				if cursors.ext = pac_units_external.no_element then
					log (ERROR, "Unit " & to_string (unit_name) &
						 " not defined in device model !", console => true);
					
					raise semantic_error_1 with
						"ERROR: Unit " & to_string (unit_name) &
						 " not defined in device model !";
				end if;

			end if;
			
		end query_units;
		
	begin -- any_unit
		query_element (
			position	=> device_cursor,
			process		=> query_units'access);
		
		return cursors;
	end any_unit;


	
	function all_units (
		device_cursor	: in pac_devices_lib.cursor)
		return pac_unit_names.list
	is
		result : pac_unit_names.list; -- to be returned

		use pac_devices_lib;
		use pac_units_internal;
		use pac_units_external;
		
		procedure query_internal (c : in pac_units_internal.cursor) is begin
			result.append (key (c));
		end query_internal;

		procedure query_external (c : in pac_units_external.cursor) is begin
			result.append (key (c));
		end query_external;
									 
	begin
		iterate (element (device_cursor).units_internal, query_internal'access);
		iterate (element (device_cursor).units_external, query_external'access);

		return result;
	end all_units;

	

	
	function units_total (
		device_cursor	: in pac_devices_lib.cursor)
		return type_unit_count is
		use pac_devices_lib;
		use pac_units_external;
		use pac_units_internal;
		e, i : count_type;
	begin
		e := length (element (device_cursor).units_external);
		i := length (element (device_cursor).units_internal);
		return type_unit_count (e + i);
	end units_total;


	

	function get_package_variant (
		device_cursor	: in pac_devices_lib.cursor;
		variant			: in pac_package_variant_name.bounded_string)
		return pac_variants.cursor
	is 
		result : pac_variants.cursor;

		procedure query_variants (
			device_name	: in pac_device_model_file.bounded_string;
			device		: in type_device_lib) 
		is 
			use pac_variants;
			--vc : constant pac_variants.cursor := find (device.variants, variant);
		begin
			result := find (device.variants, variant);
			--if vc /= pac_variants.no_element then
				--result := vc;
			--else
				--raise semantic_error_1 with "Package variant " 
					--& enclose_in_quotes (to_string (variant)) &
					--" not defined."; 
					---- CS output model name. Mind max. string length of error message.
			--end if;
		end query_variants;
		
	begin
		query_element (device_cursor, query_variants'access);
		return result;
	end get_package_variant;


	
	
	
	function variant_available (
		device_cursor	: in pac_devices_lib.cursor;
		variant			: in pac_package_variant_name.bounded_string)  -- D, N
		return boolean is
		
		result : boolean := false; -- to be returned
		
		procedure query_variants (
			device_name	: in pac_device_model_file.bounded_string;
			device		: in type_device_lib) is
		begin
			if pac_variants.contains (device.variants, variant) then
				result := true;
			end if;
		end;
		
	begin
		pac_devices_lib.query_element (
			position	=> device_cursor,
			process		=> query_variants'access);
		
		return result;
	end variant_available;


	

	function available_variants (
		device_cursor	: in pac_devices_lib.cursor)
		return pac_variants.map
	is
		result : pac_variants.map; -- to be returned
		use pac_devices_lib;
	begin
		case element (device_cursor).appearance is
			when APPEARANCE_PCB		=> result := element (device_cursor).variants;
			when APPEARANCE_VIRTUAL	=> null;
		end case;
		
		return result;
	end available_variants;


	
	
	function locate_device (model : in pac_device_model_file.bounded_string) -- ../libraries/devices/transistor/pnp.dev
		return pac_devices_lib.cursor 
	is
		use pac_devices_lib;
		cursor : pac_devices_lib.cursor := pac_devices_lib.find (devices, model);
	begin
		return cursor;
	end;



	
	function locate_unit (
		device_cursor	: in pac_devices_lib.cursor;
		unit_name		: in pac_unit_name.bounded_string) -- like "I/O-Bank 3"
		return type_unit_cursors 
	is
		use pac_devices_lib;
		use pac_units_external;
		use pac_units_internal;

		status : type_unit_ext_int;
		
		cursor_external : pac_units_external.cursor;
		cursor_internal : pac_units_internal.cursor;

		procedure query_units (
			model	: in pac_device_model_file.bounded_string;
			device	: in type_device_lib) is
		begin
			-- Most likely the requested unit is external. So we search first in 
			-- the list of external units of the given device:
			cursor_external := find (device.units_external, unit_name);

			-- If the unit has been found, return the cursor to it:
			if cursor_external /= pac_units_external.no_element then
				status := EXT;
			else
			-- If the unit could not be found, it must be an internal unit. Search among
			-- the internal units of the given device:
				cursor_internal := find (device.units_internal, unit_name);
				status := INT;
			end if;
		end;
		
	begin -- locate_unit
		--put_line (to_string (pac_devices_lib.key (device_cursor)));

		query_element (device_cursor, query_units'access);

		case status is
			when EXT => return (EXT, cursor_external);
			when INT => return (INT, cursor_internal);
		end case;

	end locate_unit;



	
	function get_package_model (
		device_cursor	: in pac_devices_lib.cursor;
		variant			: in pac_package_variant_name.bounded_string)
		return pac_package_model_file_name.bounded_string 
	is
		package_model : pac_package_model_file_name.bounded_string; -- to be returned (packages/smd/SOT23.pac)
		
		procedure query_variants (
			device_name	: in pac_device_model_file.bounded_string;
			device		: in type_device_lib) 
		is
			use pac_variants;
			variant_cursor : pac_variants.cursor;
		begin
			variant_cursor := pac_variants.find (device.variants, variant);
			package_model := element (variant_cursor).package_model;
		end;
		
	begin
		pac_devices_lib.query_element (
			position	=> device_cursor,
			process		=> query_variants'access);

		return package_model;
	end get_package_model;


	
	function properties (
	-- Returns the poperties of the given port of the given device.
		device_cursor	: in pac_devices_lib.cursor;
		port_name		: in pac_port_name.bounded_string)
		return pac_ports.cursor is

		port_cursor : pac_ports.cursor; -- to be returned

		use pac_devices_lib;

		procedure query_units (
			model	: in pac_device_model_file.bounded_string; -- ../libraries/devices/logic_ttl/7400.dev
			device	: in type_device_lib) is

			use pac_units_internal;
			unit_internal_cursor : pac_units_internal.cursor := device.units_internal.first;
			
			use pac_units_external;
			unit_external_cursor : pac_units_external.cursor := device.units_external.first;

			use pac_ports;

			procedure query_ports (
			-- Query ports of internal unit.
				unit_name	: in pac_unit_name.bounded_string;
				unit		: in type_unit_internal) is
			begin				
				port_cursor := find (unit.symbol.ports, port_name);
			end query_ports;

			procedure query_symbols (
			-- Query ports of external unit.
				unit_name	: in pac_unit_name.bounded_string;
				unit		: in type_unit_external) is
				use pac_symbols;
				symbol_cursor : pac_symbols.cursor := locate (unit.model);

				procedure query_ports (
					file	: in pac_symbol_model_file.bounded_string; -- ../libraries/symbols/NAND.sym
					symbol	: in type_symbol) is
				begin
					port_cursor := find (symbol.ports, port_name);
				end;
				
			begin -- query_symbols
				query_element (
					position	=> symbol_cursor,
					process		=> query_ports'access);
			end query_symbols;
			
		begin -- query_units
			-- search the port among the internal units first
			while unit_internal_cursor /= pac_units_internal.no_element loop

				query_element (
					position	=> unit_internal_cursor,
					process		=> query_ports'access);

				-- The search ends when the given port has been found.
				if port_cursor /= pac_ports.no_element then
					exit;
				end if;
				
				next (unit_internal_cursor);
			end loop;

			-- if port not found among the internal units, search in external units:
			if port_cursor = pac_ports.no_element then
				while unit_external_cursor /= pac_units_external.no_element loop

					query_element (
						position	=> unit_external_cursor,
						process		=> query_symbols'access);

					-- The search ends when the given port has been found.
					if port_cursor /= pac_ports.no_element then
						exit;
					end if;
										
					next (unit_external_cursor);
				end loop;
			end if;

		end query_units;
		
	begin -- properties
		query_element (
			position	=> device_cursor,
			process		=> query_units'access);
		
		return port_cursor;
	end properties;

	
end et_devices;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

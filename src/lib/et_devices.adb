------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                              DEVICES                                     --
--                                                                          --
--                              B o d y                                     --
--                                                                          --
--         Copyright (C) 2019 Mario Blunk, Blunk electronic                 --
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

with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;
with ada.text_io;				use ada.text_io;
with ada.strings; 				use ada.strings;
--with ada.strings.maps;
with ada.strings.maps.constants;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.strings.bounded; 		use ada.strings.bounded;
with ada.strings.unbounded; 	use ada.strings.unbounded;
with ada.numerics.real_arrays;  use ada.numerics.real_arrays;
with ada.directories;			use ada.directories;
with ada.exceptions; 			use ada.exceptions;

with et_string_processing;
with et_coordinates;
with et_import;
with et_text;

with et_symbols;				use et_symbols;
with et_packages;				use et_packages;

package body et_devices is

	
	function to_string (name : in type_device_model_file.bounded_string) 
		return string is
	begin
		return type_device_model_file.to_string (name);
	end;

	function to_file_name (name : in string) 
		return type_device_model_file.bounded_string is
	begin
		return type_device_model_file.to_bounded_string (name);
	end;
	

	function to_string (value : in type_value.bounded_string) return string is
	-- Returns the given value as string.
	begin
		return type_value.to_string (value);
	end to_string;

	function value_length_valid (value : in string) return boolean is
	-- Tests if the given value is longer than allowed. Returns false if too long.
	-- Returns true if length is in allowed range.		
		use et_string_processing;
	begin
		if value'length > value_length_max then
			log (WARNING, "value " & enclose_in_quotes (value) & " is longer than" 
				 & positive'image (value_length_max) & " characters !");
			return false;
		else
			return true;
		end if;
	end value_length_valid;

	function truncate (value : in string) return type_value.bounded_string is
		value_out : string (1 .. value_length_max);
		use et_string_processing;
	begin
		value_out := value ((value'first) .. value'first - 1 + value_length_max);

		log (WARNING, "value will be truncated to " & enclose_in_quotes (value_out));
		return type_value.to_bounded_string (value_out);
	end truncate;
	
	function value_characters_valid (
		value		: in type_value.bounded_string;
		characters	: in character_set := value_characters) 
		return boolean is
	-- Tests if the given value contains only valid characters as specified
	-- by given character set. Returns false if invalid character found.
	-- Issues warning.
		use et_string_processing;
		use type_value;
		invalid_character_position : natural := 0;
	begin
		invalid_character_position := index (
			source	=> value,
			set 	=> characters,
			test 	=> outside);

		if invalid_character_position > 0 then
			log (WARNING, "value " &
				 enclose_in_quotes (type_value.to_string (value))
				 & " has invalid character at position"
				 & natural'image (invalid_character_position)
				);
			return false;
		else
			return true;
		end if;
	end value_characters_valid;

	procedure value_invalid (value : in string) is 
	-- Issues error message and raises constraint error.
		use et_string_processing;
	begin
		log (ERROR, "value " & enclose_in_quotes (value) &
			 " invalid !", console => true);
		raise constraint_error;
	end;

	function to_value (
	-- Tests the given value for length and invalid characters.
		value						: in string;
		error_on_invalid_character	: in boolean := true)
		return type_value.bounded_string is
		
		value_out : type_value.bounded_string; -- to be returned		
	begin
		-- Test length of given value. truncate if too long:
		if value_length_valid (value) then
			value_out := type_value.to_bounded_string (value);
		else
			value_out := truncate (value);
		end if;

		-- Test characters in (truncated) value. If error_on_invalid_character 
		-- is required by caller, abort on invalid character (default).
		if value_characters_valid (value_out) then
			null;
		else
			if error_on_invalid_character then
				value_invalid (type_value.to_string (value_out));
			end if;
		end if;
			
		return value_out;
	end to_value;


	
	function to_string (prefix : in type_device_name_prefix.bounded_string) return string is
	-- returns the given prefix as string
	begin
		return type_device_name_prefix.to_string (prefix); -- leading space not allowd !
	end to_string;

	function to_prefix (prefix : in string) return type_device_name_prefix.bounded_string is begin
		return type_device_name_prefix.to_bounded_string (prefix);
	end to_prefix;

	procedure check_prefix_length (prefix : in string) is
	-- Tests if the given prefix is longer than allowed.
		use et_string_processing;
	begin
		if prefix'length > device_name_prefix_length_max then
			log (ERROR, "max. number of characters for device name prefix is" 
				 & positive'image (device_name_prefix_length_max) & " !",
				console => true);
			raise constraint_error;
		end if;
	end check_prefix_length;
	
	procedure check_prefix_characters (prefix : in type_device_name_prefix.bounded_string) is
	-- Tests if the given prefix contains only valid characters.
	-- Raises exception if invalid character found.
		use et_string_processing;
		invalid_character_position : natural := 0;
	begin
		invalid_character_position := index (
			source	=> prefix,
			set		=> device_name_prefix_characters,
			test	=> outside);

		if invalid_character_position > 0 then
			log (ERROR, "device prefix " & to_string (prefix) 
				 & " has invalid character at position"
				 & natural'image (invalid_character_position),
				console => true
				);
			raise constraint_error;
		end if;
	end check_prefix_characters;

	

	




	function to_string (terminals : in type_terminal_count) return string is
	-- Returns the given number of terminals as string.
	begin
		return " terminal count" & type_terminal_count'image (terminals);
	end to_string;
	
	





	function to_string (unit_name : in type_unit_name.bounded_string) return string is
	-- Returns the given unit name as string.
	begin
		return type_unit_name.to_string (unit_name);
	end to_string;

	function to_unit_name (unit_name : in string) return type_unit_name.bounded_string is
	-- Returns the given unit name as type_unit_name.
	begin
		-- CS do character and length checks
		return type_unit_name.to_bounded_string (unit_name);
	end to_unit_name;

	function to_string (swap_level : in type_unit_swap_level) return string is begin
		return type_unit_swap_level'image (swap_level);
	end to_string;

	function to_swap_level (swap_level : in string) return type_unit_swap_level is begin
		return type_unit_swap_level'value (swap_level);
	end to_swap_level;
	
	function to_string (add_level : in type_unit_add_level) return string is begin
		return latin_1.space & to_lower (type_unit_add_level'image (add_level));
	end to_string;

	function to_add_level (add_level : in string) return type_unit_add_level is begin
		return type_unit_add_level'value (add_level);
	end to_add_level;

	

	

	
	
	function to_string (package_variant : in type_component_variant_name.bounded_string) return string is begin
		return type_component_variant_name.to_string (package_variant);
	end to_string;

	function to_component_variant_name (variant_name : in string) 
		return type_component_variant_name.bounded_string is
	begin
		return type_component_variant_name.to_bounded_string (variant_name);
	end to_component_variant_name;

	procedure check_variant_name_length (variant_name : in string) is
	-- tests if the given variant name is not longer than allowed
		use et_string_processing;
	begin
		if variant_name'length > component_variant_name_length_max then
			log (WARNING, "variant name too long. Max. length is" 
				 & positive'image (component_variant_name_length_max) & " !");
		end if;
	end check_variant_name_length;
	
	procedure check_variant_name_characters (
		variant		: in type_component_variant_name.bounded_string;
		characters	: in character_set := component_variant_name_characters) is
	-- Tests if the given variant name contains only valid characters as specified
	-- by given character set.
		use et_string_processing;
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




	
	function variant_available (
	-- Returns true if given device provides the given package variant.
	-- The given device must be real. Means appearance SCH_PCB.
		device_cursor	: in type_devices.cursor;
		variant			: in type_component_variant_name.bounded_string)  -- D, N
		return boolean is
		
		result : boolean := false; -- to be returned
		
		procedure query_variants (
			device_name	: in type_device_model_file.bounded_string;
			device		: in type_device) is
		begin
			if type_component_variants.contains (device.variants, variant) then
				result := true;
			end if;
		end;
		
	begin
		type_devices.query_element (
			position	=> device_cursor,
			process		=> query_variants'access);
		
		return result;
	end variant_available;

	function locate_device (model : in type_device_model_file.bounded_string) -- ../libraries/devices/transistor/pnp.dev
	-- Locates the given generic device in container "devices".
		return type_devices.cursor is
		use type_devices;
		cursor : type_devices.cursor := type_devices.find (devices, model);
	begin
		return cursor;
	end;
	
	function package_model (
	-- Returns the name of the package model of the given device according to the given variant.
	-- The given device must be real. Means appearance SCH_PCB.
		device_cursor	: in type_devices.cursor;
		variant			: in type_component_variant_name.bounded_string) -- D, N
		return type_package_model_file.bounded_string is -- libraries/packages/smd/SOT23.pac
		package_model : type_package_model_file.bounded_string; -- to be returned (packages/smd/SOT23.pac)

		procedure query_variants (
			device_name	: in type_device_model_file.bounded_string;
			device		: in type_device) is
			use type_component_variants;
			variant_cursor : type_component_variants.cursor;
		begin
			variant_cursor := type_component_variants.find (device.variants, variant);
			package_model := element (variant_cursor).package_model;
		end;

	begin
		type_devices.query_element (
			position	=> device_cursor,
			process		=> query_variants'access);

		return package_model;
	end package_model;


	
	function properties (
	-- Returns the poperties of the given port of the given device.
		device_cursor	: in type_devices.cursor;
		port_name		: in type_port_name.bounded_string)
		return type_ports.cursor is

		port_cursor : type_ports.cursor; -- to be returned

		use type_devices;

		procedure query_units (
			model	: in type_device_model_file.bounded_string; -- ../libraries/devices/logic_ttl/7400.dev
			device	: in type_device) is

			use type_units_internal;
			unit_internal_cursor : type_units_internal.cursor := device.units_internal.first;
			
			use type_units_external;
			unit_external_cursor : type_units_external.cursor := device.units_external.first;

			use type_ports;

			procedure query_ports (
			-- Query ports of internal unit.
				unit_name	: in type_unit_name.bounded_string;
				unit		: in type_unit_internal) is
			begin				
				port_cursor := find (unit.symbol.ports, port_name);
			end query_ports;

			procedure query_symbols (
			-- Query ports of external unit.
				unit_name	: in type_unit_name.bounded_string;
				unit		: in type_unit_external) is
				use type_symbols;
				symbol_cursor : type_symbols.cursor := locate (unit.file);

				procedure query_ports (
					file	: in type_symbol_model_file.bounded_string; -- ../libraries/symbols/NAND.sym
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
			while unit_internal_cursor /= type_units_internal.no_element loop

				query_element (
					position	=> unit_internal_cursor,
					process		=> query_ports'access);

				-- The search ends when the given port has been found.
				if port_cursor /= type_ports.no_element then
					exit;
				end if;
				
				next (unit_internal_cursor);
			end loop;

			-- if port not found among the internal units, search in external units:
			if port_cursor = type_ports.no_element then
				while unit_external_cursor /= type_units_external.no_element loop

					query_element (
						position	=> unit_external_cursor,
						process		=> query_symbols'access);

					-- The search ends when the given port has been found.
					if port_cursor /= type_ports.no_element then
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

------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                              DEVICES                                     --
--                                                                          --
--                              B o d y                                     --
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

with ada.strings; 				use ada.strings;
with ada.strings.maps.constants;
with ada.strings.fixed; 		use ada.strings.fixed;

with et_exceptions;				use et_exceptions;

package body et_devices is
	
	use et_terminals;
	
	function to_string (purpose : in pac_device_purpose.bounded_string) return string is begin
		return pac_device_purpose.to_string (purpose);
	end to_string;

	function purpose_length_valid (purpose : in string) return boolean is 
	-- Returns true if given purpose is too long. Issues warning.
		use et_string_processing;
	begin
		if purpose'length > purpose_length_max then
			log (WARNING, "purpose " & enclose_in_quotes (purpose) & " is longer than" 
				 & positive'image (purpose_length_max) & " characters !", 
				console => true);
			return false;
		else
			return true;
		end if;
	end;
		
	function purpose_characters_valid (
	-- Tests if the given value contains only valid characters as specified
	-- by given character set. Returns false if invalid character found.
		purpose		: in pac_device_purpose.bounded_string;
		characters	: in character_set := purpose_characters) 
		return boolean is
		use et_string_processing;
		use pac_device_purpose;
		invalid_character_position : natural := 0;
	begin
		invalid_character_position := index (
			source	=> purpose,
			set 	=> characters,
			test 	=> outside);

		if invalid_character_position > 0 then
			log (WARNING, "purpose " & enclose_in_quotes (to_string (purpose))
				 & " has invalid character at position"
				 & natural'image (invalid_character_position)
				);
			return false;
		else
			return true;
		end if;
	end purpose_characters_valid;

	procedure purpose_invalid (purpose : in string) is 
		use et_string_processing;
	begin
		--log (ERROR, "purpose " & enclose_in_quotes (purpose) &
			 --" invalid !", console => true);
		--raise constraint_error;
		raise syntax_error_1 with
			"ERROR: Purpose " & enclose_in_quotes (purpose) & " invalid !";
	end purpose_invalid;

	function to_purpose (
	-- Tests the given purpose for length and invalid characters.
		purpose 					: in string;
		error_on_invalid_character	: in boolean := true)
		return pac_device_purpose.bounded_string is

		purpose_out : pac_device_purpose.bounded_string; -- to be returned
	begin
		-- Test length of given purpose:
		if purpose_length_valid (purpose) then
			purpose_out := pac_device_purpose.to_bounded_string (purpose);
		else
			purpose_invalid (purpose);
		end if;

		-- Test characters:
		if purpose_characters_valid (purpose_out) then
			null;
		else
			purpose_invalid (purpose);
		end if;

		return purpose_out;
	end to_purpose;

	function is_empty (purpose : in pac_device_purpose.bounded_string) return boolean is 
		use pac_device_purpose;
	begin
		if length (purpose) = 0 then return true;
		else return false;
		end if;
	end is_empty;


	
	function to_string (name : in pac_device_model_file.bounded_string) 
		return string is
	begin
		return pac_device_model_file.to_string (name);
	end;

	function to_file_name (name : in string) 
		return pac_device_model_file.bounded_string is
	begin
		return pac_device_model_file.to_bounded_string (name);
	end;
	

	function to_string (value : in pac_device_value.bounded_string) return string is begin
		return pac_device_value.to_string (value);
	end;

	function to_value (value : in string) return pac_device_value.bounded_string is begin
		return pac_device_value.to_bounded_string (value);
	end;

	
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

	function truncate (value : in string) return pac_device_value.bounded_string is
		value_out : string (1 .. value_length_max);
		use et_string_processing;
	begin
		value_out := value ((value'first) .. value'first - 1 + value_length_max);

		log (WARNING, "value will be truncated to " & enclose_in_quotes (value_out));
		return pac_device_value.to_bounded_string (value_out);
	end truncate;
	
	function value_characters_valid (
		value		: in pac_device_value.bounded_string;
		characters	: in character_set := value_characters) 
		return boolean is
	-- Tests if the given value contains only valid characters as specified
	-- by given character set. Returns false if invalid character found.
	-- Issues warning.
		use et_string_processing;
		use pac_device_value;
		invalid_character_position : natural := 0;
	begin
		invalid_character_position := index (
			source	=> value,
			set 	=> characters,
			test 	=> outside);

		if invalid_character_position > 0 then
			log (WARNING, "value " &
				 enclose_in_quotes (pac_device_value.to_string (value))
				 & " has invalid character at position"
				 & natural'image (invalid_character_position) & " !");
			return false;
		else
			return true;
		end if;
	end value_characters_valid;

	procedure value_invalid (value : in string) is 
		use et_string_processing;
	begin
		--log (ERROR, "value " & enclose_in_quotes (value) &
			 --" invalid !", console => true);
		--raise constraint_error;
		raise syntax_error_1 with
			"ERROR: Value " & enclose_in_quotes (value) & " invalid !";
	end value_invalid;

	function to_value_with_check (
	-- Tests the given value for length and invalid characters.
		value						: in string;
		error_on_invalid_character	: in boolean := true)
		return pac_device_value.bounded_string is
		
		value_out : pac_device_value.bounded_string; -- to be returned		
	begin
		-- Test length of given value. truncate if too long:
		if value_length_valid (value) then
			value_out := pac_device_value.to_bounded_string (value);
		else
			value_out := truncate (value);
		end if;

		-- Test characters in (truncated) value. If error_on_invalid_character 
		-- is required by caller, abort on invalid character (default).
		if value_characters_valid (value_out) then
			null;
		else
			if error_on_invalid_character then
				value_invalid (pac_device_value.to_string (value_out));
			end if;
		end if;
			
		return value_out;
	end to_value_with_check;

	-- Returns true if value is empty ("").
	function is_empty (value : in pac_device_value.bounded_string) return boolean is 
		use pac_device_value;
	begin
		if length (value) = 0 then return true;
		else return false;
		end if;
	end is_empty;
		







	function to_string (prefix : in pac_device_prefix.bounded_string) return string is
	begin
		return pac_device_prefix.to_string (prefix); -- leading space not allowd !
	end to_string;

	function to_prefix (prefix : in string) return pac_device_prefix.bounded_string is begin
		return pac_device_prefix.to_bounded_string (prefix);
	end to_prefix;

	procedure check_prefix_length (prefix : in string) is
	-- Tests if the given prefix is longer than allowed.
		use et_string_processing;
	begin
		if prefix'length > prefix_length_max then
			log (ERROR, "max. number of characters for device name prefix is" 
				 & positive'image (prefix_length_max) & " !",
				console => true);
			raise constraint_error;
		end if;
	end check_prefix_length;
	
	procedure check_prefix_characters (prefix : in pac_device_prefix.bounded_string) is
	-- Tests if the given prefix contains only valid characters.
	-- Raises exception if invalid character found.
		use et_string_processing;
		invalid_character_position : natural := 0;
	begin
		invalid_character_position := index (
			source	=> prefix,
			set		=> prefix_characters,
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

	

	

	function to_string (index : in type_name_index) return string is begin
		return latin_1.space & trim (type_name_index'image (index), left);
	end;

	function to_index (index : in string) return type_name_index is begin
		return type_name_index'value (index);
	end;

	function same_prefix (left, right : in type_device_name) return boolean is begin
		if prefix (left) = prefix (right) then
			return true;
		else
			return false;
		end if;
	end same_prefix;
	
	function to_device_name (text_in : in string) return type_device_name is

		-- justify given text_in on the left
		text_in_justified : string (1 .. text_in'length) := to_upper (text_in);
	
		r : type_device_name := (
				prefix		=> pac_device_prefix.to_bounded_string(""),
				id 			=> 0,
				id_width	=> 1);
	
		c : character;
		p : pac_device_prefix.bounded_string;
	
		procedure invalid_device_name is
			use et_string_processing;
		begin
			--log (ERROR, latin_1.lf & "invalid device name '" 
				 --& text_in_justified & "'", console => true);

			-- CS show position of affected character ?
			raise syntax_error_1 with 
				"ERROR: Device name " & enclose_in_quotes (text_in_justified) & " invalid !";

		end invalid_device_name;

		d : positive;
		digit : natural := 0;

		use pac_device_prefix;

	begin -- to_device_name
		-- assemble prefix
		for i in text_in_justified'first .. text_in_justified'last loop
			c := text_in_justified (i);
			
			case i is 
				-- The first character MUST be a valid prefix character.
				when 1 => 
					if is_in (c, prefix_characters) then
						r.prefix := r.prefix & c;
					else 
						invalid_device_name;
					end if;
					
				-- Further characters are appended to prefix if they are valid prefix characters.
				-- If anything else is found, the prefix is assumed as complete.
				when others =>
					if is_in (c, prefix_characters) then
						r.prefix := r.prefix & c;
					else
						d := i; -- d holds the position of the charcter after the prefix.
							-- d is requried when reading the component id. see below.
						exit;
					end if;
			end case;
		end loop;

		-- assemble id
		-- Start with the last character in text_in_justified.
		-- Finish at the position d (that is the first digit after the last letter, see above).
		-- All the characters within this range must be digits.
		-- The significance of the digit is increased after each pass.
		for i in reverse d .. text_in_justified'last loop
			c := text_in_justified (i);
			
			if is_digit (c) then
				r.id := r.id + 10**digit * natural'value (1 * c);
			else
				invalid_device_name;
			end if;

			digit := digit + 1; -- increase digit significance (10**0, 10**1, ...)
		end loop;
		
		-- There must be at least one digit. If no digit found, then the given
		-- device name has no index.
		if digit > 0 then
		
			-- Set the id width.
			-- It is the number of digits processed when the id was assembled (see above).
			-- Example: if the given string was IC002 then digit is 3.
			r.id_width := digit;
		else
			raise syntax_error_1 with "ERROR: Device name invalid. Missing index !";
		end if;
		
		return r;
	end to_device_name;

	function "<" (left, right : in type_device_name) return boolean is
	-- Returns true if left comes before right.
	-- If left equals right, the return is false.
	-- CS: needs verification !
		result : boolean := false;
		use pac_device_prefix;
	begin
		-- First we compare the prefix.
		-- Example: If left is C201 and right is R4 then the result is true as C comes before R.

		if left.prefix < right.prefix then -- like C201 and R4
			result := true;
		elsif left.prefix > right.prefix then -- like R4 and C201
			result := false;
		elsif left.prefix = right.prefix then -- like IC33 and IC34

			-- If equal prefixes, we compare the id:
			if left.id < right.id then -- like 33 and 34
				result := true;
			else
				result := false; -- like 34 and 33
			end if;

		end if;

		-- in case of equivalence of left and right, we return false (default)
		return result;
	end;	

	function "=" (left, right : in type_device_name) return boolean is
	-- Returns true if left equals right.
	-- Example: if IC4 = IC4 then return true.
		result : boolean := false;
		use pac_device_prefix;
	begin
		-- First we compare the prefix. If prefixes are equal, we compare the id.
		-- If either of them does not match, the result is set false.
		if left.prefix = right.prefix then -- like IC and IC

			if left.id = right.id then -- like 4 and 4
				result := true;
			else -- like 5 and 6
				result := false;
			end if;
			
		else -- like R and IC
			result := false; 
		end if;

		return result;
	end;

	function to_string (name : in type_device_name) return string is
	-- Returns the given device name as string.
	-- Prepends leading zeros according to name.id_width.
		id_width_wanted	: natural := name.id_width;
	
		-- The width of the given id is obtained by converting the id to a string
		-- and then by measuring its length:
		id_width_given	: natural := trim (natural'image (name.id),left)'length;

		-- Finally the number of zeros to prepend is the difference of wanted 
		-- and given digits:
		lz : natural := id_width_wanted - id_width_given;
	begin
		case lz is
			when 0 => -- no leading zeroes
				return pac_device_prefix.to_string (name.prefix) 
					& trim (natural'image (name.id),left);
				
			when others => -- leading zeros required
				return pac_device_prefix.to_string (name.prefix) 
					& lz * '0' & trim (natural'image (name.id),left);
		end case;
	end to_string;
	
	function prefix (name : in type_device_name) return pac_device_prefix.bounded_string is begin
	-- Returns the prefix of the given device name.
		return name.prefix;
	end;

	function index (name : in type_device_name) return type_name_index is begin
	-- Returns the index of the given device name.
		return name.id;
	end;

	function to_device_name (
		prefix	: in pac_device_prefix.bounded_string; 	-- R, C, L
		index	: in type_name_index;				-- 1, 20, ..
		width	: in type_index_width := type_index_width'first) -- the number of digits
		return type_device_name is
		device_name : type_device_name; -- to be returned
	begin
		-- assign prefix and index as requested:
		device_name.prefix := prefix;
		device_name.id := index;

		-- Calculate the width of the index. examples: it is 3 for IC987, 2 for C77
		-- The width of the index is obtained by converting the given index to a string
		-- and then by measuring its length:
		device_name.id_width := trim (natural'image (index),left)'length;

		-- If width IS provided AND wider than the just calculated width,
		-- then the calculated width is overwritten.
		if width /= type_index_width'first then

			-- If width is smaller or equal the calculated width nothing happens.
			-- Otherwise width is set according to the provided width:
			if width <= device_name.id_width then
				null;
			else			
				device_name.id_width := width;
			end if;
		end if;
		
		return device_name;
	end;

	procedure offset_index (
		name	: in out type_device_name;
		offset	: in type_name_index) is
	begin
		name := to_device_name (
			prefix	=> prefix (name),
			index	=> name.id + offset);
		-- the width of the index is calculated automatically by to_device_name.
	end;

	



	function to_string (terminals : in type_terminal_count) return string is
	-- Returns the given number of terminals as string.
	begin
		return " terminal count" & type_terminal_count'image (terminals);
	end to_string;
	
	





	function to_string (unit_name : in pac_unit_name.bounded_string) return string is begin
		return pac_unit_name.to_string (unit_name);
	end;

	function to_unit_name (unit_name : in string) return pac_unit_name.bounded_string is begin
		-- CS do character and length checks
		return pac_unit_name.to_bounded_string (to_upper (unit_name));
	end;

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

	
	function to_string (swap_level : in type_swap_level) return string is begin
		return type_swap_level'image (swap_level);
	end;

	function to_swap_level (swap_level : in string) return type_swap_level is begin
		return type_swap_level'value (swap_level);
	end;


	
	function to_string (add_level : in type_add_level) return string is begin
		return to_lower (type_add_level'image (add_level));
	end;

	function to_add_level (add_level : in string) return type_add_level is begin
		return type_add_level'value (add_level);
	end;

	

	

	
	
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
		use et_string_processing;
	begin
		if variant_name'length > variant_name_length_max then
			log (WARNING, "variant name too long. Max. length is" 
				 & positive'image (variant_name_length_max) & " !");
		end if;
	end check_variant_name_length;
	
	procedure check_variant_name_characters (
		variant		: in pac_package_variant_name.bounded_string;
		characters	: in character_set := variant_name_characters) is
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



	
	function rotate_placeholders (
		symbol_cursor	: in pac_units_internal.cursor;
		destination		: in et_coordinates.type_position)
		return type_rotated_placeholders
	is
		use pac_units_internal;
		use et_coordinates.pac_geometry_sch;

		r : type_rotated_placeholders; -- to be returned
	begin
		r.name		:= element (symbol_cursor).symbol.name;
		r.value		:= element (symbol_cursor).symbol.value;
		r.purpose	:= element (symbol_cursor).symbol.purpose;

		-- rotate the positions of placeholders according to rotation given by caller:
		rotate (r, rot (destination));
		
		return r;
	end rotate_placeholders;


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

		use et_string_processing;

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
							cursors.int := first_internal (CAN);
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
								cursors.ext := first_external (CAN);
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

		use et_string_processing;
		
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
			when PCB		=> result := element (device_cursor).variants;
			when VIRTUAL	=> null;
		end case;
		
		return result;
	end available_variants;

	
	function locate_device (model : in pac_device_model_file.bounded_string) -- ../libraries/devices/transistor/pnp.dev
	-- Locates the given generic device in container "devices".
		return pac_devices_lib.cursor is
		use pac_devices_lib;
		cursor : pac_devices_lib.cursor := pac_devices_lib.find (devices, model);
	begin
		return cursor;
	end;

	function locate_unit (
		device_cursor	: in pac_devices_lib.cursor;
		unit_name		: in pac_unit_name.bounded_string) -- like "I/O-Bank 3"
		return type_unit_cursors is

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
	
	function package_model (
		device_cursor	: in pac_devices_lib.cursor;
		variant			: in pac_package_variant_name.bounded_string) -- D, N
		return type_package_model_file.bounded_string is -- libraries/packages/smd/SOT23.pac
		package_model : type_package_model_file.bounded_string; -- to be returned (packages/smd/SOT23.pac)

		procedure query_variants (
			device_name	: in pac_device_model_file.bounded_string;
			device		: in type_device_lib) is
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
	end package_model;


	
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

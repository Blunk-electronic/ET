------------------------------------------------------------------------------
--                                                                          --
--                           SYSTEM ET LIBRARIES                            --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               B o d y                                    --
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

--   The two letters "CS" indicate a "construction side" where things are not
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

package body et_libraries is

-- DEVICES
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
	
-- SYMBOLS
	function to_string (name : in type_symbol_model_file.bounded_string) 
		return string is
	begin
		return type_symbol_model_file.to_string (name);
	end to_string;

	function to_file_name (name : in string)
		return type_symbol_model_file.bounded_string is
	begin
		return type_symbol_model_file.to_bounded_string (name);
	end;

-- PACKAGES
	function to_string (name : in type_package_model_file.bounded_string) 
		return string is
	begin
		return type_package_model_file.to_string (name);
	end;

	function to_file_name (name : in string) 
		return type_package_model_file.bounded_string is
	begin
		return type_package_model_file.to_bounded_string (name);
	end;

	
	
	function to_string (person : in type_person_name.bounded_string) return string is
	-- Returns the given person name as string.
	begin
		return latin_1.space & type_person_name.to_string (person);
	end to_string;

	function to_text_size (size : in type_distance) return type_text_size is
	-- Converts given distance to type_text_size. Raises error on excessive text size.
		use et_string_processing;
	begin
		if size not in type_text_size then
			log_indentation_reset;
			log (message_error & "text " 
				 & to_string (size => size, preamble => true)  
				 & " out of range !",
				 console => true);

			log ("Allowed range is " & to_string (type_text_size'first, preamble => false) & " .. "
				 & to_string (type_text_size'last, preamble => false),
				 console => true);

			raise constraint_error;
		end if;
		return size;
	end to_text_size;
	
	function to_string (
		size		: in type_text_size;
		preamble	: in boolean := true) return string is
	-- Returns the given text size as string.
	begin
		if preamble then
			return "size " & et_coordinates.to_string (size);
		else
			return et_coordinates.to_string (size);
		end if;
	end to_string;

	function to_string (width : in type_text_line_width) return string is
	-- Returns the given line width as string.
	begin
		return latin_1.space & et_coordinates.to_string (width);
	end to_string;

	function to_string (style : in type_text_style) return string is begin
		return latin_1.space & to_lower (type_text_style'image (style));
	end to_string;
	
	function to_text_style (style : in string) return type_text_style is begin
		return type_text_style'value (style);
	end to_text_style;
	
	-- TEXT ALIGNMENT
	function to_string (alignment : in type_text_alignment_horizontal) return string is begin
		return latin_1.space & to_lower (type_text_alignment_horizontal'image (alignment));
	end to_string;

	function to_alignment_horizontal (alignment : in string) return type_text_alignment_horizontal is begin
		return type_text_alignment_horizontal'value (alignment);
	end to_alignment_horizontal;
	
	function to_string (alignment : in type_text_alignment_vertical) return string is begin
		return latin_1.space & to_lower (type_text_alignment_vertical'image (alignment));
	end to_string;

	function to_alignment_vertical (alignment : in string) return type_text_alignment_vertical is begin
		return type_text_alignment_vertical'value (alignment);
	end to_alignment_vertical;
	
	function to_string (alignment : in type_text_alignment) return string is
	begin
		return " alignment (hor./vert.) "
			& to_string (alignment.horizontal)
			& " / "
			& to_string (alignment.vertical);
	end to_string;

	function to_string (direction : in type_port_direction) return string is begin
		return latin_1.space & to_lower (type_port_direction'image (direction));
	end to_string;
	
	function to_port_direction (direction : in string) return type_port_direction is begin
		return type_port_direction'value (direction);
	end to_port_direction;
	
	function to_string (sensitivity : in type_sensitivity_edge) return string is begin
		return latin_1.space & to_lower (type_sensitivity_edge'image (sensitivity));
	end to_string;

	function to_sensitivity_edge (sensitivity : in string) return type_sensitivity_edge is begin
		return type_sensitivity_edge'value (sensitivity);
	end to_sensitivity_edge;

	function to_string (sensitivity : in type_sensitivity_level) return string is begin
		return latin_1.space & to_lower (type_sensitivity_level'image (sensitivity));
	end to_string;

	function to_sensitivity_level (sensitivity : in string) return type_sensitivity_level is begin
		return type_sensitivity_level'value (sensitivity);
	end to_sensitivity_level;

	function to_string (inverted : in type_output_inverted) return string is begin
		return latin_1.space & to_lower (type_output_inverted'image (inverted));
	end to_string;

	function to_output_inverted (inverted : in string) return type_output_inverted is begin
		return type_output_inverted'value (inverted);
	end to_output_inverted;

	function to_string (weakness : in type_output_weakness) return string is begin
		return latin_1.space & to_lower (type_output_weakness'image (weakness));
	end to_string;

	function to_output_weakness (weakness : in string) return type_output_weakness is begin
		return type_output_weakness'value (weakness);
	end to_output_weakness;

	function to_string (level : in type_power_level) return string is
	-- Converts the power level (like LEVEL_POSITIVE) to a string (like positive).
	-- The prefix LEVEL_ is removed.
		level_string : string := to_lower (type_power_level'image (level)); -- level_positive, level_negative
		A : positive := index (level_string, "_") + 1; -- the position after the first underscore
		B : positive := level_string'length;
	begin
		return latin_1.space & level_string (A .. B);
	end to_string;

	function to_power_level (level : in string) return type_power_level is 
	-- Converts the power level (like positive) to power level (like LEVEL_POSITIVE).
	-- The prefix LEVEL_ is prepended.
	begin
		return type_power_level'value ("LEVEL_" & level);
	end to_power_level;

	function to_string (tristate : in type_output_tristate) return string is begin
		return latin_1.space & to_lower (type_output_tristate'image (tristate));
	end to_string;

	function to_output_tristate (tristate : in string) return type_output_tristate is begin
		return type_output_tristate'value (tristate);
	end to_output_tristate;
	
	function to_string (visible : in type_port_name_visible) return string is begin
		return latin_1.space & to_lower (type_port_name_visible'image (visible));
	end to_string;

	function to_port_name_visible (visible : in string) return type_port_name_visible is begin
		return type_port_name_visible'value (visible);
	end to_port_name_visible;
	
	function to_string (visible : in type_terminal_name_visible) return string is begin
		return latin_1.space & to_lower (type_terminal_name_visible'image (visible));
	end to_string;

	function to_terminal_name_visible (visible : in string) return type_terminal_name_visible is begin
		return type_terminal_name_visible'value (visible);
	end to_terminal_name_visible;
	
	function to_string (port : in type_port_name.bounded_string) return string is begin
		return type_port_name.to_string (port);
	end to_string;

	function to_port_name (name : in string) return type_port_name.bounded_string is begin
		return type_port_name.to_bounded_string (name);
	end to_port_name;
	
	function to_port_name_text_size (text : in string) return type_port_name_text_size is
	-- Converts a string to type_port_name_text_size.
	begin
		return type_port_name_text_size'value (text);
	end to_port_name_text_size;
	
	function to_string (terminal : in type_terminal_name.bounded_string) return string is
	-- Returns the given terminal name as string.
	begin
		return latin_1.space & type_terminal_name.to_string (terminal);
	end to_string;

	function to_terminal_name (terminal : in string) return type_terminal_name.bounded_string is
	-- Converts a string to a type_terminal_name.	
	begin
		return type_terminal_name.to_bounded_string (terminal);
	end to_terminal_name;
	
	function to_terminal_name_text_size (text : in string) return type_terminal_name_text_size is
	-- Converts a string to type_terminal_name_text_size.
	begin
		return type_terminal_name_text_size'value (text);
	end to_terminal_name_text_size;

	function to_string (package_variant : in type_component_variant_name.bounded_string) return string is
	-- converts a type_component_variant_name to a string.
	begin
		return type_component_variant_name.to_string (package_variant);
	end to_string;

	function to_component_variant_name (variant_name : in string) 
		return type_component_variant_name.bounded_string is
	-- converts a string to a variant name
	begin
		return type_component_variant_name.to_bounded_string (variant_name);
	end to_component_variant_name;

	procedure check_variant_name_length (variant_name : in string) is
	-- tests if the given variant name is not longer than allowed
		use et_string_processing;
	begin
		if variant_name'length > component_variant_name_length_max then
			log (message_warning & "variant name too long. Max. length is" 
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
			log (message_warning & "invalid character in variant name " 
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

	function to_string (name : in type_frame_template_name.bounded_string) return string is begin
		return type_frame_template_name.to_string (name);
	end to_string;
	
	function to_template_name (name : in string) return type_frame_template_name.bounded_string is begin
		return type_frame_template_name.to_bounded_string (name);
	end to_template_name;
	
	function to_string (partcode : in type_component_partcode.bounded_string) return string is begin
		return type_component_partcode.to_string (partcode);
	end to_string;

	function to_partcode (partcode : in string) return type_component_partcode.bounded_string is begin
		return type_component_partcode.to_bounded_string (partcode);
	end to_partcode;
	
	procedure check_partcode_length (partcode : in string) is
	-- Tests if the given partcode is longer than allowed.
		use et_string_processing;
	begin
		if partcode'length > component_partcode_length_max then
			log (message_warning & "partcode too long. Max. length is" 
				 & positive'image (component_partcode_length_max) & " !");
		end if;
	end check_partcode_length;
	
	procedure check_partcode_characters (
		partcode	: in type_component_partcode.bounded_string;
		characters	: in character_set := component_partcode_characters) is
	-- Tests if the given partcode contains only valid characters as specified
	-- by given character set.
		use et_string_processing;
		use type_component_partcode;
		invalid_character_position : natural := 0;
	begin
		invalid_character_position := index (
			source	=> partcode,
			set		=> characters,
			test	=> outside);

		if invalid_character_position > 0 then
			log (message_warning & "partcode " & to_string (partcode) 
				 & " has invalid character at position"
				 & natural'image (invalid_character_position));
		end if;
	end check_partcode_characters;


	function to_string (filled : in type_circle_filled) return string is begin
		return latin_1.space & to_lower (type_circle_filled'image (filled));
	end to_string;

	function to_circle_filled (filled : in string) return type_circle_filled is begin
		return type_circle_filled'value (filled);
	end to_circle_filled;


	function to_string (meaning : in type_text_meaning) return string is begin
		return latin_1.space & to_lower (type_text_meaning'image (meaning));
	end to_string;

	function to_text_meaning (meaning : in string) return type_text_meaning is begin
		return type_text_meaning'value (meaning);
	end to_text_meaning;

	function to_component_attribute_text_size (text : in string) return type_placeholder_text_size is
	-- Converts a string to a type_placeholder_text_size.
	begin
		return type_placeholder_text_size'value (text);
	end to_component_attribute_text_size;

	function to_string (text_content : in type_text_content.bounded_string) return string is begin
		return type_text_content.to_string (text_content);
	end to_string;

	function to_content (content : in string) return type_text_content.bounded_string is begin
		return type_text_content.to_bounded_string (content);
	end to_content;

	
	procedure check_text_content_length (content : in string) is
	-- Tests if the content is longer than allowed.
		use et_string_processing;
	begin
		if content'length > text_length_max then
			log_indentation_reset;
			log (message_error & "max. number of characters for a text field is" 
				 & positive'image (text_length_max) & " !",
				 console => true);
			raise constraint_error;
		end if;
	end check_text_content_length;
	
	procedure write_placeholder_properties (
	-- Writes the properties of the given placeholder.
		placeholder		: in type_text_placeholder;
		log_threshold	: in et_string_processing.type_log_level) is

		use et_string_processing;
	begin
		-- meaning
		log (to_string (placeholder.meaning), log_threshold);
		log_indentation_up;
		
		-- position
		log (to_string (placeholder.position), log_threshold);

		-- size
		log (et_libraries.to_string (size => placeholder.size), log_threshold);

		-- style
		log ("style "
			& to_lower (type_text_style'image (placeholder.style)), log_threshold);

		-- line width
		log ("line width"
			& to_string (width => placeholder.line_width), log_threshold);

		-- rotation
		log (to_string (placeholder.rotation), log_threshold); 

		-- visible
		--log ("visible "
		--	& to_lower (et_libraries.type_text_visible'image (placeholder.visible)), log_threshold);

		-- alignment
		log ("alignment (hor/vert) "
			& to_lower (et_libraries.type_text_alignment_horizontal'image (placeholder.alignment.horizontal))
			& "/"
			& to_lower (et_libraries.type_text_alignment_vertical'image (placeholder.alignment.vertical)), log_threshold);

		log_indentation_down;
	end write_placeholder_properties;


	procedure write_text_properies (
	-- Outputs the properties of the given text.
		text : in et_libraries.type_text;
		log_threshold : in et_string_processing.type_log_level) is

		use et_string_processing;
	begin
-- 		log_indentation_up;
		
		-- meaning
		log ("field/attribute" & et_libraries.to_string (text.meaning), level => log_threshold);
		log_indentation_up;
		
		-- content
		if et_libraries.type_text_content.length (text.content) > 0 then
			log ("content '" & et_libraries.type_text_content.to_string(text.content) & "'",
				level => log_threshold);
		else
			log ("no content", level => log_threshold);
		end if;

		-- position
		log (to_string (text.position), level => log_threshold + 1);
		
		-- size
		log ("size" & et_coordinates.to_string (text.size), level => log_threshold + 1);

		-- style
		log ("style " & to_lower(et_libraries.type_text_style'image (text.style)),
			 level => log_threshold + 1);

		-- line width
		log ("line width" & et_coordinates.to_string (text.line_width),
			level => log_threshold + 1);

		-- rotation
		log (to_string (text.rotation), level => log_threshold + 1);

		-- visible
		--log ("visible " & to_lower(et_libraries.type_text_visible'image (text.visible)),
		--	level => log_threshold + 1);

		-- alignment
		log ("alignment (horizontal/vertical) "
			& to_lower (et_libraries.type_text_alignment_horizontal'image (text.alignment.horizontal))
			& "/"
			& to_lower (et_libraries.type_text_alignment_vertical'image (text.alignment.vertical)),
			level => log_threshold + 1);

-- 		log_indentation_down;
		log_indentation_down;
	end write_text_properies;


	function content (text : in type_text) return string is
	-- Returns the content of the given text as string.
		c : type_text_content.bounded_string;
	begin
		c := text.content;
		return type_text_content.to_string (c);
	end content;

	function to_string (value : in type_component_value.bounded_string) return string is
	-- Returns the given value as string.
	begin
		return type_component_value.to_string (value);
	end to_string;

	function to_value (value : in string) return type_component_value.bounded_string is
	begin
		return type_component_value.to_bounded_string (value);
	end to_value;
	
	function value_length_valid (value : in string) return boolean is
	-- Tests if the given value is longer than allowed. Returns false if too long.
	-- Returns true if length is in allowed range.		
		use et_string_processing;
	begin
		if value'length > component_value_length_max then
			log (message_warning & "value " & value & " is longer than" 
				 & positive'image (component_value_length_max) & " characters !");
			return false;
		else
			return true;
		end if;
	end value_length_valid;

	function truncate (value : in string) return type_component_value.bounded_string is
		value_out : string (1 .. component_value_length_max);
		use et_string_processing;
	begin
		value_out := value ((value'first) 
							.. value'first - 1 + component_value_length_max);

		log (message_warning & "value will be truncated to " & value_out);
		return to_value (value_out);
	end truncate;
	
	function value_characters_valid (
		value		: in type_component_value.bounded_string;
		characters	: in character_set := component_value_characters) 
		return boolean is
	-- Tests if the given value contains only valid characters as specified
	-- by given character set. Returns false if invalid character found.
		use et_string_processing;
		use type_component_value;
		invalid_character_position : natural := 0;
	begin
		invalid_character_position := index (
			source => value,
			set => characters,
			test => outside);

		if invalid_character_position > 0 then
			log (message_warning & "value " & to_string (value) 
				 & " has invalid character at position"
				 & natural'image (invalid_character_position)
				);
			return false;
		else
			return true;
		end if;
	end value_characters_valid;
	
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
			log_indentation_reset;
			log (message_error & "max. number of characters for device name prefix is" 
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
			log_indentation_reset;
			log (message_error & "device prefix " & to_string (prefix) 
				 & " has invalid character at position"
				 & natural'image (invalid_character_position),
				console => true
				);
			raise constraint_error;
		end if;
	end check_prefix_characters;

	function to_string (ref_id : in type_device_name_index) return string is begin
		return latin_1.space & type_device_name_index'image (ref_id);
	end to_string;

	function to_reference_id (ref_id : in string) return type_device_name_index is begin
		return type_device_name_index'value (ref_id);
	end to_reference_id;

	function to_device_name (
	-- Converts a string like "IC303" to a composite type_device_name.
	-- Raises constraint error if prefix contains invalid characters.
	-- Raises constraint error if id contains non-digit characters.
	-- Leading zeroes in the id are removed. R002 becomes R2.
		text_in : in string)
		return type_device_name is
		use et_libraries;

		-- justify given text_in on the left
		text_in_justified : string (1 .. text_in'length) := text_in;
	
		r : type_device_name := (
				prefix		=> type_device_name_prefix.to_bounded_string(""),
				id 			=> 0,
				id_width	=> 1);
	
		c : character;
		p : type_device_name_prefix.bounded_string;
	
		procedure invalid_reference is
			use et_string_processing;
		begin
			log (text => latin_1.lf & message_error & "invalid device name '" 
				 & text_in_justified & "'", console => true);
			-- CS show position of affected character ?
			raise constraint_error;
		end invalid_reference;

		d : positive;
		digit : natural := 0;

		use et_libraries.type_device_name_prefix;

	begin -- to_device_name
		-- assemble prefix
		for i in text_in_justified'first .. text_in_justified'last loop
			c := text_in_justified(i);
			
			case i is 
				-- The first character MUST be a valid prefix character.
				when 1 => 
					if is_in (c, device_name_prefix_characters) then
						r.prefix := r.prefix & c;
					else 
						invalid_reference;
					end if;
					
				-- Further characters are appended to prefix if they are valid prefix characters.
				-- If anything else is found, the prefix is assumed as complete.
				when others =>
					if is_in (c, device_name_prefix_characters) then
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
			c := text_in_justified(i);
			
			if is_digit(c) then
				r.id := r.id + 10**digit * natural'value(1 * c);
			else
				invalid_reference;
			end if;

			digit := digit + 1; -- increase digit significance (10**0, 10**1, ...)
		end loop;

		-- Set the id width.
		-- It is the number of digits processed when the id was assembled (see above).
		-- Example: if the given string was IC002 then digit is 3.
		r.id_width := digit;
		
		return r;
	end to_device_name;

	
	function to_string (
		appearance	: in type_component_appearance;
		verbose		: in boolean := false)
		return string is
	-- Returns the given component appearance as string.
	begin
		if verbose then
			case appearance is
				when sch =>
					return ("appears in schematic only (virtual component)");
				when sch_pcb =>
					return ("appears in schematic and layout");
				when pcb =>
					return ("appears in layout only (mechanical component)");
			end case;
		else
			return latin_1.space & to_lower (type_component_appearance'image (appearance));
		end if;
	end to_string;

	function to_appearance (appearance : in string) return type_component_appearance is begin
		return type_component_appearance'value (appearance);
	end to_appearance;

	function to_string (unit_name : in type_unit_name.bounded_string) return string is
	-- Returns the given unit name as string.
	begin
		return type_unit_name.to_string (unit_name);
	end to_string;

	function to_unit_name (unit_name : in string) return type_unit_name.bounded_string is
	-- Returns the given unit name as type_unit_name.
	begin
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

	function to_string (packge : in type_component_package_name.bounded_string) return string is
	-- Returns the given package name as string.
	-- CS: provide a parameter that turns the preamble on/off
	begin
		return type_component_package_name.to_string (packge);
	end to_string;

	function to_package_name (package_name : in string) return type_component_package_name.bounded_string is
	-- Converts a string to a type_component_package_name.	
	begin
		return type_component_package_name.to_bounded_string (package_name);
	end to_package_name;
	
	procedure check_package_name_length (packge : in string) is
	-- Tests if the given package is longer than allowed.
		use et_string_processing;
	begin
		if packge'length > component_package_name_length_max then
			log (message_warning & "package name too long. Max. length is" 
				 & positive'image (component_package_name_length_max) & " !");
		end if;
	end check_package_name_length;

	procedure check_package_name_characters (
		packge		: in type_component_package_name.bounded_string;
		characters	: in character_set := component_package_name_characters)
		is
	-- Tests if the given package name contains only valid characters as specified
	-- by given character set.
	-- Raises exception if invalid character found.
		use et_string_processing;
		use type_component_package_name;
		invalid_character_position : natural := 0;
	begin
		invalid_character_position := index (
			source => packge,
			set => characters,
			test => outside);

		if invalid_character_position > 0 then
			log (message_warning & "package name " & to_string (packge) 
				 & " has invalid character at position"
				 & natural'image (invalid_character_position));
		end if;
	end check_package_name_characters;
	
	function to_string (terminals : in type_terminal_count) return string is
	-- Returns the given number of terminals as string.
	begin
		return " terminal count" & type_terminal_count'image (terminals);
	end to_string;
	
	function to_string (reference : in type_device_name) return string is
	-- Returns the given device name as string.
	-- Prepends leading zeros according to reference.id_width.
		id_width_wanted	: natural := reference.id_width;
	
		-- The width of the given id is obtained by converting the id to a string
		-- and then by measuring its length:
		id_width_given	: natural := trim(natural'image(reference.id),left)'length;

		-- Finally the number of zeros to prepend is the difference of wanted 
		-- and given digits:
		lz				: natural := id_width_wanted - id_width_given;
	begin
		case lz is
			when 0 => -- no leading zeroes
				return (type_device_name_prefix.to_string (reference.prefix) 
					& trim(natural'image (reference.id),left));
				
			when others => -- leading zeros required
				return (type_device_name_prefix.to_string (reference.prefix) 
					& lz * '0' & trim(natural'image (reference.id),left));
		end case;
	end to_string;
	
	function prefix (reference : in type_device_name) return type_device_name_prefix.bounded_string is
	-- Returns the prefix of the given device name.
	begin
		return reference.prefix;
	end prefix;

	
end et_libraries;

-- Soli Deo Gloria

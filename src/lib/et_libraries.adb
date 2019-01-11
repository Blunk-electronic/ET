------------------------------------------------------------------------------
--                                                                          --
--                           SYSTEM ET LIBRARIES                            --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2018 Mario Blunk, Blunk electronic                 --
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
with et_configuration;

package body et_libraries is

-- 	function to_string (full_library_name : in type_full_library_name.bounded_string) return string is
-- 	-- Returns the given full library name as string;
-- 	begin
-- 		return type_full_library_name.to_string (full_library_name);
-- 	end to_string;
-- 
-- 	function to_full_library_name (full_library_name : in string) return type_full_library_name.bounded_string is
-- 	-- converts a string to a full library name.
-- 	begin
-- 		return type_full_library_name.to_bounded_string (full_library_name);
-- 	end to_full_library_name;


-- DEVICES
	function to_string (device_library_name : in type_device_library_name.bounded_string) return string is
	-- Returns the given device library name as string;
	begin
		return type_device_library_name.to_string (device_library_name);
	end to_string;

	function to_device_library_name (device_library_name : in string) return type_device_library_name.bounded_string is
	-- converts a string to a device library name.
	begin
		return type_device_library_name.to_bounded_string (device_library_name);
	end to_device_library_name;
	
-- SYMBOLS
	function to_string (symbol_library_name : in type_symbol_library_name.bounded_string) return string is
	-- Returns the given symbol library name as string;
	begin
		return type_symbol_library_name.to_string (symbol_library_name);
	end to_string;

	function to_symbol_library_name (symbol_library_name : in string) return type_symbol_library_name.bounded_string is
	-- converts a string to a device library name.
	begin
		return type_symbol_library_name.to_bounded_string (symbol_library_name);
	end to_symbol_library_name;

-- PACKAGES
	function to_string (package_library_name : in type_package_library_name.bounded_string) return string is
	-- Returns the given package library name as string;
	begin
		return type_package_library_name.to_string (package_library_name);
	end to_string;

	function to_package_library_name (package_library_name : in string) return type_package_library_name.bounded_string is
	-- converts a string to a device library name.
	begin
		return type_package_library_name.to_bounded_string (package_library_name);
	end to_package_library_name;

	
	
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

	function to_port_direction (direction : in string) return type_port_direction is begin
		return type_port_direction'value (direction);
	end to_port_direction;
		
	function to_string (
		direction	: in type_port_direction;
		preamble	: in boolean := true) return string is
	-- Returns the given port direction as string.
	begin
		if preamble then
			return " direction " & to_lower (type_port_direction'image (direction));
		else
			return latin_1.space & to_lower (type_port_direction'image (direction));
		end if;
	end to_string;

	function to_string (characteristic : in type_port_characteristic) return string is begin
		return latin_1.space & to_lower (type_port_characteristic'image (characteristic));
	end to_string;

	function to_string (port_visible : in type_port_name_visible) return string is begin
		return latin_1.space & to_lower (type_port_name_visible'image (port_visible));
	end to_string;

	function to_string (terminal_visible : in type_terminal_name_visible) return string is begin
		return latin_1.space & to_lower (type_terminal_name_visible'image (terminal_visible));
	end to_string;
	
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
			log_indentation_reset;
			log (message_error & "max. number of characters for package variant name is" 
				 & positive'image (component_variant_name_length_max) & " !",
				 console => true);
			raise constraint_error;
		end if;
	end check_variant_name_length;
	
	procedure check_variant_name_characters (
		variant		: in type_component_variant_name.bounded_string;
		characters	: in character_set := component_variant_name_characters) is
	-- Tests if the given variant name contains only valid characters as specified
	-- by given character set.
	-- Raises exception if invalid character found.
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
			log_indentation_reset;
			log (message_error & "invalid character in variant name '" 
				& to_string (variant) & "' at position" & natural'image (invalid_character_position),
				console => true);
			raise constraint_error;
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
	
	procedure check_generic_name_characters (
	-- Checks if the given generic component name meets certain conventions.
		name : in type_component_generic_name.bounded_string; -- TRANSISTOR_NPN
		characters : in character_set := component_generic_name_characters) is

		use et_string_processing;
		invalid_character_position : natural := 0;

	begin
		-- Test given generic name and get position of possible invalid characters.
		invalid_character_position := index (
			source => name,
			set => characters,
			test => outside);

		-- CS: test if tilde is the first character of the generic name.
		-- This requires a special test that allows a tilde at ONLY this position.
				
		-- Evaluate position of invalid character.
		if invalid_character_position > 0 then
			log_indentation_reset;
			log (message_error & "invalid character in generic component name '" 
				& to_string (name) & "' at position" & natural'image (invalid_character_position),
				console => true);
			raise constraint_error;
		end if;
	end check_generic_name_characters;

	function to_string (generic_name : in type_component_generic_name.bounded_string) return string is
	-- Returns the given generic name as as string.
	-- CS: provide a parameter that turns the pretext like "generic name" on/off
	begin
		--return ("generic name " & type_component_generic_name.to_string (name_in_library));
		return (type_component_generic_name.to_string (generic_name));
	end to_string;

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
			log_indentation_reset;
			log (message_error & "max. number of characters for part code is" 
				 & positive'image (component_partcode_length_max) & " !",
				 console => true);
			raise constraint_error;
		end if;
	end check_partcode_length;
	
	procedure check_partcode_characters (
		partcode	: in type_component_partcode.bounded_string;
		characters	: in character_set := component_partcode_characters) is
	-- Tests if the given partcode contains only valid characters as specified
	-- by given character set.
	-- Raises exception if invalid character found.
		use et_string_processing;
		use type_component_partcode;
		invalid_character_position : natural := 0;
	begin
		invalid_character_position := index (
			source => partcode,
			set => characters,
			test => outside);

		if invalid_character_position > 0 then
			log_indentation_reset;
			log (message_error & "component part code " & to_string (partcode) 
				 & " has invalid character at position"
				 & natural'image (invalid_character_position),
				console => true
				);
			raise constraint_error;
		end if;
	end check_partcode_characters;

	function compare_date (left, right : in type_component_date) return boolean is
	-- Returns true if left comes before right.
	-- If left equals right, the return is false.
	begin
		-- 2017-12-31T23:55:04 is before 2017-12-31T23:55:06
		if left < right then
			return true;
		else
			return false;
		end if;
	end compare_date;
	
	function to_string (
		date	: in type_component_date;
		verbose	: in boolean := false)
		return string is
	-- Returns the given date as string.
	begin
		if verbose then
			return " date " & string (date);
		else
			return latin_1.space & string (date);
		end if;
	end to_string;
	
	procedure date_format_error (date : in string) is
		use et_string_processing;
	begin
		log_indentation_reset;
		log (message_error & date & " date format invalid ! Expected format is " 
			& component_date_format & " Example: " & string (component_date_example),
			console => true);
		raise constraint_error;
	end date_format_error;

	procedure check_date_length (date : in string) is	
-- 		use et_string_processing;
	begin
		if date'length /= component_date_length then
-- 			log ("date length is     " & natural'image (date'length));
-- 			log ("date length should " & natural'image (component_date_length));
			date_format_error (date);
		end if;
	end check_date_length;
		
	procedure check_date_characters (
		date		: in type_component_date;
		characters	: in character_set := component_date_characters) is
	-- Tests if the given date contains only valid characters as specified
	-- by given character set.
	-- Raises exception if invalid character found.
		use et_string_processing;
		invalid_character_position : natural := 0;
	begin
		invalid_character_position := index (
			source => string (date),
			set => characters,
			test => outside);

		if invalid_character_position > 0 then
			log_indentation_reset;
			log (message_error & "component date " & string (date)
				 & " has invalid character at position"
				 & natural'image (invalid_character_position),
				console => true
				);
			raise constraint_error;
		end if;
	end check_date_characters;

	procedure check_author_length (author : in string) is
	-- Tests if the given author is longer than allowed.
		use et_string_processing;
	begin
		if author'length > component_author_length_max then
			log_indentation_reset;
			log (message_error & "max. number of characters for author name is" 
				 & positive'image (component_author_length_max) & " !",
				 console => true);
			raise constraint_error;
		end if;
	end check_author_length;
	
	procedure check_author_characters (
		author		: in type_component_author.bounded_string;
		characters	: in character_set := component_author_characters) is
	-- Tests if the given author contains only valid characters as specified
	-- by given character set.
	-- Raises exception if invalid character found.
		use et_string_processing;
		use type_component_author;
		invalid_character_position : natural := 0;
	begin
		invalid_character_position := index (
			source => author,
			set => characters,
			test => outside);

		if invalid_character_position > 0 then
			log_indentation_reset;
			log (message_error & "component author " & to_string (author)
				 & " has invalid character at position"
				 & natural'image (invalid_character_position),
				console => true
				);
			raise constraint_error;
		end if;
	end check_author_characters;

	function to_string (fill : in type_fill) return string is begin
		return latin_1.space & to_lower (type_fill_border'image (fill.border))
		& latin_1.space & "pattern" & latin_1.space 
		& to_lower (type_fill_pattern'image (fill.pattern));
	end to_string;
	
-- 	function to_string (variant : in type_component_variant) return string is
-- 	-- Returns the given variant as string.
-- 	-- NOTE: This displays the type_component_variant (see et_libraries.ads).
-- 	-- Do not confuse with type_variant (see et_schematic.ads) which also contains the variant name
-- 	-- like in TL084D or TL084N.
-- 	-- CS: provide a parameter that turns the pretext on/off ? Useful ?
-- 
-- 	-- If the library name or package name of given variant is empty, assume item_not_specified.
-- 	-- This produces a still readable output like "library item_not_specified package item_not_specified".
-- 		v : type_component_variant := variant;
-- 		use et_string_processing;
-- 	begin
-- 		if type_full_library_name.length (v.library) = 0 then
-- 			v.library := type_full_library_name.to_bounded_string (item_not_specified);
-- 		end if;
-- 
-- 		if type_component_package_name.length(v.packge) = 0 then
-- 			v.packge := type_component_package_name.to_bounded_string (item_not_specified);
-- 		end if;
-- 		
-- 		return ("library " & type_full_library_name.to_string (v.library)
-- 			& " package " & type_component_package_name.to_string (v.packge));
-- 	end to_string;


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
	
	procedure check_value_length (value : in string) is
	-- Tests if the given value is longer than allowed.
		use et_string_processing;
	begin
		if value'length > component_value_length_max then
			log_indentation_reset;
			log (message_error & "max. number of characters for value is" 
				 & positive'image (component_value_length_max) & " !",
				console => true);
			raise constraint_error;
		end if;
	end check_value_length;
	
	procedure check_value_characters (
		value : in type_component_value.bounded_string;
		characters : in character_set := component_value_characters) is
	-- Tests if the given value contains only valid characters as specified
	-- by given character set.
	-- Raises exception if invalid character found.
		use et_string_processing;
		use type_component_value;
		invalid_character_position : natural := 0;
	begin
		invalid_character_position := index (
			source => value,
			set => characters,
			test => outside);

		if invalid_character_position > 0 then
			log_indentation_reset;
			log (message_error & "component value " & to_string (value) 
				 & " has invalid character at position"
				 & natural'image (invalid_character_position),
				 console => true
				);
			raise constraint_error;
		end if;
	end check_value_characters;


	
	function to_string (prefix : in type_component_prefix.bounded_string) return string is
	-- returns the given prefix as string
	begin
		return type_component_prefix.to_string (prefix); -- leading space not allowd !
	end to_string;

	function to_prefix (prefix : in string) return type_component_prefix.bounded_string is begin
		return type_component_prefix.to_bounded_string (prefix);
	end to_prefix;

	procedure check_prefix_length (prefix : in string) is
	-- Tests if the given prefix is longer than allowed.
		use et_string_processing;
	begin
		if prefix'length > component_prefix_length_max then
			log_indentation_reset;
			log (message_error & "max. number of characters for component prefix is" 
				 & positive'image (component_prefix_length_max) & " !",
				console => true);
			raise constraint_error;
		end if;
	end check_prefix_length;
	
	procedure check_prefix_characters (
		prefix : in type_component_prefix.bounded_string;
		characters : in character_set) is
	-- Tests if the given prefix contains only valid characters as specified
	-- by given character set. Raises exception if invalid character found.
		use et_string_processing;
		invalid_character_position : natural := 0;
	begin
		invalid_character_position := index (
			source => prefix,
			set => characters,
			test => outside);

		if invalid_character_position > 0 then
			log_indentation_reset;
			log (message_error & "component prefix " & to_string (prefix) 
				 & " has invalid character at position"
				 & natural'image (invalid_character_position),
				console => true
				);
			raise constraint_error;
		end if;
	end check_prefix_characters;

	function to_string (ref_id : in type_component_reference_id) return string is begin
		return latin_1.space & type_component_reference_id'image (ref_id);
	end to_string;

	function to_reference_id (ref_id : in string) return type_component_reference_id is begin
		return type_component_reference_id'value (ref_id);
	end to_reference_id;
	
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
	
	function compose_partcode_root (
	-- The root of a partcode in general is something like R_PAC_S_0805_VAL_ .
	-- If optionally the value is provided, it gets appended which would result
	-- in something like R_PAC_S_0805_VAL_100R.
		prefix		: in type_component_prefix.bounded_string;			-- R
		packge		: in type_component_package_name.bounded_string;	-- S_0805
		value 		: in type_component_value.bounded_string := type_component_value.to_bounded_string ("")) -- 100R
		return type_component_partcode.bounded_string is

		use type_component_prefix;
		use type_component_package_name;
		use type_component_value;
		use type_component_partcode;
		use et_configuration;
	begin
		return to_bounded_string (
			to_string (prefix)				-- R
			& partcode_keyword_separator	-- _
			& to_partcode_keyword (COMPONENT_PACKAGE) -- PAC
			& partcode_keyword_separator	-- _
			& to_string (packge)			-- S_0805
			& partcode_keyword_separator	-- _
			& to_partcode_keyword (COMPONENT_VALUE) -- VAL
			& partcode_keyword_separator	-- _
			& to_string (value)				-- 100R
			);
	end compose_partcode_root;

	procedure validate_other_partcode_keywords (
	-- Validates optional keywords as specified in configuration file.
	-- Starts the validation from the given character position.
		partcode		: in type_component_partcode.bounded_string; -- R_PAC_S_0805_VAL_100R_TOL_5_PMAX_0W125
		from			: in positive; -- the character position to start from
		log_threshold	: in et_string_processing.type_log_level) is

		use et_configuration.type_partcode_keywords;
		use et_libraries.type_component_partcode;
		use et_configuration;
		use et_configuration.type_partcode_keyword_argument;
		use et_string_processing;
		
		len 		: positive := length (partcode); 	-- the length of the given partcode
		place 		: positive := from; 				-- the position of the character being processed
		keyword_end : positive;							-- the last character position of the current keyword

		keyword_follows : boolean;	-- goes true if a keyword is expected next

		keyword : type_partcode_keyword.bounded_string;	-- the keyword being processed

		argument_start : positive;
		argument : type_partcode_keyword_argument.bounded_string; -- the argument being processed

		procedure validate_argument (
			kw : in type_partcode_keyword.bounded_string;
			arg : in type_partcode_keyword_argument.bounded_string) is
		begin
			log ("keyword " & to_string (kw) 
				 & " argument " & to_string (argument => argument), log_threshold + 1);

			-- CS: currently no validation ! Here the argument could be checked against the keyword
			-- example: after PMAX must follow something like 15 (for 15W watts)
			-- after VMAX must follow 6V3 ...
			-- Instead of comma, use unit of measurement (same scheme as in component value).
			-- The format of the argument should be specified in the configuration file.
		end validate_argument;
			
	begin -- validate_other_partcode_keywords
		log ("optional keywords ...", log_threshold);
		log_indentation_up;

		-- If the first character to start with, is a separator, then an argument follows.
		-- Otherwise a keyword follows. Example:
		-- _100R_TOL_5_PMAX_0W125 -> argument follows
		-- x_TOL_5_PMAX_0W125 -> keyword follows
		if element (partcode, place) = partcode_keyword_separator then
			keyword_follows := false;
		else
			keyword_follows := true;
		end if;

		-- advance through the partcode characters
		while place < len loop

			if keyword_follows then
				-- log ("reading keyword");				
				if element (partcode, place) = partcode_keyword_separator then
					place := place + 1;
					keyword_end := index (partcode, (1 => partcode_keyword_separator), from => place) - 1;
					
					keyword := to_partcode_keyword (slice (partcode, place, keyword_end));
					log ("keyword " & to_string (keyword), log_threshold + 2);
					
					place := keyword_end + 1; -- point to separator right after keyword
					argument_start := place + 1; -- so the argument is expected after the separator
					
					keyword_follows := false;
					validate_partcode_keyword (keyword);
					
					-- A keyword must occur only once. Otherwise raise error:
					if type_component_partcode.count (partcode, to_string (keyword)) > 1 then
						log_indentation_reset;
						log (message_error & "keyword " & to_string (keyword) & " can be used only once !");
						raise constraint_error;
					end if;
				else
					place := place + 1;	-- next character of keyword
				end if;

			else -- argument follows
				-- log ("reading argument");
				place := place + 1;
				-- If the argument starts, "place" points to the first character of the argument.

				-- If a separator occurs, the argument ends.
				if element (partcode, place) = partcode_keyword_separator then

					-- detect missing argument
					if place = argument_start then
						log_indentation_reset;
						log (message_error & "expect argument at position" & positive'image (place) & " !");
						raise constraint_error;
					end if;
					
					keyword_follows := true;

					-- The argument can now be sliced from argument_start to the place before the separator:
					argument := to_partcode_keyword_argument (slice (partcode, argument_start, place - 1));
					validate_argument (keyword, argument);
					
				elsif place = len then -- last argument in partcode
					
					-- The argument can now be sliced from argument_start to the end of the partcode:
					argument := to_partcode_keyword_argument (slice (partcode, argument_start, place));
					validate_argument (keyword, argument);
				end if;

			end if;
			
		end loop;

		log_indentation_down;

		exception
			when event:
				others =>
					log_indentation_reset;
					log (message_error & "part code " & to_string (partcode) & " invalid !", console => true);
					log (ada.exceptions.exception_message (event));
					raise;
		
	end validate_other_partcode_keywords;
	
	procedure validate_component_partcode_in_library (
	-- Tests if the given partcode of a library component is correct.
	-- The given properties are assumed to be those of a real component.
	--  - If partcode keywords are not specified in the 
	--    configuration file, nothing is validated. It is the users responsibility 
	--    to specify a correct partcode.
	--  - If partcode keywords are specified in the configuration file,
	--    the root part (like R_PAC_S_0805_VAL_) is validated.
		partcode		: in type_component_partcode.bounded_string;		-- R_PAC_S_0805_VAL_
		name			: in type_component_generic_name.bounded_string;	-- 74LS00
		prefix			: in type_component_prefix.bounded_string;			-- R
		packge			: in type_component_package_name.bounded_string;	-- S_0805
		log_threshold	: in et_string_processing.type_log_level)
		is

		use et_configuration;
		use et_string_processing;
		use type_component_partcode;

		partcode_expect : type_component_partcode.bounded_string;		
		place : natural;
		
		procedure partcode_invalid is
		begin
			log_indentation_reset;
			log (message_error & "component " & to_string (name)
				 & " part code invalid !", console => true);
			log ("found    '" & to_string (partcode) & "'", console => true);
			log ("expected '" & to_string (partcode_expect) & "'", console => true);
			raise constraint_error;
		end partcode_invalid;
	
	begin -- validate_component_partcode_in_library
		log ("validating part code in library ...", log_threshold);
		log_indentation_up;
		
		if partcode_keywords_specified then

			log (to_string (partcode), log_threshold + 1);
			
			-- Compose the root of the partcode as it should be.
			-- The root is usually something like R_PAC_S_0805_VAL_ which contains
			-- the given prefix and package name.
			partcode_expect := compose_partcode_root (
				prefix => prefix,
				packge => packge);

			-- the root of the partcode must be the very first part of the given partcode.
			place := index (partcode, to_string (partcode_expect));
			if place /= 1 then
				partcode_invalid;
			end if;

			validate_other_partcode_keywords (
				partcode => partcode, 				-- the partcode to be validated
				from => length (partcode_expect), 	-- last character position of root part code
				log_threshold => log_threshold + 1);
			
		end if;

		log_indentation_down;
	end validate_component_partcode_in_library;

	
	procedure validate_component_partcode_in_schematic ( -- CS move to et_schematic
	-- Tests if the given partcode of a schematic component is correct.
	-- The given properties are assumed to be those of a real component.
	--  - If partcode keywords are not specified in the 
	--    configuration file, nothing is validated. It is the users responsibility 
	--    to specify a correct partcode.
	--  - If partcode keywords are specified in the configuration file,
	--    the root part (like R_PAC_S_0805_VAL_) is validated.
		partcode		: in type_component_partcode.bounded_string;		-- R_PAC_S_0805_VAL_100R
		reference		: in type_component_reference;						-- R45
		packge			: in type_component_package_name.bounded_string;	-- S_0805
		value 			: in type_component_value.bounded_string;			-- 100R
		log_threshold	: in et_string_processing.type_log_level)
		is

		use et_configuration;
		use et_string_processing;
		use type_component_partcode;

		place : natural;
		partcode_expect : type_component_partcode.bounded_string;
		
		procedure partcode_invalid is
		begin
			log_indentation_reset;
			log (message_error & "component " & to_string (reference)
				 & " part code invalid !", console => true);
			log ("found    '" & to_string (partcode) & "'", console => true);
			log ("expected '" & to_string (partcode_expect) & "'", console => true);
			raise constraint_error;
		end partcode_invalid;

	begin -- validate_component_partcode_in_schematic
		log ("validating part code in schematic ...", log_threshold);
		log_indentation_up;
		
		if partcode_keywords_specified then

			-- Compose the root of the partcode as it should be.
			-- The root is usually something like R_PAC_S_0805_VAL_100R which contains
			-- the given prefix, package name and - if provided - the value.
			partcode_expect := compose_partcode_root (
				prefix => reference.prefix,
				packge => packge,
				value => value);

			-- the root of the partcode must be the very first part of the given partcode.
			place := index (partcode, to_string (partcode_expect));
			if place /= 1 then
				partcode_invalid;
			end if;

			validate_other_partcode_keywords (
				partcode => partcode, -- the partcode to be validated
				from => length (partcode_expect), -- last character position of root part code
				log_threshold => log_threshold + 1);

		end if;

		log_indentation_down;
	end validate_component_partcode_in_schematic;

	
	procedure validate_component_value (
	-- Tests if the given component value meets certain conventions.
	-- This test depends on the category of the component. If no prefixes specified
	-- in the configuration file, this test does nothing.
		value 		: in type_component_value.bounded_string;
		reference	: in type_component_reference;
		appearance	: in type_component_appearance)
		is

		use et_libraries.type_component_value;
		use et_string_processing;
		use et_configuration;

		component_category : type_component_category;
		value_length : natural := type_component_value.length (value);

		procedure value_invalid is
		begin
			log_indentation_reset;
			log (message_error & "component " & to_string (reference) 
				& " value " & to_string (value) & " invalid ! Check unit of measurement !",
				console => true);
			raise constraint_error;
		end value_invalid;

		procedure no_value_warning is
		begin
			log (message_warning & "component " & to_string (reference) 
				& " has no value !");
		end no_value_warning;
		
		procedure no_value_error is
		begin
			log_indentation_reset;
			log (message_error & "component " & to_string (reference) 
				& " has no value !", 
				console => true);
			raise constraint_error;
		end no_value_error;
		
		procedure test_unit_of_measurement is
		-- Tests if the unit of measurement is valid and placed properly in something like 220k56 .
		-- Tests if the first character is a digit.
			use ada.strings.maps.constants;
			place		: positive := 1; -- the pointer to the character being examined
			char 		: character; -- the character being examined

			unit_start	: natural; -- the position where the unit of measurement begins
			unit_ok 	: boolean := false; -- goes true once the unit of measurement is considered as ok
		
			use type_unit_abbrevation;
			use type_units_of_measurement;
			
			-- This cursor points to the unit of measurement being probed
			unit_cursor : type_units_of_measurement.cursor := component_units.first;

			procedure test_if_unit_ok is
			-- Raises alarm if unit_ok if false.
			begin
				if not unit_ok then
					value_invalid;
				end if;
			end test_if_unit_ok;

			function unit_found return boolean is
			-- Sets unit_ok flag and returns true if the unit (indicated by unit_cursor)
			-- is placed at position "place".
			-- Advances "place" to the position of the last character of the unit.
			begin
				unit_start := index (value, to_string (element (unit_cursor)), place);
				if unit_start = place then
					-- unit valid. advance place to end of unit and return true.
					place := place + length (element (unit_cursor)) - 1;
					unit_ok := true;
					return true;
				else
					-- unit invalid. return false
					unit_ok := false;
					return false;
				end if;
			end unit_found;
			
		begin -- test_unit_of_measurement
			-- We process one character after another in the given value.
			while place <= value_length loop
				char := element (value, place);

				-- Test if first character is a digit.
				if place = 1 and not is_digit (char) then
					value_invalid;
				end if;
				
				-- Initially we assume there has no unit of measurement been found.
				-- So we advance until the first non-digit is found to check the unit.
				-- Once a valid unit was found, we expect ONLY digits after the unit 
				-- of measurement.
				if not unit_ok then
				
					if not is_digit (char) then -- integer part complete
						-- Now the unit of measurement begins.

						-- Probe units of measurement according to component category.
						-- For picking up the units we use the map key (KILOOHM, MIRCROFARAD, ...)
						-- in order to get to the actual unit as defined in configuration file (k, u, ...).
						-- Unit_start becomes greater zero if the unit of measurement has been found
						-- at the current place.
						case component_category is

							when BATTERY =>
								while unit_cursor /= type_units_of_measurement.no_element loop
									case key (unit_cursor) is
										when VOLT =>
											if unit_found then exit; end if;
										when others => null;
									end case;
									next (unit_cursor);
								end loop;
								test_if_unit_ok;
							
							when CAPACITOR =>
								while unit_cursor /= type_units_of_measurement.no_element loop
									case key (unit_cursor) is
										when PICOFARAD | NANOFARAD | MICROFARAD | MILLIFARAD | FARAD =>
											if unit_found then exit; end if;
										when others => null;
									end case;
									next (unit_cursor);
								end loop;
								test_if_unit_ok;

							when FUSE =>
								while unit_cursor /= type_units_of_measurement.no_element loop
									case key (unit_cursor) is
										when MILLIAMPERE | AMPERE =>
											if unit_found then exit; end if;
										when others => null;
									end case;
									next (unit_cursor);
								end loop;
								test_if_unit_ok;
								
							when INDUCTOR =>
								while unit_cursor /= type_units_of_measurement.no_element loop
									case key (unit_cursor) is
										when NANOHENRY | MICROHENRY | MILLIHENRY | HENRY =>
											if unit_found then exit; end if;
										when others => null;
									end case;
									next (unit_cursor);
								end loop;
								test_if_unit_ok;
								
							when RESISTOR | RESISTOR_NETWORK =>
								while unit_cursor /= type_units_of_measurement.no_element loop
									case key (unit_cursor) is
										when MILLIOHM | OHM | KILOOHM | MEGAOHM | GIGAOHM =>
											if unit_found then exit; end if;
										when others => null;
									end case;
									next (unit_cursor);
								end loop;
								test_if_unit_ok;

							when QUARTZ =>
								while unit_cursor /= type_units_of_measurement.no_element loop
									case key (unit_cursor) is
										when KILOHERTZ | MEGAHERTZ | GIGAHERTZ =>
											if unit_found then exit; end if;
										when others => null;
									end case;
									next (unit_cursor);
								end loop;
								test_if_unit_ok;
								
								
							when others => null;
								
						end case;
					end if;

				else 
					-- Unit has been found valid. 
					-- Expect trailing digits exclusively after the unit of measurement.
					-- Abort on non-digit charcters.
					if not is_digit (char) then
						value_invalid;
					end if;
				end if;

				-- advance to next character in given value
				place := place + 1;
			end loop;

			-- After processing the given value, if no valid unit of measurement found, abort.
			test_if_unit_ok;

		end test_unit_of_measurement;

	begin -- validate_component_value
		-- Do the test if component prefixes specified. Otherwise do nothing.
		if component_prefixes_specified then
	
			-- If a value is provided, means it has non-zero length we conduct some tests.
			-- If no value provided, the category determines whether to abort or not.
			if value_length > 0 then

				-- Rule for real components only: 
				-- Units of measurement must be in accordance with the component category
				case appearance is
					
					when sch_pcb => 

						component_category := category (reference);
						
						-- For certain component categories the value must start 
						-- with a digit (like 3n3, 1V8, ...):
						case component_category is
							when BATTERY | CAPACITOR | FUSE | INDUCTOR | RESISTOR | RESISTOR_NETWORK | QUARTZ => -- CS: others ?
								test_unit_of_measurement;

							when others => null;
						end case;

						
					when others => null; -- CS: value check for others ?
				end case;

				
			else
				-- no value provided
				
				-- For certain component categories there is no need for a value. The properties of such parts
				-- are available via the part code.
				-- NOTE: Some CAE tools insist on a value. KiCad does. EAGLE does not.
				-- For other categories (R, L, C, ...) the value is essential for reading and understanding the schematic.
				case appearance is
					when sch_pcb =>
						case category (reference) is

							-- no value required for:
							when HEATSINK | JUMPER | MOTOR | MICROPHONE | NETCHANGER | SWITCH | TESTPOINT | CONNECTOR =>
								null;

							-- value essential for all other categories:
							when others =>
								no_value_error;

						end case;

					when others => no_value_error; -- CS: probably it would be sufficient to output a warning instead (use no_value_warning)
						-- CS: check value against generic name in libarary ?
				end case;
						
			end if;

		end if;
			
		exception
			when others => 
				-- CS: explain more detailled what is wrong
				value_invalid;

	end validate_component_value;

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
			log_indentation_reset;
			log (message_error & "max. number of characters for package/footprint is" 
				 & positive'image (component_package_name_length_max) & " !",
				console => true);
			raise constraint_error;
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
			log_indentation_reset;
			log (message_error & "component package name " & to_string (packge) 
				 & " has invalid character at position"
				 & natural'image (invalid_character_position),
				console => true
				);
			raise constraint_error;
		end if;
	end check_package_name_characters;
	
	procedure validate_component_package_name (name : in type_component_package_name.bounded_string) is
	-- Tests if the given component package name meets certain conventions.
		use type_component_package_name;
		use et_string_processing;
		
		procedure no_package is
		begin
			log_indentation_reset;
			log (message_error & "no package associated !", 
				console => true);
			raise constraint_error;
		end no_package;
			
	begin -- validate_component_package_name
		if length (name) > 0 then
			check_package_name_characters (name, component_package_name_characters);
		else
			no_package;
		end if;
	end validate_component_package_name;

	function to_string (terminals : in type_terminal_count) return string is
	-- Returns the given number of terminals as string.
	begin
		return " terminal count" & type_terminal_count'image (terminals);
	end to_string;
	
	function to_string (reference : in type_component_reference) return string is
	-- Returns the given component reference as string.
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
				return (type_component_prefix.to_string (reference.prefix) 
					& trim(natural'image (reference.id),left));
				
			when others => -- leading zeros required
				return (type_component_prefix.to_string (reference.prefix) 
					& lz * '0' & trim(natural'image (reference.id),left));
		end case;
	end to_string;
	
	function prefix (reference : in type_component_reference) return type_component_prefix.bounded_string is
	-- Returns the prefix of the given component reference.
	begin
		return reference.prefix;
	end prefix;

	procedure check_reference_characters (
	-- Tests if the given reference like IC702 (as string) contains valid characters.
	-- Unless a special character set is passed, it defaults to component_reference_characters.
		reference : in string; -- IC704
		characters : in character_set := component_reference_characters) is

		use et_string_processing;
		invalid_character_position : natural := 0;
	begin
		invalid_character_position := index (
			source => reference,
			set => characters,
			test => outside);

		if invalid_character_position > 0 then
			log_indentation_reset;
			log (message_error & "component reference " & to_string (packge) 
				 & " has invalid character at position"
				 & natural'image (invalid_character_position),
				console => true
				);
			raise constraint_error;
		end if;
	end check_reference_characters;
	
	
end et_libraries;

-- Soli Deo Gloria

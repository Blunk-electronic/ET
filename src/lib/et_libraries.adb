------------------------------------------------------------------------------
--                                                                          --
--                           SYSTEM ET LIBRARIES                            --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 Mario Blunk, Blunk electronic                 --
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

	function to_string (library_name : in type_library_name.bounded_string) return string is
	-- Returns the given library name as string.
	begin
		return type_library_name.to_string (library_name);
	end to_string;
	
	function to_string (directory : in type_library_directory.bounded_string) return string is
	-- Returns the given library directory as string;
	begin
		return type_library_directory.to_string (directory);
	end to_string;

	function to_string (full_library_name : in type_full_library_name.bounded_string) return string is
	-- Returns the given full library name as string;
	begin
		return type_full_library_name.to_string (full_library_name);
	end to_string;

	function to_string (person : in type_person_name.bounded_string) return string is
	-- Returns the given person name as string.
	begin
		return type_person_name.to_string (person);
	end to_string;

	function to_text_size (size : in type_distance) return type_text_size is
	-- Converts given size to type_text_size.
	-- Reports a warning if text size out of range.
		use et_string_processing;
	begin
		if size not in type_text_size then
			log (message_warning & "text size " & to_string (size) & " out of range !");
		end if;
		return size;
	end to_text_size;
	
	function to_string (size : in type_text_size) return string is
	-- Returns the given text size as string.
	begin
		return "size " & et_coordinates.to_string (size);
	end to_string;

	function width_to_string (width : in type_text_line_width) return string is
	-- Returns the given line width as string.
	begin
		return "width " & et_coordinates.to_string (width);
	end width_to_string;
	
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
	
	function to_string (port_name : in type_port_name.bounded_string) return string is
	-- Returns the given port name as string.
	begin
		return type_port_name.to_string (port_name);
	end to_string;

	function to_string (pin_name : in type_pin_name.bounded_string) return string is
	-- Returns the given pin name as string.
	begin
		return type_pin_name.to_string (pin_name);
	end to_string;

	procedure check_component_name (
	-- Checks if the given generic component name meets certain conventions.
		name : in type_component_name.bounded_string; -- TRANSISTOR_NPN
		customized : in boolean := false) -- when true use customized character set
		-- for the test (depends on import CAD format).
		is

		use et_import;
		use et_string_processing;
		use et_libraries.type_component_value;

		invalid_character_position : natural := 0;
		component_name_characters_customized : character_set;
	
	begin
		-- Test given generic name and get position of possible invalid characters.

		-- If a customized test is required, then for certain CAD formats 
		-- some additional characters are allowed. Otherwise the test is
		-- conducted against the default character set.
		if customized then

			case et_import.cad_format is

				when kicad_v4 =>
				-- KiCad requirement for components with the value field set to "invisible" 
				-- strange idea but we have to live with it
				-- see <https://forum.kicad.info/t/why-a-tilde-in-schematic-library/8263/6>
				-- So we extend the default character set by tilde:
					component_name_characters_customized := component_name_characters or to_set ('~');

					-- Test given generic name and get position of possible invalid characters.
					invalid_character_position := index (
						source => name,
						set => component_name_characters_customized,
						test => outside);

					-- CS: test if tilde is the first character of the generic name.
					-- This requires a special test that allows a tilde at ONLY this position.
				
				when others =>

					-- Test given generic name and get position of possible invalid characters.
					invalid_character_position := index (
						source => name,
						set => component_name_characters,
						test => outside);
					
			end case;

		else -- no customization -> default test
			-- Test given generic name and get position of possible invalid characters.
			invalid_character_position := index (
				source => name,
				set => component_name_characters,
				test => outside);
		end if;
		
		-- Evaluate position of invalid character.
		case invalid_character_position is
			when 0 => -- test passed. no forbidden characters found
				null;

			when others =>
				log_indentation_reset;
				log (
					text => message_error & "invalid character in generic component name '" 
						& to_string (name) & "' at position" & natural'image (invalid_character_position),
					console => true
					);
				raise constraint_error;
		end case;

		-- CS: other checks ?

	end check_component_name;

	function strip_tilde (generic_name : in type_component_name.bounded_string) return
		type_component_name.bounded_string is
	-- Removes a possible heading tilde character from a generic component name.
	-- example: ~TRANSISTOR_NPN becomes TRANSISTOR_NPN	
	-- This function is a kicad_v4 requirement. It has no meaning for other CAD formats and
	-- returns generic_name as it is.
		use et_import;
		length : type_component_name.length_range;
	begin
		if et_import.cad_format = kicad_v4 then
			if element (generic_name, 1) = '~' then
				length := type_component_name.length (generic_name);
				return type_component_name.bounded_slice (generic_name, 2, length);
			else
				return generic_name;
			end if;
		else
			return generic_name;
		end if;
	end strip_tilde;

	function prepend_tilde (generic_name : in type_component_name.bounded_string) return
		type_component_name.bounded_string is
	-- Prepends a heading tilde character to a generic component name.
	-- example: TRANSISTOR_NPN becomes ~TRANSISTOR_NPN
	-- This function is a kicad_v4 requirement. It has no meaning for other CAD formats and
	-- returns generic_name as it is.
		use et_import;
	begin
		if et_import.cad_format = kicad_v4 then
			return '~' & generic_name;
		else
			return generic_name;
		end if;
	end prepend_tilde;
	
	function to_string (name_in_library : in type_component_name.bounded_string) return string is
	-- Returns the given name_in_library as as string.
	-- CS: provide a parameter that turns the pretext like "name in library" on/off
	begin
		--return ("name in library " & type_component_name.to_string(name_in_library));
		return (type_component_name.to_string(name_in_library));
	end to_string;

	function to_string (partcode : in type_component_partcode.bounded_string) return string is
	-- Returns the given partcode as string.
	begin
		return type_component_partcode.to_string (partcode);
	end to_string;
	
	function to_string (purpose : in type_component_purpose.bounded_string) return string is
	-- Returns the given purpose as string.
	begin
		return type_component_purpose.to_string (purpose);
	end to_string;

	function to_string ( variant : in type_component_variant) return string is
	-- Returns the given variant as string.
	-- NOTE: This displays the type_component_variant (see et_libraries.ads).
	-- Do not confuse with type_variant (see et_schematic.ads) which also contains the variant name
	-- like in TL084D or TL084N.
	-- CS: provide a parameter that turns the pretext on/off ? Useful ?

	-- If the library name or package name of given variant is empty, assume item_not_specified.
	-- This produces a still readable output like "library item_not_specified package item_not_specified".
		v : type_component_variant := variant;
		use et_string_processing;
	begin
		if type_full_library_name.length (v.library) = 0 then
			v.library := type_full_library_name.to_bounded_string (item_not_specified);
		end if;

		if type_component_package_name.length(v.packge) = 0 then
			v.packge := type_component_package_name.to_bounded_string (item_not_specified);
		end if;
		
		return ("library " & type_full_library_name.to_string (v.library)
			& " package " & type_component_package_name.to_string (v.packge));
	end to_string;


	function to_string (meaning : in type_text_meaning) return string is
	-- Returns the given text meaning as uppercase string.
	begin
		return type_text_meaning'image (meaning);
	end to_string;

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
		log ("size " & to_string (placeholder.size), log_threshold);

		-- style
		log ("style "
			& to_lower (type_text_style'image (placeholder.style)), log_threshold);

		-- line width
		log ("line width "
			& width_to_string (placeholder.line_width), log_threshold);

		-- angle
		log (to_string (placeholder.orientation), log_threshold); 

		-- visible
		log ("visible "
			& to_lower (et_libraries.type_text_visible'image (placeholder.visible)), log_threshold);

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
		log ("field/attribute '" & et_libraries.to_string (text.meaning) & "'", level => log_threshold);
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
		log ("size " & et_coordinates.to_string (text.size), level => log_threshold + 1);

		-- style
		log ("style " & to_lower(et_libraries.type_text_style'image (text.style)),
			 level => log_threshold + 1);

		-- line width
		log ("line width " & et_coordinates.to_string (text.line_width),
			level => log_threshold + 1);

		-- orientation
		log (to_string (text.orientation), level => log_threshold + 1);

		-- visible
		log ("visible " & to_lower(et_libraries.type_text_visible'image (text.visible)),
			level => log_threshold + 1);

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
		return type_text_content.to_string(c);
	end content;

	function to_string (value : in type_component_value.bounded_string) return string is
	-- Returns the given value as string.
	begin
		return type_component_value.to_string(value);
	end to_string;

	procedure check_value_characters (
		value : in type_component_value.bounded_string;
		characters : in character_set) is
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
		return type_component_prefix.to_string (prefix);
	end to_string;

	function check_prefix_characters (
		prefix : in type_component_prefix.bounded_string;
		characters : in character_set)
		return type_component_prefix.bounded_string is
	-- Tests if the given prefix contains only valid characters as specified
	-- by given character set.
	-- Raises exception if invalid character found.
	-- Returns prefix unchanged otherwise.	
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
		
		return prefix;
	end check_prefix_characters;
	
	function to_string (appearance : in type_component_appearance) return string is
	-- Returns the given component appearance as string.
	begin
		case appearance is
			when sch =>
				return ("appears in schematic only (virtual component)");
			when sch_pcb =>
				return ("appears in schematic and layout");
			when pcb =>
				return ("appears in layout only (mechanical component)");
		end case;
	end to_string;

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

	function to_string (add_level : in type_unit_add_level) return string is
	-- Returns the given add level as string.
	begin
		return " add level " & type_unit_add_level'image (add_level);
	end to_string;

	function to_string (bom : in type_bom) return string is
	-- Returns the given bom variable as string.	
	begin
		return type_bom'image (bom);
	end to_string;

	procedure validate_bom_status (text : in string) is
	-- Validates BOM status. Case sensitive !
		use et_string_processing;
	begin
		if text = type_bom'image (YES) then
			null;
		elsif text = type_bom'image (NO) then
			null;
		else
			log_indentation_reset;
			log (message_error & "BOM status '"
					& text & "' invalid !" 
					& " Must be either "
					& to_string (YES) & " or "
					& to_string (NO) & " !",
				console => true);
			raise constraint_error;
		end if;
	end validate_bom_status;
	
	function compose_partcode_root (
		prefix		: in type_component_prefix.bounded_string;			-- R
		packge		: in type_component_package_name.bounded_string;	-- S_0805
		value 		: in type_component_value.bounded_string := type_component_value.to_bounded_string ("")) -- 100R
		return type_component_partcode.bounded_string is

		use type_component_prefix;
		use type_component_package_name;
		use type_component_value;
		use type_component_partcode;
	begin
		return to_bounded_string (
			to_string (prefix)			-- R
			& partcode_separator		-- _
			& partcode_keyword_package	-- PAC
			& partcode_separator		-- _
			& to_string (packge)		-- S_0805
			& partcode_separator		-- _
			& partcode_keyword_value	-- VAL
			& partcode_separator		-- _
			& to_string (value)			-- 100R
			);
	end compose_partcode_root;
	
	procedure validate_component_partcode_in_library (
	-- Tests if the given partcode of a library component is correct.
		partcode	: in type_component_partcode.bounded_string;		-- R_PAC_S_0805_VAL_
		name		: in type_component_name.bounded_string;			-- 74LS00
		prefix		: in type_component_prefix.bounded_string;			-- R
		packge		: in type_component_package_name.bounded_string;	-- S_0805
		bom			: in type_bom)	-- YES, NO
		is

		use et_string_processing;
		use type_component_partcode;
		
		partcode_expect : type_component_partcode.bounded_string;

		procedure partcode_invalid is
		begin
			log_indentation_reset;
			log (message_error & "component " & to_string (name)
				 & " partcode invalid !", console => true);
			log ("found    '" & to_string (partcode) & "'", console => true);
			log ("expected '" & to_string (partcode_expect) & "'", console => true);
			raise constraint_error;
		end partcode_invalid;

		place : natural;		
	
	begin
		-- The partcode must be valid for mounted components only.
		case bom is
			when YES =>

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

			when NO =>
				null;
				-- CS: expect partcode_default ?
-- 				if to_string (partcode) /= partcode_default then
-- 					partcode_default_missing;
-- 				end if;

		end case;
	end validate_component_partcode_in_library;

	
	procedure validate_component_partcode_in_schematic (
	-- Tests if the given partcode of a schematic component is correct.
		partcode	: in type_component_partcode.bounded_string;		-- R_PAC_S_0805_VAL_100R
		reference	: in type_component_reference;						-- R45
		packge		: in type_component_package_name.bounded_string;	-- S_0805
		value 		: in type_component_value.bounded_string;			-- 100R
		bom			: in type_bom)	-- YES, NO
		is
		use et_string_processing;
		use type_component_partcode;

		partcode_expect : type_component_partcode.bounded_string;
		
		procedure partcode_invalid is
		begin
			log_indentation_reset;
			log (message_error & "component " & to_string (reference)
				 & " partcode invalid !", console => true);
			log ("found    '" & to_string (partcode) & "'", console => true);
			log ("expected '" & to_string (partcode_expect) & "'", console => true);
			raise constraint_error;
		end partcode_invalid;

-- 		procedure partcode_default_missing is
-- 		begin
-- 			log (message_warning & "component " & to_string (reference)
-- 				 & " is not mounted and should have the default partcode "
-- 					-- CS: show package, value and partcode as it should be
-- 			raise constraint_error;
-- 		end partcode_default_missing;

		
		place : natural;


		
	begin -- validate_component_partcode_in_schematic

		-- The partcode must be valid for mounted components only.
		case bom is
			when YES =>

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

			when NO =>
				null;
				-- CS: expect partcode_default ?
-- 				if to_string (partcode) /= partcode_default then
-- 					partcode_default_missing;
-- 				end if;

		end case;
	end validate_component_partcode_in_schematic;

	
	procedure validate_component_value (
	-- Tests if the given component value meets certain conventions.
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
				& " value " & to_string (value) & " invalid !",
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
		
			use type_component_unit;
			use type_component_units;
			
			-- This cursor points to the unit of measurement being probed
			unit_cursor : type_component_units.cursor := component_units.first;

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
								while unit_cursor /= type_component_units.no_element loop
									case key (unit_cursor) is
										when VOLT =>
											if unit_found then exit; end if;
										when others => null;
									end case;
									next (unit_cursor);
								end loop;
								test_if_unit_ok;
							
							when CAPACITOR =>
								while unit_cursor /= type_component_units.no_element loop
									case key (unit_cursor) is
										when PICOFARAD | NANOFARAD | MICROFARAD | MILLIFARAD | FARAD =>
											if unit_found then exit; end if;
										when others => null;
									end case;
									next (unit_cursor);
								end loop;
								test_if_unit_ok;

							when FUSE =>
								while unit_cursor /= type_component_units.no_element loop
									case key (unit_cursor) is
										when MILLIAMPERE | AMPERE =>
											if unit_found then exit; end if;
										when others => null;
									end case;
									next (unit_cursor);
								end loop;
								test_if_unit_ok;
								
							when INDUCTOR =>
								while unit_cursor /= type_component_units.no_element loop
									case key (unit_cursor) is
										when NANOHENRY | MICROHENRY | MILLIHENRY | HENRY =>
											if unit_found then exit; end if;
										when others => null;
									end case;
									next (unit_cursor);
								end loop;
								test_if_unit_ok;
								
							when RESISTOR | RESISTOR_NETWORK =>
								while unit_cursor /= type_component_units.no_element loop
									case key (unit_cursor) is
										when MILLIOHM | OHM | KILOOHM | MEGAOHM | GIGAOHM =>
											if unit_found then exit; end if;
										when others => null;
									end case;
									next (unit_cursor);
								end loop;
								test_if_unit_ok;

							when QUARTZ =>
								while unit_cursor /= type_component_units.no_element loop
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
		-- If a value is provided, means it has non-zero length we conduct some tests.
		-- If no value provided, the category determines whether to abort or not.
		if value_length > 0 then

			-- Rule #1: There are only those characters allowed as specified 
			-- in component_value_characters:
			check_value_characters (
				value => value,
				characters => component_value_characters);

			-- Rule #2 for real components only: 
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
		
	procedure check_package_characters (
		packge		: in type_component_package_name.bounded_string;
		characters	: in character_set)
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
	end check_package_characters;
	
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
			
	begin
		if length (name) > 0 then
			check_package_characters (name, component_package_characters);
		else
			no_package;
		end if;
	end validate_component_package_name;
	
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
				return (type_component_prefix.to_string(reference.prefix) 
					& trim(natural'image(reference.id),left));
				
			when others => -- leading zeros required
				return (type_component_prefix.to_string(reference.prefix) 
					& lz * '0' & trim(natural'image(reference.id),left));
		end case;
	end to_string;
	
	function prefix (reference : in type_component_reference) return type_component_prefix.bounded_string is
	-- Returns the prefix of the given component reference.
	begin
		return reference.prefix;
	end prefix;

	function component_appearance (cursor : in type_components.cursor)
	-- Returns the component appearance where cursor points to.
		return type_component_appearance is
	begin
		return type_components.element (cursor).appearance;
	end component_appearance;

--	CS: currently there is no need for a component summary	
-- 	procedure write_component_properties (component : in type_components.cursor) is
-- 	-- Writes the properties of the component indicated by the given cursor.
-- 		use et_string_processing;
-- 		use et_libraries;
-- 		use et_libraries.type_units_internal;
-- 		
-- 		unit_cursor : type_units_internal.cursor;
-- 		unit_count	: count_type;
-- 		units		: type_units_internal.map;
-- 
-- 	begin
-- 		log ("component properties");
-- 		
-- 		-- component name in library
-- 		log ("name " & type_component_name.to_string (type_components.key (component)));
-- 
-- 		-- number of internal units
-- 		unit_count := length (element (component).units_internal);
-- 		
-- 		log ("number of internal units" & count_type'image (unit_count));
-- 
-- 		-- write unit properties
-- 		
-- 		-- NOTE: As a workaround we load units here temporarily
-- 		-- NOTE: with GNAT 4.8 .. 7.x it is not possible to advance the unit_cursor with "next". The program gets caught
-- 		-- in an infinite loop. So the workaround here is to copy the whole units_internal map to units and move
-- 		-- cursor in the local map "units".
-- 		units := element (component).units_internal;
-- 
-- 		case unit_count is
-- 
-- 			when 0 => 
-- 				-- component has no units 
-- 				raise constraint_error; -- CS: this should never happen
-- 				
-- 			when others =>
-- 
-- 				-- The initial idea was to set the unit_cursor as follows.
-- 				-- Statement A:
-- 				unit_cursor := first (type_components.element(component).units_internal);
-- 				-- Then the unit_cursor should be moved with the "next" procedure. This causes the program to freeze.
-- 
-- 				-- Workaround. This statement overwrites the malfunctioning cursor and solves
-- 				-- the issue for the time being. Comment this statmement to reproduce the bug:
-- 				-- Statement B:
-- 				unit_cursor := first (units);
-- 
-- 				
-- 				loop 
-- 					exit when unit_cursor = type_units_internal.no_element;
-- 					
-- 					-- put_line(standard_output, "step 1");
-- 					log ("unit " & type_unit_name.to_string (key (unit_cursor)));
-- 
-- 					-- CS: output draw objects
-- 					
-- 					-- put_line(standard_output, "step 2");
-- 
-- 					-- Here the program freezes or keeps trappend in a forever-loop if 
-- 					-- statement A is used. With statement B everyting works fine:
-- 					unit_cursor := next (unit_cursor);
-- 					-- put_line(standard_output, "step 3");
-- 
-- 				end loop;
-- 			
-- 		end case;
-- 		
-- 	end write_component_properties;

	function find_component (
	-- Searches the given library for the given component. Returns a cursor to that component.
		library		: in type_full_library_name.bounded_string;
		component	: in type_component_name.bounded_string) 
		return type_components.cursor is

		lib_cursor	: type_libraries.cursor;
		comp_cursor	: type_components.cursor := no_element;
	
		use type_libraries;
		use et_string_processing;

		procedure locate (
			library : in type_full_library_name.bounded_string;
			components : in type_components.map) is
		begin
			-- Generic names in library sometimes start with a tilde. 
			-- So, first we search for the given component without tilde.
			-- If no match, sarch for the given component with a tilde prepended.
			-- If still no match, comp_cursor is empty (no_element).
			comp_cursor := components.find (component); -- TRANSISTOR_NPN
			if comp_cursor = type_components.no_element then
				comp_cursor := components.find (prepend_tilde (component)); -- ~TRANSISTOR_NPN
				--CS: log ?
			end if;
		end locate;
	
	begin
		lib_cursor := component_libraries.find (library);

		-- If the given library exists, locate the given component therein.
		-- Otherwise generate a warning.
		if lib_cursor /= type_libraries.no_element then
			query_element (
				position => lib_cursor,
				process => locate'access);
		else
			log (message_warning & "library " & to_string (library) & " not found !");
			-- CS: raise constraint_error ?
		end if;

		return comp_cursor;
	end find_component;

	function first_internal_unit (
	-- Returns the cursor to the first unit of the given component.
		component_cursor : in type_components.cursor)
		return type_units_internal.cursor is

		unit_cursor : type_units_internal.cursor;

		procedure locate (
			name : in type_component_name.bounded_string;
			component : in type_component) is

			use type_units_internal;
			use et_string_processing;
		begin
			-- Set the unit cursor to the first unit of the component.
			unit_cursor := type_units_internal.first (component.units_internal);

			-- In case the component has no units, abort.
			if unit_cursor = type_units_internal.no_element then
				log_indentation_reset;
				log (message_error & "generic component " 
						& to_string (name_in_library => type_components.key (component_cursor)) 
						& " has no units !",
					console => true);
				raise constraint_error;
			end if;
		end locate;
	
	begin
		-- locate the component by the given component cursor
		type_components.query_element (component_cursor, locate'access);

		-- CS: do something if cursor invalid. via exception handler ?
		return unit_cursor;
	end first_internal_unit;


	function first_port (
	-- Returns the cursor to the first port of the given unit
		unit_cursor : in type_units_internal.cursor)
		return type_ports.cursor is

		port_cursor : type_ports.cursor;

		procedure locate (
			name : in type_unit_name.bounded_string;
			unit : in type_unit_internal) is

			use type_ports;
			use et_string_processing;
		begin
			-- Set the port cursor to the first port of the unit.
			port_cursor := type_ports.first (unit.symbol.ports);

			-- In case the unit has no ports, abort.
			if port_cursor = type_ports.no_element then
				--log_indentation_reset;
				log (message_warning & "generic unit " 
						& to_string (unit_name => type_units_internal.key (unit_cursor)) 
						& " has no ports !");
					--console => true);
				--CS raise constraint_error;
			end if;
		end locate;

	begin
		type_units_internal.query_element (unit_cursor, locate'access);
		
		-- CS: do something if cursor invalid. via exception handler ?
		return port_cursor;
	end first_port;



	
	procedure no_generic_model_found (
		reference : in type_component_reference; -- IC303
		library : in type_full_library_name.bounded_string; -- ../lib/transistors.lib
		generic_name : in type_component_name.bounded_string) -- TRANSISTOR_NPN
		is
		use et_string_processing;
	begin
		log_indentation_reset;
		log (message_error & "component " & to_string (reference) -- CS: output coordinates
			& " has no generic model " & to_string (generic_name)
			& " in library " & to_string (library), console => true);
		raise constraint_error;
	end no_generic_model_found;

	
end et_libraries;

-- Soli Deo Gloria

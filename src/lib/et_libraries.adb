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
--with ada.characters.handling;	use ada.characters.handling;
with ada.text_io;				use ada.text_io;
with ada.strings; 				use ada.strings;
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

	
	function to_string (packge : in type_component_package_name.bounded_string) return string is
	-- Returns the given package name as as string.
	-- CS: provide a parameter that turns the pretext on/off
	begin
		return (type_component_package_name.to_string(packge));
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


	function to_string ( meaning : in type_text_meaning) return string is
	-- Converts meaning to string.
	begin
		-- we can do a direct conversion
		return to_lower(type_text_meaning'image(meaning));
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

	function check_value_characters (
		value : in type_component_value.bounded_string;
		characters : in character_set)
		return type_component_value.bounded_string is
	-- Tests if the given value contains only valid characters as specified
	-- by given character set.
	-- Raises exception if invalid character found.
	-- Returns value unchanged otherwise.	
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
				 & natural'image (invalid_character_position));
			raise constraint_error;
		end if;
		
		return value;
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
				 & natural'image (invalid_character_position));
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
		return to_lower (type_bom'image (bom));
	end to_string;
	
	function component_value_valid (
	-- Returns true if the given component value meets certain conventions.									   
		value 		: in type_component_value.bounded_string;
		reference	: in type_component_reference;
		appearance	: in type_component_appearance)
		return boolean is

-- 		use et_libraries.type_component_value;
		use et_string_processing;
		use et_configuration;

		-- After the precheck for valid characters the value is stored here:
		value_prechecked : type_component_value.bounded_string;

		value_length : natural := type_component_value.length (value);
		char_pos : positive;
		
	begin
		-- As a general rule, each component should have a value assigned. If not issue warning.
		if value_length > 0 then

			-- Rule #1: There are only those characters allowed as specified 
			-- in component_value_characters:
			value_prechecked := check_value_characters (
				value => value,
				characters => component_value_characters);

			-- Rule #2 for real components only: 
			-- Units of measurement must be in accordance with the component category
			case appearance is
				
				when sch_pcb => 
					case category (reference) is
						when resistor => null;
							--for char_pos in 1
						when others => null;
					end case;

				when others => null; -- CS
			end case;

		else
			log (message_warning & "component " & to_string (reference) & " has no value !");
		end if;

		

		return true;
		
		exception
			when others => 
				-- CS: explain what is wrong

				return false;

	end component_value_valid;

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

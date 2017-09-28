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

	function to_string (port_name : in type_port_name.bounded_string) return string is
	-- Returns the given port name as string.
	begin
		return type_port_name.to_string (port_name);
	end to_string;
	
	function to_string (name_in_library : in type_component_name.bounded_string) return string is
	-- Returns the given name_in_library as as string.
	-- CS: provide a parameter that turns the pretext like "name in library" on/off
	begin
		--return ("name in library " & type_component_name.to_string(name_in_library));
		return (type_component_name.to_string(name_in_library));
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

	
	procedure write_placeholder_properties (placeholder : in type_text_placeholder) is
	-- Writes the properties of the given placeholder.
		use et_string_processing;
	begin
		-- meaning
		log (et_libraries.to_string(placeholder.meaning));
		log_indentation_up;
		
		-- position
		log (to_string (placeholder.position));

		-- size
		log ("size" & et_libraries.type_text_size'image (placeholder.size));

		-- style
		log ("style "
			& to_lower(et_libraries.type_text_style'image (placeholder.style)));

		-- line width
		log ("line width"
			& et_libraries.type_text_line_width'image (placeholder.line_width));

		-- angle
		log (to_string (placeholder.orientation)); 

		-- visible
		log ("visible "
			& to_lower (et_libraries.type_text_visible'image (placeholder.visible)));

		-- alignment
		log ("alignment (hor/vert) "
			& to_lower (et_libraries.type_text_alignment_horizontal'image (placeholder.alignment.horizontal))
			& "/"
			& to_lower (et_libraries.type_text_alignment_vertical'image (placeholder.alignment.vertical)));

		log_indentation_down;
	end write_placeholder_properties;


	procedure write_text_properies (text : in et_libraries.type_text) is
	-- Outputs the properties of the given text.
		use et_string_processing;
		log_threshold : type_log_level := 1;
	begin
-- 		log_indentation_up;
		
		-- meaning
		log ("text field " & et_libraries.to_string(text.meaning), level => log_threshold);
		log_indentation_up;
		
		-- position
		log (to_string (text.position), level => log_threshold);

		-- content
		if et_libraries.type_text_content.length(text.content) > 0 then
			log ("content '" & et_libraries.type_text_content.to_string(text.content) & "'",
				level => log_threshold);
		else
			log ("no content", level => log_threshold);
		end if;
		
		-- size
		log ("size" & et_libraries.type_text_size'image (text.size), level => log_threshold);

		-- style
		log ("style " & to_lower(et_libraries.type_text_style'image (text.style)),
			 level => log_threshold);

		-- line width
		log ("line width" & et_libraries.type_text_line_width'image (text.line_width),
			level => log_threshold);

		-- orientation
		log (to_string (text.orientation), level => log_threshold);

		-- visible
		log ("visible " & to_lower(et_libraries.type_text_visible'image (text.visible)),
			level => log_threshold);

		-- alignment
		log ("alignment (horizontal/vertical) "
			& to_lower (et_libraries.type_text_alignment_horizontal'image (text.alignment.horizontal))
			& "/"
			& to_lower (et_libraries.type_text_alignment_vertical'image (text.alignment.vertical)),
			level => log_threshold);

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
	
	function component_value_valid (
	-- Returns true if the given component value meets certain conventions.									   
		value 		: in type_component_value.bounded_string;
		reference	: in type_component_reference)
		return boolean is

		use et_libraries.type_component_value;
		i : natural := 0;
		v : boolean := false;
	begin
		-- Rule #1: There are only those characters allowed as specified in component_value_characters:
		i := index ( source => value, set => component_value_characters, test => outside);

		case i is
			when 0 => -- test passed. no forbidden characters found
				v := true;

			when others =>
				v := false;
				et_string_processing.write_message(
					file_handle => current_output,
					text => et_string_processing.message_error & "value '" & to_string (value) & "' contains invalid character "
						& "at position" & natural'image(i),
					console => true);
				-- CS: goto end
		end case;

		-- Rule #2: Units in accordance with the component prefix
		-- CS:

		-- goto-label here
		
		return v;
	end component_value_valid;

	
	procedure write_component_properties (component : in type_components.cursor) is
	-- Writes the properties of the component indicated by the given cursor.
		use et_string_processing;
		use et_libraries;
		use et_libraries.type_units_internal;
		
		unit_cursor : type_units_internal.cursor;
		unit_count	: count_type;
		units		: type_units_internal.map;
	begin
		log ("component properties");
		
		-- component name in library
		log ("name " & type_component_name.to_string (type_components.key (component)));

		-- number of internal units
		unit_count := length (element (component).units_internal);
		
		log ("number of internal units" & count_type'image (unit_count));

		-- write unit properties
		
		-- NOTE: As a workaround we load units here temporarily
		-- NOTE: with GNAT 4.8 .. 7.x it is not possible to advance the unit_cursor with "next". The program gets caught
		-- in an infinite loop. So the workaround here is to copy the whole units_internal map to units and move
		-- cursor in the local map "units".
		units := element (component).units_internal;

		case unit_count is

			when 0 => 
				-- component has no units 
				raise constraint_error; -- CS: this should never happen
				
			when others =>

				-- The initial idea was to set the unit_cursor as follows.
				-- Statement A:
				unit_cursor := first (type_components.element(component).units_internal);
				-- Then the unit_cursor should be moved with the "next" procedure. This causes the program to freeze.

				-- Workaround. This statement overwrites the malfunctioning cursor and solves
				-- the issue for the time being. Comment this statmement to reproduce the bug:
				-- Statement B:
				unit_cursor := first (units);

				
				loop 
					exit when unit_cursor = type_units_internal.no_element;
					
					-- put_line(standard_output, "step 1");
					log ("unit " & type_unit_name.to_string (key (unit_cursor)));

					-- CS: output draw objects
					
					-- put_line(standard_output, "step 2");

					-- Here the program freezes or keeps trappend in a forever-loop if 
					-- statement A is used. With statement B everyting works fine:
					unit_cursor := next (unit_cursor);
					-- put_line(standard_output, "step 3");

				end loop;
			
		end case;
		
	end write_component_properties;

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
			comp_cursor := components.find (component);
			if comp_cursor /= type_components.no_element then
				null; 
				-- CS: log ("found !");
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

	
end et_libraries;

-- Soli Deo Gloria

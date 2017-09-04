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

with et_schematic;				use et_schematic;

with et_geometry;				use et_geometry;

with et_general;				use et_general;
with et_string_processing;		use et_string_processing;

package body et_libraries is

	function to_string ( position : in type_coordinates) return string is
	-- Returns the given position as string.
	begin
		return coordinates_preamble
			& trim (et_libraries.type_grid'image(position.x),left) 
			& coordinates_dimension_separator
			& trim (et_libraries.type_grid'image(position.y),left);

	end to_string;

	function to_string (angle : in type_angle) return string is
	-- Returns the the given angle as string.
		preamble	: constant string (1..5) := "angle";
		suffix		: constant string (1..4) := " deg";
	begin
		return (preamble & type_angle'image (angle) & suffix);
	end to_string;

	function to_string (port_name : in type_port_name.bounded_string) return string is
	-- Returns the given port name as string.
	begin
		return type_port_name.to_string (port_name);
	end to_string;
	
	function to_string ( name_in_library : in type_component_name.bounded_string) return string is
	-- Returns the given name_in_library as as string.
	-- CS: provide a parameter that turns the pretext like "name in library" on/off
	begin
		--return ("name in library " & type_component_name.to_string(name_in_library));
		return (type_component_name.to_string(name_in_library));
	end to_string;

	function to_string ( packge : in type_component_package_name.bounded_string) return string is
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
	begin
		if type_library_full_name.length(v.library) = 0 then
			v.library := type_library_full_name.to_bounded_string(et_general.item_not_specified);
		end if;

		if type_component_package_name.length(v.packge) = 0 then
			v.packge := type_component_package_name.to_bounded_string(et_general.item_not_specified);
		end if;
		
		return ("library " & type_library_full_name.to_string(v.library)
			& " package " & type_component_package_name.to_string(v.packge));
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
		log (et_libraries.to_string (placeholder.position));

		-- size
		log ("size" & et_libraries.type_text_size'image (placeholder.size));

		-- style
		log ("style "
			& to_lower(et_libraries.type_text_style'image (placeholder.style)));

		-- line width
		log ("line width"
			& et_libraries.type_text_line_width'image (placeholder.line_width));

		-- angle
		log (et_libraries.to_string (placeholder.orientation)); 

		-- visible
		log ("visible "
			& to_lower(et_libraries.type_text_visible'image (placeholder.visible)));

		-- alignment
		log ("alignment (hor/vert) "
			& to_lower(et_libraries.type_text_alignment_horizontal'image(placeholder.alignment.horizontal))
			& "/"
			& to_lower(et_libraries.type_text_alignment_vertical'image(placeholder.alignment.vertical)));

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
		log (et_libraries.to_string (text.position), level => log_threshold);

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
		log (et_libraries.to_string (text.orientation), level => log_threshold);

		-- visible
		log ("visible " & to_lower(et_libraries.type_text_visible'image (text.visible)),
			level => log_threshold);

		-- alignment
		log ("alignment (horizontal/vertical) "
			& to_lower(et_libraries.type_text_alignment_horizontal'image(text.alignment.horizontal))
			& "/"
			& to_lower(et_libraries.type_text_alignment_vertical'image(text.alignment.vertical)),
			level => log_threshold);

-- 		log_indentation_down;
		log_indentation_down;
	end write_text_properies;


	function content ( text : in type_text) return string is
	-- Returns the content of the given text as string.
		c : type_text_content.bounded_string;
	begin
		c := text.content;
		return type_text_content.to_string(c);
	end content;




	
	function to_string ( value : in type_component_value.bounded_string) return string is
	-- Returns the given value as string.
	begin
		return type_component_value.to_string(value);
	end to_string;

	function component_value_valid (
	-- Returns true if the given component value meets certain conventions.									   
		value 		: in type_component_value.bounded_string;
		reference	: in et_general.type_component_reference)
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
					text => message_error & "value '" & to_string(value) & "' contains invalid character "
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

	
end et_libraries;

-- Soli Deo Gloria

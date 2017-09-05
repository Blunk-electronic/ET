------------------------------------------------------------------------------
--                                                                          --
--                    SYSTEM ET SCHEMATIC DECLARATIONS                        --
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
with ada.characters.handling;	use ada.characters.handling;
with ada.strings; 				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;

with ada.text_io;				use ada.text_io;

with et_general;
with et_libraries;
with et_string_processing;
with et_import;


package body et_schematic is
	
	function to_string (position : in type_coordinates) return string is
	-- Returns the given position as string.
	begin
		return coordinates_preamble
			& trim (positive'image(position.sheet_number),left) 
			& et_libraries.coordinates_dimension_separator
			& trim (et_libraries.type_grid'image(position.x),left)
			& et_libraries.coordinates_dimension_separator
			& trim (et_libraries.type_grid'image(position.y),left);

		-- CS: output in both mil and mm
		
		-- CS: exception handler
	end to_string;
	
	procedure write_label_properties (label : in type_net_label) is
	-- Writes the properties of the given net label in the logfile.
		use et_string_processing;
		use et_libraries;
	
		log_threshold : type_log_level := 1;
	begin
		log_indentation_up;
		case label.label_appearance is
			when simple =>
				log (text => "simple label");
			when tag =>
				log (text => "tag label");
				-- CS: directon, global, hierarchic, style, ...
		end case;

		log_indentation_up;
		log ("name '" & type_net_name.to_string (label.text) & "' ");
		log (to_string (et_libraries.type_coordinates (label.coordinates)), log_threshold);
		log (et_libraries.to_string (label.orientation), log_threshold);
		
		case label.label_appearance is
			when simple =>
				null;
			when tag =>
				null;
				--put("tag label ");
				-- CS: directon, global, hierarchic, style, ...
		end case;

		log_indentation_down;
		log_indentation_down;

	end write_label_properties;

	procedure write_note_properties (note : in et_schematic.type_note) is
	-- Writes the properties of the given note
		use et_string_processing;
		use et_libraries;
	
		log_threshold : type_log_level := 1;
	begin
		log_indentation_up;
		log ("text note");

		log_indentation_up;

		-- content
		if et_libraries.type_text_content.length (note.content) > 0 then
			log ("content '" & type_text_content.to_string (note.content) & "'");
		else
			log (et_string_processing.message_warning & "no content !"); 
		end if;

		
		if log_level >= log_threshold then
			
			-- position
			log (to_string (et_libraries.type_coordinates (note.coordinates)), log_threshold);
			
			-- size
			log ("size" & et_libraries.type_text_size'image (note.size));

			-- style
			log ("style " & to_lower(et_libraries.type_text_style'image (note.style)));

			-- line width
			log ("line width" & et_libraries.type_text_line_width'image (note.line_width));

			-- angle
			log (et_libraries.to_string (note.orientation));

			-- visible
			log ("visible " & to_lower(et_libraries.type_text_visible'image (note.visible)));

			-- alignment
			log ("alignment (hor/vert) "
				& to_lower(et_libraries.type_text_alignment_horizontal'image(note.alignment.horizontal))
				& "/"
				& to_lower(et_libraries.type_text_alignment_vertical'image(note.alignment.vertical)));

		end if;
		
		log_indentation_down;
		log_indentation_down;
	end write_note_properties;
	
	procedure write_component_properties ( component : in type_components.cursor) is
	-- Writes the properties of the component indicated by the given cursor.
		use et_string_processing;
	begin
		log_indentation_up;
		
		-- reference (serves as key in list of components)
		log ("component " & to_string (type_components.key(component)) & " properties");

		log_indentation_up;
		
		-- CS: library file name
		-- name in library
		log ("name in library "
			& et_libraries.to_string (type_components.element(component).name_in_library));
		
		-- value
		log ("value "
			& et_libraries.type_component_value.to_string (type_components.element(component).value));

		-- commissioned
		log ("commissioned "
			& string (type_components.element(component).commissioned));

		-- updated
		log ("updated      "
			& string (type_components.element(component).updated));

		-- author
		log ("author "
			& et_libraries.type_person_name.to_string (type_components.element(component).author));
		
		-- appearance
		log (to_string (type_components.element(component).appearance));

		-- depending on the component appearance there is more to report:
		case type_components.element(component).appearance is
			when sch_pcb =>

				-- package variant
				log (et_libraries.to_string (type_components.element(component).variant.variant));
				-- NOTE: This displays the type_component_variant (see et_libraries.ads).
				-- Do not confuse with type_variant (see et_schematic.ads) which also contains the variant name
				-- like in TL084D or TL084N.

				-- datasheet
				log ("datasheet "
					& et_libraries.type_component_datasheet.to_string (type_components.element(component).datasheet));

				-- partcode
				log ("partcode "
					& et_libraries.type_component_partcode.to_string (type_components.element(component).partcode));
				
				-- function
				log ("purpose "
					& et_libraries.type_component_purpose.to_string (type_components.element(component).purpose));
				
			when pcb => null; -- CS
			when others => null; -- CS should never happen as virtual components do not have a package
		end case;

		log_indentation_down;
		log_indentation_down;
		
	end write_component_properties;

	procedure write_unit_properties (unit : in type_units.cursor) is
	-- Writes the properties of the unit indicated by the given cursor.
		use et_string_processing;
	begin
		log_indentation_up;
		
		-- unit name
		log ("unit " 
			& et_libraries.type_unit_name.to_string (type_units.key(unit)) & " properties");

		log_indentation_up;
		
		-- alternative representation
		log ("alternative (deMorgan) representation " 
			& to_lower (et_schematic.type_alternative_representation'image (type_units.element(unit).alt_repres)));

		-- timestamp
		log ("timestamp " 
			& string (type_units.element (unit).timestamp));

		-- position
		log (et_schematic.to_string (type_units.element(unit).position));

		-- placeholders
		log ("placeholders");
		log_indentation_up;

			-- reference
			et_libraries.write_placeholder_properties (
				placeholder => type_units.element(unit).reference);

			-- value
			et_libraries.write_placeholder_properties (
				placeholder => type_units.element(unit).value);

			-- some placeholders exist depending on the component appearance
			case type_units.element(unit).appearance is
				when sch_pcb =>
					
					-- package/footprint
					et_libraries.write_placeholder_properties (
						placeholder => type_units.element(unit).packge);

					-- datasheet
					et_libraries.write_placeholder_properties (
						placeholder => type_units.element(unit).datasheet);

					-- purpose
					et_libraries.write_placeholder_properties (
						placeholder => type_units.element(unit).purpose);
					
					-- partcode
					et_libraries.write_placeholder_properties (
						placeholder => type_units.element(unit).partcode);

				when others => null;
			end case;

			-- commissioned
			et_libraries.write_placeholder_properties (
				placeholder => type_units.element(unit).commissioned);

			-- updated
			et_libraries.write_placeholder_properties (
				placeholder => type_units.element(unit).updated);

			-- author
			et_libraries.write_placeholder_properties (
				placeholder => type_units.element(unit).author);

		log_indentation_down;
		log_indentation_down;
		log_indentation_down;		
	end write_unit_properties;

	procedure write_coordinates_of_segment (segment : in type_net_segment) is
	-- Writes the start and end coordinates of a net segment.
		use et_string_processing;
		use et_libraries;
	
		log_threshold : type_log_level := 1;
	begin
		log_indentation_up;
		
		log ("start "
			& to_string (et_libraries.type_coordinates (segment.coordinates_start))
			& " end " 
			& to_string (et_libraries.type_coordinates (segment.coordinates_end)),
			level => log_threshold
			);
		
		log_indentation_down;
	end write_coordinates_of_segment;

	procedure write_coordinates_of_junction (junction : in type_net_junction) is
	-- Writes the coordinates of a net junction.
		use et_string_processing;
		use et_libraries;
	
		log_threshold : type_log_level := 1;
	begin
		log_indentation_up;
		
		log (to_string (et_libraries.type_coordinates (junction.coordinates)),
			 level => log_threshold
			); 
		
		log_indentation_down;
	end write_coordinates_of_junction;			

	
	function to_component_reference (
	-- Converts a string like "IC303" to a composite type_component_reference.
	-- If allow_special_character_in_prefix is given true, the first character
	-- is allowed to be a special character.
	-- NOTE: Leading zeroes in the id are removed.
		text_in : in string;
		allow_special_character_in_prefix : in boolean := false		
		) return type_component_reference is

		r : type_component_reference := (
				prefix => type_component_prefix.to_bounded_string(""),
				id => 0,
				id_width => 1);
	
		c : character;
		p : type_component_prefix.bounded_string;
	
		procedure invalid_reference is
		begin
			et_string_processing.write_message(
				file_handle => current_output,
				text => latin_1.lf & et_string_processing.message_error & "invalid component reference '" & text_in & "'",
				console => true);
			
			raise constraint_error;
		end invalid_reference;

		d : positive;
		digit : natural := 0;

		use et_libraries.type_component_prefix;
	begin
		-- assemble prefix
		for i in text_in'first .. text_in'last loop
			c := text_in(i);
			
			case i is 
				-- The first character MUST be an upper case letter.
				-- If allow_special_charater_in_prefix then the first letter is
				-- allowed to be a special character. (kicad uses '#' for power symbols)
				when 1 => 
					case allow_special_character_in_prefix is
						when false =>
							if is_upper(c) then
								r.prefix := r.prefix & c;
							else 
								invalid_reference;
							end if;

						when true =>
							if is_upper(c) or is_special(c) then
								r.prefix := r.prefix & c;
							else 
								invalid_reference;
							end if;
					end case;
					
				-- Further characters are appended to prefix if they are upper case letters.
				-- If a upper-case-letter is found, the prefix is assumed as complete.
				-- A lower case letter will later be detetect when assembling the component id.
				when others =>
					if is_upper(c) then
						r.prefix := r.prefix & c;
					else
						--put("   prefix " & type_component_prefix.to_string(r.prefix));
						-- CS: check if allowed prefix
						d := i; -- d holds the position of the charcter after the prefix.
							-- d is requried when reading the component id. see below.
						exit;
					end if;
			end case;
		end loop;

		-- assemble id
		-- Start with the last character in text_in.
		-- Finish at the position d (that is the first digit after the last letter, see above).
		-- All the characters within this range must be digits.
		-- The significance of the digit is increased after each pass.
		for i in reverse d .. text_in'last loop
			c := text_in(i);
			
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
		
-- 		put_line(" id    " & natural'image(r.id));
-- 		put_line(" digits" & natural'image(r.id_width));
		
		return r;
	end to_component_reference;

	function to_string ( reference : in type_component_reference) return string is
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


	function compare_component_by_reference ( left, right : in type_component_reference) return boolean is
	-- Returns true if left comes before right.
	-- If left equals right, the return is false.
		result : boolean := false;
		use et_libraries.type_component_prefix;
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
	end compare_component_by_reference;


	
end et_schematic;
-- Soli Deo Gloria

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
		log ("component " & et_general.to_string (type_components.key(component)) & " properties");

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
		log (et_general.to_string (type_components.element(component).appearance));

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
	

end et_schematic;
-- Soli Deo Gloria

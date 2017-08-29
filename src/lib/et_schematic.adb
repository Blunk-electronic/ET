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
	
	procedure write_label_properties ( label : in type_net_label; indentation : in natural := 0) is
	-- Writes the properties of the given net label in the logfile.
		function indent ( i : in natural) return string renames et_string_processing.indentation;
	begin
		case label.label_appearance is
			when simple =>
				put(indent(indentation) & "simple label ");
			when tag =>
				put(indent(indentation) & "tag label ");
				-- CS: directon, global, hierarchic, style, ...
		end case;

		put_line("'" & type_net_name.to_string(label.text) & "' ");

		put_line(indent(indentation + 1) & et_schematic.to_string(label.coordinates));
		put_line(indent(indentation + 1) & et_libraries.to_string(label.orientation));
		
		case label.label_appearance is
			when simple =>
				null;
			when tag =>
				null;
				--put("tag label ");
				-- CS: directon, global, hierarchic, style, ...
		end case;

	end write_label_properties;

	procedure write_note_properties (note : in et_schematic.type_note; indentation : in natural := 0) is
	-- Writes the properties of the given note
		function indent ( i : in natural) return string renames et_string_processing.indentation;
	begin
		put_line(indent(indentation)	
			& "text note");

		-- position
		put_line(indent(indentation + 1)
			& et_schematic.to_string (note.coordinates));

		-- content
		if et_libraries.type_text_content.length(note.content) > 0 then
			put_line(indent(indentation + 1) 
				& "content '" & et_libraries.type_text_content.to_string(note.content) & "'");
		else
			put_line(indent(indentation + 1)
				& "no content");
		end if;
		
		-- size
		put_line(indent(indentation + 1)
			& "size"
			& et_libraries.type_text_size'image (note.size));

		-- style
		put_line(indent(indentation + 1)
			& "style "
			& to_lower(et_libraries.type_text_style'image (note.style)));

		-- line width
		put_line(indent(indentation + 1)
			& "line width"
			& et_libraries.type_text_line_width'image (note.line_width));

		-- orientation
		put_line(indent(indentation + 1)
			& et_libraries.to_string (note.orientation));

		-- visible
		put_line(indent(indentation + 1)
			& "visible "
			& to_lower(et_libraries.type_text_visible'image (note.visible)));

		-- alignment
		put_line(indent(indentation + 1)
			& "aligment (hor/vert) "
			& to_lower(et_libraries.type_text_alignment_horizontal'image(note.alignment.horizontal))
			& "/"
			& to_lower(et_libraries.type_text_alignment_vertical'image(note.alignment.vertical)));
			
	end write_note_properties;
	
	procedure write_component_properties ( component : in type_components.cursor; indentation : in natural := 0) is
	-- Writes the properties of the component indicated by the given cursor.
		function indent ( i : in natural) return string renames et_string_processing.indentation;
	begin
		-- reference (serves as key in list of components)
		put_line(indent(indentation) 
			& "component " 
			& et_general.to_string (type_components.key(component)));

		-- CS: library file name
		-- name in library
		put_line(indent(indentation + 1)
			& "name in library "
			& et_libraries.to_string (type_components.element(component).name_in_library));
		
		-- value
		put_line(indent(indentation + 1)
			& "value "
			& et_libraries.type_component_value.to_string (type_components.element(component).value));

		-- commissioned
		put_line(indent(indentation + 1)
			& "commissioned "
			& string(type_components.element(component).commissioned));

		-- updated
		put_line(indent(indentation + 1)
			& "updated      "
			& string(type_components.element(component).updated));

		-- author
		put_line(indent(indentation + 1)
			& "author "
			& et_libraries.type_person_name.to_string (type_components.element(component).author));

		
		-- appearance
		put_line(indent(indentation + 1)
			& et_general.to_string (type_components.element(component).appearance));

		-- depending on the component appearance there is more to report:
		case type_components.element(component).appearance is
			when sch_pcb =>

				-- package variant
				put_line(indent(indentation + 1)
					& et_libraries.to_string (type_components.element(component).variant.variant));
				-- NOTE: This displays the type_component_variant (see et_libraries.ads).
				-- Do not confuse with type_variant (see et_schematic.ads) which also contains the variant name
				-- like in TL084D or TL084N.

				-- datasheet
				put_line(indent(indentation + 1)
					& "datasheet "
					& et_libraries.type_component_datasheet.to_string (type_components.element(component).datasheet));

				-- partcode
				put_line(indent(indentation + 1)
					& "partcode "
					& et_libraries.type_component_partcode.to_string (type_components.element(component).partcode));
				
				-- function
				put_line(indent(indentation + 1)
					& "purpose "
					& et_libraries.type_component_purpose.to_string (type_components.element(component).purpose));
				
			when pcb => null; -- CS
			when others => null; -- CS should never happen as virtual components do not have a package
		end case;
	end write_component_properties;

	procedure write_unit_properties ( unit : in type_units.cursor; indentation : in natural := 0 ) is
	-- Writes the properties of the unit indicated by the given cursor.
		function indent ( i : in natural) return string renames et_string_processing.indentation;
	begin
		-- unit name
		put_line(indent(indentation) 
			& "with unit " 
			& et_libraries.type_unit_name.to_string (type_units.key(unit)));

		-- alternative representation
		put_line(indent(indentation + 1) 
			& "alternative (deMorgan) representation " 
			& to_lower (et_schematic.type_alternative_representation'image (type_units.element(unit).alt_repres)));

		-- timestamp
		put_line(indent(indentation + 1) 
			& "timestamp " 
			& string (type_units.element (unit).timestamp));

		-- position
		put_line(indent(indentation + 1) 
			& et_schematic.to_string (type_units.element(unit).position));

		-- placeholders
		put_line(indent(indentation + 1) 
			& "placeholders");

			-- reference
			et_libraries.write_placeholder_properties (
				placeholder => type_units.element(unit).reference,
				indentation => indentation + 2);

			-- value
			et_libraries.write_placeholder_properties (
				placeholder => type_units.element(unit).value,
				indentation => indentation + 2);

			-- some placeholders exist depending on the component appearance
			case type_units.element(unit).appearance is
				when sch_pcb =>
					
					-- package/footprint
					et_libraries.write_placeholder_properties (
						placeholder => type_units.element(unit).packge,
						indentation => indentation + 2);

					-- datasheet
					et_libraries.write_placeholder_properties (
						placeholder => type_units.element(unit).datasheet,
						indentation => indentation + 2);

					-- purpose
					et_libraries.write_placeholder_properties (
						placeholder => type_units.element(unit).purpose,
						indentation => indentation + 2);
					
					-- partcode
					et_libraries.write_placeholder_properties (
						placeholder => type_units.element(unit).partcode,
						indentation => indentation + 2);

				when others => null;
			end case;

			-- commissioned
			et_libraries.write_placeholder_properties (
				placeholder => type_units.element(unit).commissioned,
				indentation => indentation + 2);

			-- updated
			et_libraries.write_placeholder_properties (
				placeholder => type_units.element(unit).updated,
				indentation => indentation + 2);

			-- author
			et_libraries.write_placeholder_properties (
				placeholder => type_units.element(unit).author,
				indentation => indentation + 2);
			
	end write_unit_properties;

	procedure write_coordinates_of_segment (segment : in type_net_segment; indentation : in natural := 0 ) is
	-- Writes the start and end coordinates of a net segment.
		function indent ( i : in natural) return string renames et_string_processing.indentation;
	begin
		put_line(indent(indentation) 
			& "start "
			& to_string (segment.coordinates_start) 
			& " end " 
			& to_string (segment.coordinates_end));
	end write_coordinates_of_segment;

	procedure write_coordinates_of_junction (junction : in type_net_junction; indentation : in natural := 0) is
	-- Writes the coordinates of a net junction.
		function indent ( i : in natural) return string renames et_string_processing.indentation;
	begin
		put_line(indent(indentation) 
			& et_schematic.to_string (junction.coordinates)); 
	end write_coordinates_of_junction;			
	

end et_schematic;
-- Soli Deo Gloria

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

	
	function to_string (orientation : in type_orientation) return string is
	-- Returns the the given orientation as string.
	begin
		return("orientation " & et_general.type_orientation'image(orientation));
		-- CS: exception handler
	end to_string;

	
	procedure write_component_properties ( component : in type_components.cursor; indentation : in natural := 0) is
	-- Writes the properties of the component indicated by the given cursor.
		function indent ( i : in natural) return string renames et_string_processing.indentation;
	begin
		-- reference
		put_line(indent(indentation) 
			& "component " 
			& et_general.to_string (type_components.key(component)));

		-- value
		put_line(indent(indentation + 1)
			& "value "
			& et_schematic.type_component_value.to_string (type_components.element(component).value));
		
		-- CS: library file name
		-- name in library
		put_line(indent(indentation + 1)
					& et_libraries.to_string (type_components.element(component).name_in_library));

		
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

				-- partcode
				put_line(indent(indentation + 1)
					& "partcode "
					& et_libraries.type_component_partcode.to_string (type_components.element(component).partcode));

				
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
			& et_schematic.type_alternative_representation'image (type_units.element(unit).alt_repres));

		-- timestamp
		put_line(indent(indentation + 1) 
			& "timestamp " 
			& string(type_units.element (unit).timestamp));

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

			-- package/footprint
			et_libraries.write_placeholder_properties (
				placeholder => type_units.element(unit).packge,
				indentation => indentation + 2);

			-- datasheet
			et_libraries.write_placeholder_properties (
				placeholder => type_units.element(unit).datasheet,
				indentation => indentation + 2);

			-- function
			et_libraries.write_placeholder_properties (
				placeholder => type_units.element(unit).fnction,
				indentation => indentation + 2);
			
			-- partcode
			et_libraries.write_placeholder_properties (
				placeholder => type_units.element(unit).partcode,
				indentation => indentation + 2);

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

	

end et_schematic;
-- Soli Deo Gloria

------------------------------------------------------------------------------
--                                                                          --
--                    SYSTEM ET NETLIST DECLARATIONS                        --
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

with ada.containers;            use ada.containers;
with ada.containers.indefinite_ordered_maps;


with ada.directories;

with et_general;
with et_libraries;
with et_schematic;
with et_string_processing;		use et_string_processing;
with et_geometry;
with et_import;


package body et_netlist is


	procedure dummy is begin null; end dummy;

	function build_portlists return type_portlists.map is
		pl : type_portlists.map;

		use et_libraries;
		use et_schematic;
		use et_schematic.type_components;
		
		component_cursor	: et_schematic.type_components.cursor;
		library_cursor		: type_library_names.cursor;
	begin
		
		log (text => "component list");
		log_indentation_up;
		
		reset_component_cursor (component_cursor);
		while component_cursor /= et_schematic.type_components.no_element loop

			--write_component_properties (component_cursor);
			log (text => to_string (component_reference (component_cursor)));
			log (text => "name in library " & et_libraries.type_component_name.to_string (component_name_in_library (component_cursor)));

			-- search libraries "name in library"
			reset_library_cursor (library_cursor);



			
			next (component_cursor);
		end loop;

		log_indentation_down;
		
		return pl;
	end build_portlists;
	
end et_netlist;
-- Soli Deo Gloria

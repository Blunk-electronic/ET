------------------------------------------------------------------------------
--                                                                          --
--                           SYSTEM ET KICAD LIBRARIES                      --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               S p e c                                    --
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
with ada.text_io;				use ada.text_io;

with ada.strings.bounded; 		use ada.strings.bounded;
with ada.containers; 			use ada.containers;
--with ada.containers.vectors;
-- with ada.containers.doubly_linked_lists;
with ada.containers.ordered_maps;

with et_general;				use et_general;
with et_schematic;				use et_schematic;
with et_import;		            use et_import;

package et_kicad_libraries is

-- SCHEMATIC RELATED

	-- The kicad schematic editor refers to schematic elements as "components"
	-- (EAGLE refers to them as "symbols").
	-- Components are composites of lines, arcs, circles, texts, ports:
	type type_component is record
-- 		outline_segments_lines	: type_list_of_device_block_outline_segments_lines.list;
-- 		outline_segments_arcs 	: type_list_of_device_block_outline_segments_arcs.list;
-- 		outline_segments_circles: type_list_of_device_block_outline_segments_circles.list;
		port_list 				: type_list_of_device_block_ports.map;
--         text_list				: type_list_of_device_block_texts.list;
	end record;

	-- Components have a name and are stored in an ordered map.
	-- Within the map they are accessed by a key type_component_name (something like "CAPACITOR").
	component_name_length_max : constant positive := 100; -- CS: we restrict the part name to 100 characters
	package type_component_name is new generic_bounded_length(component_name_length_max); use type_component_name;
	package type_list_of_components is new ordered_maps (
		key_type => type_component_name.bounded_string,
		element_type => type_component);

	procedure a;
	
end et_kicad_libraries;


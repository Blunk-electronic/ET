------------------------------------------------------------------------------
--                                                                          --
--                          SYSTEM ET NETLIST                               --
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
--   ToDo: 

with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded;       use ada.strings.bounded;
with ada.strings.unbounded; 	use ada.strings.unbounded;
with ada.containers;            use ada.containers;
-- with ada.containers.vectors;
with ada.containers.doubly_linked_lists;
-- with ada.containers.indefinite_doubly_linked_lists;
with ada.containers.ordered_maps;
-- with ada.containers.indefinite_ordered_maps;
-- with ada.containers.ordered_sets;

with et_general;
with et_libraries;
with et_schematic;
with et_string_processing;

package et_netlist is

 	type type_port is new et_schematic.type_port_base with null record;

	-- Ports of a component are collected in a simple list.
	package type_ports is new doubly_linked_lists ( 
		element_type => type_port); 
	use type_ports;

	-- The components with their ports are collected in a map with the component reference as key:
	package type_portlists is new ordered_maps (
		key_type => et_libraries.type_component_reference,
		element_type => type_ports.list,
		"<" => et_schematic.compare_reference);

	function build_portlists return type_portlists.map;
	-- Returns a list of components with the absolute positions of their ports.
	
	portlists : type_portlists.map; -- CS: temporily used, remove !
	
	
end et_netlist;

-- Soli Deo Gloria

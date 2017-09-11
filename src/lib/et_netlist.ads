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
with ada.containers.vectors;
with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_doubly_linked_lists;
with ada.containers.ordered_maps;
with ada.containers.indefinite_ordered_maps;
with ada.containers.ordered_sets;

with et_general;
with et_libraries;				use et_libraries;
with et_string_processing;

package et_netlist is

	procedure dummy;
-- 	type type_port is record
		
	
-- 	package type_component_portlists is new ordered_maps (
-- 		key_type => type_component_reference,
-- 		element_type => et_libraries.type_ports

	

-- 	-- This is the port of a component within the schematic.
-- 	type type_port_of_component is record
-- 		pin			: et_libraries.type_pin_name.bounded_string; -- example: "144" or in case of a BGA package "E14"
-- 		position	: type_coordinates; -- full set of coordinates (module, sheet, x,y, ...)
-- 
-- 		-- for ERC we need electrical information
-- 		direction	: et_libraries.type_port_direction; -- passive, in, out, ...
-- 		-- CS: style ?
-- 	end record;
-- 
-- 	-- Ports of a component are collected in a map. The key into the map is the port name.
-- 	package type_ports_of_component is new ordered_maps ( 
-- 		key_type => et_libraries.type_port_name.bounded_string, -- like "CLOCK" or "CE"
-- 		element_type => type_port_of_component,
-- 		"<" => et_libraries.type_port_name."<"); 
-- 



	
	
end et_netlist;

-- Soli Deo Gloria

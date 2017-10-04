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

-- with ada.strings.maps;			use ada.strings.maps;
-- with ada.strings.bounded;       use ada.strings.bounded;
-- with ada.strings.unbounded; 	use ada.strings.unbounded;
with ada.containers;            use ada.containers;
-- with ada.containers.vectors;
-- with ada.containers.doubly_linked_lists;
-- with ada.containers.indefinite_doubly_linked_lists;
with ada.containers.ordered_maps;
-- with ada.containers.indefinite_ordered_maps;
-- with ada.containers.ordered_sets;

with et_coordinates;
with et_libraries;
with et_schematic;
with et_string_processing;

package et_netlist is

	use et_schematic.type_net_name;
	use et_schematic.type_ports;

	-- This is the netlist of a single submodule:
	package type_netlist is new ordered_maps (
		key_type => et_schematic.type_net_name.bounded_string, -- net name like "MCU_CLOCK"
		element_type => et_schematic.type_ports.set); -- the list of ports connected with the net

	use type_netlist;
	use et_coordinates.type_submodule_name;
	
	-- The rig netlist is a map of netlists. For each submodule we have a
	-- distinctive netlist. The key to the particular netlist is the submodule name.
	package type_rig_netlists is new ordered_maps (
		key_type => et_coordinates.type_submodule_name.bounded_string, -- example "MOTOR_DRIVER"
		element_type => type_netlist.map); -- the netlist of the submodule

	-- This is the place where we store the netlists of the rig.
	rig_netlists : type_rig_netlists.map;

	
	procedure make_netlist;
	
end et_netlist;

-- Soli Deo Gloria

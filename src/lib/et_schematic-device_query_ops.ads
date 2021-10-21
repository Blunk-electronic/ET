------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                   SCHEMATIC DEVICE QUERY OPERATIONS                      --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2021 Mario Blunk, Blunk electronic          --
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

--   For correct displaying set tab width in your editor to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
--   ToDo: 
--		1. Objects like net segments, net labels, notes ... 
--		   should be collected in ordered sets instead of doubly_linked_lists
--			- the benefits: placing identical objects at the same position would be impossible
--			- the cons: ordering subprograms required
--		3. device accessories

with ada.text_io;				use ada.text_io;
with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded;       use ada.strings.bounded;
with ada.containers;            use ada.containers;
with ada.containers.vectors;
with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_doubly_linked_lists;
with ada.containers.ordered_maps;
with ada.containers.indefinite_ordered_maps;
with ada.containers.ordered_sets;

with et_general;				use et_general;
with et_nets;					use et_nets;
with et_coordinates;			use et_coordinates;
with et_assembly_variants;		use et_assembly_variants;

with et_terminals;
with et_packages;				use et_packages;
with et_pcb;
with et_pcb_coordinates;
--with et_submodules;
--with et_numbering;
--with et_material;
--with et_netlists;
with et_geometry;
--with et_text;
with et_symbols;				use et_symbols;
with et_devices;				use et_devices;


package et_schematic.device_query_ops is

		
	function get_port (
		device		: in pac_devices_sch.cursor;
		terminal	: in et_terminals.pac_terminals.cursor)
		return pac_port_name.bounded_string;


	
	-- Returns a cursor to the net that is connected with the given device and terminal.
	-- If there is no net connected, then the return is no_element:
	function get_net (
		device		: in pac_devices_sch.cursor;
		terminal	: in et_terminals.pac_terminals.cursor)
		return pac_nets.cursor;

	
			
end et_schematic.device_query_ops;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

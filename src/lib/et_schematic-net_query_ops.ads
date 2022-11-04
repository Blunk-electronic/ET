------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                    SCHEMATIC NET QUERY OPERATIONS                        --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2022 Mario Blunk, Blunk electronic          --
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

-- NOTE: THIS PACKAGE IS CURRENTLY NOT USED. JUST EXPERIMENTAL STUFF.

with et_submodules;

package et_schematic.net_query_ops is

	type type_device_port is record
		device	: pac_devices_sch.cursor;
		unit	: pac_units.cursor;
		port	: pac_ports.cursor;
	end record;

	package pac_device_ports is new doubly_linked_lists (type_device_port);
	use pac_device_ports;
	
	
	type type_submodule_port is record
		submodule	: et_submodules.pac_submodules.cursor;
		port		: pac_nets.cursor;
	end record;

	package pac_submodule_ports is new doubly_linked_lists (type_submodule_port);
	use pac_submodule_ports;
	
	
	type type_netchanger_port is record
		netchanger	: et_submodules.pac_netchangers.cursor;
		port		: et_submodules.type_netchanger_port_name;
	end record;

	package pac_netchanger_ports is new doubly_linked_lists (type_netchanger_port);
	use pac_netchanger_ports;
	
	
	type type_ports is record
		devices		: pac_device_ports.list;
		submodules	: pac_submodule_ports.list;
		netchangers	: pac_netchanger_ports.list;
	end record;


	-- Returns the ports of devices, submodules and netchangers in
	-- the given net. The given assembly variant determines whether certain
	-- devices should be excluded (because they may not be present in a particular
	-- assembly variant).
	-- NOTE: If variant points to no element, then the default variant is assumend
	-- and ALL devices are returned:
	function get_ports (
		net		: in pac_nets.cursor;
		variant	: in pac_assembly_variants.cursor)
		return type_ports;

	
		
end et_schematic.net_query_ops;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

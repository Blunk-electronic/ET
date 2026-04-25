------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                             NETLISTS                                     --
--                                                                          --
--                              S p e c                                     --
--                                                                          --
-- Copyright (C) 2017 - 2026                                                --
-- Mario Blunk / Blunk electronic                                           --
-- Buchfinkenweg 3 / 99097 Erfurt / Germany                                 --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
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
--   To Do: 
--
--


with ada.containers;            use ada.containers;
with ada.containers.ordered_maps;

with et_net_names;				use et_net_names;
with et_net_ports_netchangers;	use et_net_ports_netchangers;

with et_netlist_devices;		use et_netlist_devices;
with et_netlist_submodules;		use et_netlist_submodules;


package et_netlist_cat_1 is


	
	type type_net_ports_cat_1 is record
		devices		: pac_device_ports_extended.set;
		submodules	: pac_submodule_ports_extended.set;
		netchangers	: pac_netchanger_ports.set;
		-- CS ? scope		: type_net_scope;
	end record;

	

	use pac_net_name;
	
	package pac_netlist_cat_1 is new ordered_maps (
		key_type		=> pac_net_name.bounded_string, 
		element_type	=> type_net_ports_cat_1);


	-- use pac_netlist_cat_1;


	-- Adds a net to the given netlist:
	procedure add_net_to_netlist (
		netlist		: in out pac_netlist_cat_1.map;
		name		: in pac_net_name.bounded_string;
		devices		: in pac_device_ports_extended.set;
		submodules	: in pac_submodule_ports_extended.set;
		netchangers	: in pac_netchanger_ports.set);
	
	
end et_netlist_cat_1;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

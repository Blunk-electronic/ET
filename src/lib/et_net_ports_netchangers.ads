------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                     NET SEGMENT PORTS / NETCHANGERS                      --
--                                                                          --
--                               S p e c                                    --
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
--
-- DESCRIPTION:
--
-- This package is about ports of submodules
-- as they are connected with net segments.
-- This information is part of a net segment.
--
--
--   history of changes:
--


with ada.containers; 			use ada.containers;
with ada.containers.ordered_sets;

with et_netchangers;			use et_netchangers;
with et_netchangers.schematic;	use et_netchangers.schematic;
-- with et_module_instance;		use et_module_instance;
-- with et_net_names;				use et_net_names;
-- with et_string_processing;		use et_string_processing;


package et_net_ports_netchangers is
	
	

	-- This is the port of a netchanger as it appears in a net segment:
	type type_port_netchanger is record
		index	: type_netchanger_id := type_netchanger_id'first;
		port	: type_netchanger_port_name := SLAVE; -- CS reasonable default ?
	end record;

	function "<" (left, right : in type_port_netchanger) return boolean;	


	package pac_netchanger_ports is new ordered_sets (type_port_netchanger);

	
	-- Returns true if the given list contains
	-- a netchanger port with the given index and port:
	function contains_netchanger_port (
		ports	: in pac_netchanger_ports.set;
		index	: in type_netchanger_id;
		port	: in type_netchanger_port_name)
		return boolean;


		
end et_net_ports_netchangers;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

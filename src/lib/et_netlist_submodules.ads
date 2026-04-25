------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                         NETLIST / SUBMODULES                             --
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
--
--
-- To Do: 
-- - implement characteristics of submodule port


with ada.containers;            use ada.containers;
with ada.containers.ordered_sets;

with et_net_names;				use et_net_names;
with et_module_instance;		use et_module_instance;


package et_netlist_submodules is


	-- In a netlist, a submodule that is connected with
	-- a certain net is modelled by this type:
	type type_submodule_port_extended is record
		-- The name of the submodule instance:
		submodule	: pac_module_instance_name.bounded_string; -- MOT_DRV_3
		
		-- The port of the submodule instance 
		-- is named after a net that is exported by
		-- the submodule:
		port		: pac_net_name.bounded_string; -- CLOCK_GENERATOR_OUT
		
		-- CS ? direction	: type_netchanger_port_name; -- master/slave
		-- CS ? characteristics. See et_netlist_devices.type_device_port_extended
	end record;

	
	function "<" (
		left, right : in type_submodule_port_extended) 
		return boolean;

	
	package pac_submodule_ports_extended is new ordered_sets (
		element_type	=> type_submodule_port_extended);
	
	
	
end et_netlist_submodules;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

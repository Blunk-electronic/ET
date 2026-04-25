------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                     NET SEGMENT PORTS / SUBMODULES                       --
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

with et_module_instance;		use et_module_instance;
with et_net_names;				use et_net_names;
with et_string_processing;		use et_string_processing;


package et_net_ports_submodules is
	
	
	
	-- This is the port of a submodule:
	type type_net_submodule_port is record
		-- The instance of a certain submodule:
		module_name	: pac_module_instance_name.bounded_string; -- MOT_DRV_3
		-- CS rename to submodule ?

		-- The net of the submodule is here the port name:
		port_name	: pac_net_name.bounded_string; -- CLOCK_GENERATOR_OUT
		-- CS rename to port ?
	end record;

	
	
	function "<" (
		left, right : in type_net_submodule_port) 
		return boolean;

	
	-- Many submodule ports are stored in ordered sets:
	package pac_net_submodule_ports is new 
		ordered_sets (type_net_submodule_port);

	use pac_net_submodule_ports;


		
end et_net_ports_submodules;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           NET SEGMENT PORTS                              --
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
-- This package is about ports of units, netchangers and submodules
-- as they are connected with net segments.
-- This information is part of a net segment.
--
--
--   history of changes:
--




with et_net_ports_devices;		use et_net_ports_devices;
with et_net_ports_submodules;	use et_net_ports_submodules;
with et_net_ports_netchangers;	use et_net_ports_netchangers;


package et_net_ports is
			
		
		
-- AGGREGATION OF DEVICE, SUBMODULE AND NETCHANGER PORTS:

	type type_net_ports is record
		devices		: pac_device_ports.set;
		submodules	: pac_net_submodule_ports.set;
		netchangers	: pac_netchanger_ports.set;
	end record;



	-- Merges the given two port groups to a
	-- single one:
	function merge_ports (
		right, left : in type_net_ports)
		return type_net_ports;


	-- Merges the given source ports in the target ports:
	procedure merge_ports (
		target	: in out type_net_ports;
		source	: in type_net_ports);					  
	

	-- Returns true if the given netchanger port
	-- is among the given ports:
	function in_ports (
		ports	: in type_net_ports;
		port	: in type_port_netchanger)
		return boolean;
	

	-- Returns true if the given submodule port
	-- is among the given ports:
	function in_ports (
		ports	: in type_net_ports;
		port	: in type_net_submodule_port)
		return boolean;

	
	
	-- Returns true if the given record of ports is completely emtpty.
	function no_ports (
		ports : in type_net_ports) 
		return boolean;


	-- Returns the total number of ports contained
	-- in the given port group:
	function get_port_count ( -- CS rename to get_port_count_total
		ports : in type_net_ports)
		return natural;


	function get_port_count_devices (
		ports : in type_net_ports)
		return natural;


	function get_port_count_submodules (
		ports : in type_net_ports)
		return natural;

	
	function get_port_count_netchangers (
		ports : in type_net_ports)
		return natural;

	
	
	-- These are the ports which may exist
	-- at the A or B end of a net segment.
	-- This type models the tag labels of a net segment:
	type type_net_ports_AB is record
		A, B : type_net_ports;
	end record;
	

	
	
end et_net_ports;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

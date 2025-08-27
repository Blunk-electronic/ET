------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              NET PORTS                                   --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2025                                                --
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

--   For correct displaying set tab width in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--


with ada.containers; 			use ada.containers;
with ada.containers.ordered_sets;

with et_module_instance;		use et_module_instance;
with et_port_names;				use et_port_names;
with et_symbol_ports;			use et_symbol_ports;
with et_device_name;			use et_device_name;
with et_unit_name;				use et_unit_name;
with et_net_names;				use et_net_names;
with et_netlists;


package et_net_ports is


	
	-- This is the port of a device as it appears in a net segment:
	type type_device_port is record
		device_name	: type_device_name; -- IC4
		unit_name	: pac_unit_name.bounded_string; -- A
		port_name	: pac_port_name.bounded_string; -- IN1
	end record;


	function "<" (left, right : in type_device_port) return boolean;


	-- Builds a device port:
	function to_device_port (
		device	: in type_device_name;
		unit	: in pac_unit_name.bounded_string;
		port	: in pac_port_name.bounded_string)
		return type_device_port;

	
	package pac_device_ports is new ordered_sets (type_device_port);


	function to_string (port : in type_device_port) return string;


	
	-- Iterates the device ports. 
	-- Aborts the process when the proceed-flag goes false:
	procedure iterate (
		ports	: in pac_device_ports.set;
		process	: not null access procedure (position : in pac_device_ports.cursor);
		proceed	: not null access boolean);


	
	-- This is the port of a submodule:
	type type_submodule_port is record
		-- The instance of a certain submodule:
		module_name	: pac_module_instance_name.bounded_string; -- MOT_DRV_3

		-- The net of the submodule is here the port name:
		port_name	: pac_net_name.bounded_string; -- CLOCK_GENERATOR_OUT
	end record;

	
	function "<" (left, right : in type_submodule_port) return boolean;

	
	package pac_submodule_ports is new ordered_sets (type_submodule_port);


	
	type type_ports is record -- CS rename to type_port_group ?
		devices		: pac_device_ports.set;
		submodules	: pac_submodule_ports.set;
		netchangers	: et_netlists.pac_netchanger_ports.set;
	end record;



	-- Merges the given two port groups to a
	-- single one:
	function merge_ports (
		right, left : in type_ports)
		return type_ports;


	-- Merges the given source ports in the target ports:
	procedure merge_ports (
		target	: in out type_ports;
		source	: in type_ports);					  
	

	-- Returns true if the given netchanger port
	-- is among the given ports:
	function in_ports (
		ports	: in type_ports;
		port	: in et_netlists.type_port_netchanger)
		return boolean;
	

	-- Returns true if the given submodule port
	-- is among the given ports:
	function in_ports (
		ports	: in type_ports;
		port	: in type_submodule_port)
		return boolean;

	
	
	-- Returns true if the given record of ports is completely emtpty.
	function no_ports (
		ports : in type_ports) 
		return boolean;


	-- Returns the total number of ports contained
	-- in the given port group:
	function get_port_count (
		ports : in type_ports)
		return natural;
	

	-- These are the ports which may exist
	-- at the A or B end of a net segment.
	-- This type models the tag labels of a net segment:
	type type_ports_AB is record
		A, B : type_ports;
	end record;
	

	
	
end et_net_ports;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

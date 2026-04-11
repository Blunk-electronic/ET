------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                       NET SEGMENT PORTS / DEVICES                        --
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
-- This package is about devices, units and their ports
-- as they are connected with net segments.
-- This information is part of a net segment.
--
--
--   history of changes:
--


with ada.containers; 			use ada.containers;
with ada.containers.ordered_sets;

with et_port_names;				use et_port_names;
with et_symbol_ports;			use et_symbol_ports;
with et_device_name;			use et_device_name;
with et_unit_name;				use et_unit_name;
with et_string_processing;		use et_string_processing;



package et_net_ports_devices is
	

-- DEVICE:
	
	-- This is the port of a unit as it is connected 
	-- with a net segment:
	type type_device_port is record -- CS rename to type_net_unit_port
		device_name	: type_device_name; -- IC4
		-- CS cursor to the electrical device instead ?
		-- could improve performance.
		
		unit_name	: pac_unit_name.bounded_string; -- A
		-- CS cursor to the unit instead ?
		-- could improve performance.
		
		port_name	: pac_port_name.bounded_string; -- IN1
		-- CS cursor to the port instead ?
		-- could improve performance.
	end record;


	
	function "<" (
		left, right : in type_device_port) 
		return boolean;

		

	-- Builds a device port:
	function to_device_port (
		device	: in type_device_name;
		unit	: in pac_unit_name.bounded_string;
		port	: in pac_port_name.bounded_string)
		return type_device_port;


	
	-- Converts a string like "device IC1 unit C port I1"
	-- to a device port.
	-- If something is wrong, then the error-flag is set:
	procedure make_device_port (
		arguments	: in type_fields_of_line;
		error		: out boolean;
		port		: out type_device_port);

	
	
	-- Many device ports are stored in ordered sets:
	package pac_device_ports is new 
		ordered_sets (type_device_port);
		
	use pac_device_ports;

	

	-- Returns something like "device IC1 unit A port PD4":
	function to_string (port : in type_device_port) return string;

	-- Returns something like "device IC1 unit A port PD4":
	function to_string (port : in pac_device_ports.cursor) return string;


	-- Renames the device names in the given
	-- list of device ports:
	procedure rename_device_ports (
		ports		: in out pac_device_ports.set;
		device_old	: in type_device_name;
		device_new	: in type_device_name);

	
	
	-- Iterates the device ports. 
	-- Aborts the process when the proceed-flag goes false:
	procedure iterate (
		ports	: in pac_device_ports.set;
		process	: not null access procedure (position : in pac_device_ports.cursor);
		proceed	: not null access boolean);

	
end et_net_ports_devices;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

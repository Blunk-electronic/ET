------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         MODULE READ / NETS                               --
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
--                                                                          --
------------------------------------------------------------------------------

--   For correct displaying set tab with in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
-- ToDo:
-- - clean up
--
--
--

with et_generic_module;			use et_generic_module;
with et_route;
with et_string_processing;		use et_string_processing;
with et_logging;				use et_logging;



package et_module_read_nets is


	route : et_route.type_net_route; -- scratch, CS: rework required

	

-- NETS:

	-- Reads a line that describes a net like "name AGND", "class default"
	-- or "scope  local":
	procedure read_net (
		line			: in type_fields_of_line;
		log_threshold	: in type_log_level);


	-- Assigns the net to the module:
	procedure assign_net (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level);





	
-- STRANDS:
	
	
	-- Reads a line that describes the position of a strand:
	procedure read_strand (
		line			: in type_fields_of_line;
		log_threshold	: in type_log_level);


	-- Assigns the strand to the list of strands:
	procedure assign_net_strand (
		line			: in type_fields_of_line;
		log_threshold	: in type_log_level);
		
	
	-- insert strand collection in net
	procedure insert_strands;
	
	

	
	
	
-- SEGMENTS:
	
	procedure read_net_segment (
		line			: in type_fields_of_line;
		log_threshold	: in type_log_level);
	

	-- Appends the net_segment to the list of net segments:
	procedure assign_net_segment (
		log_threshold	: in type_log_level);
	
	
	-- insert segments in strand
	procedure insert_net_segments;
	
	

	
	
	

-- JUNCTIONS:
	
	-- Reads a line that describes a net junction
	-- (like "A/B") that is active on an end
	-- of a net segment. Once the line is read, the junction
	-- is activated in variable net_junctions (see above):
	procedure read_net_junction (
		line			: in type_fields_of_line;
		log_threshold	: in type_log_level);
		
		
		
	-- Assigns the net_junctions to the net:
	procedure assign_net_junctions (
		log_threshold	: in type_log_level);
	
	
	

	
	
	
-- CONNECTORS:
	
	-- Reads a line that describes a net connector 
	-- (like "A/B direction input/output") that is attached to an end
	-- of a net segment. Once the line is read, the connector
	-- is assigned to the variable net_connectors (see above):
	procedure read_net_connector (
		line			: in type_fields_of_line;
		log_threshold	: in type_log_level);

		
		
	-- Assigns the net_connectors to the net_segment:
	procedure assign_net_connectors (
		log_threshold	: in type_log_level);

	
	
	
	
-- LABELS:
	
	-- Reads a line that describes a net label:
	procedure read_label (
		line			: in type_fields_of_line;
		log_threshold	: in type_log_level);
		
		
	-- insert label in label collection
	procedure insert_net_label;
		
		
	-- Assigns the net_labels to the net segment:
	procedure assign_net_labels (
		log_threshold	: in type_log_level);
		
		

		
		
-- PORTS:

	-- Reads a port of a device, submodule or netchanger
	-- that is connected with a net segment and sets the variables
	-- net_device_port, net_submodule_port, net_netchanger_port.
	-- Appends the port to net_segment_ports.
	-- NOTE: A device, submodule or netchanger port is defined by a
	-- single line.
	-- Upon reading the line like "A/B device/submodule/netchanger x port 1" 
	-- the port is appended to the corresponding port collection 
	-- net_segment_ports immediately after the line has been read:
	procedure read_net_port (
		line			: in type_fields_of_line;
		log_threshold	: in type_log_level);

	
	-- Assigns net_segment_ports to the net segment:
	procedure assign_net_ports (
		log_threshold	: in type_log_level);

	
	
	
	
-- ROUTE:

	-- Insert route in net:
	procedure assign_route;
		
		
		
end et_module_read_nets;

	


	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

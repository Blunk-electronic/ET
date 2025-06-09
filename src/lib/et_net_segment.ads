------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                             NET SEGMENT                                  --
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
with ada.containers.doubly_linked_lists;
with ada.containers.ordered_sets;

with et_module_instance;		use et_module_instance;
with et_schematic_coordinates;	use et_schematic_coordinates;
with et_port_names;				use et_port_names;
with et_symbol_ports;			use et_symbol_ports;
with et_device_name;			use et_device_name;
with et_unit_name;				use et_unit_name;
with et_logging;				use et_logging;
with et_net_names;				use et_net_names;
with et_net_labels;				use et_net_labels;
with et_netlists;


package et_net_segment is

	use pac_geometry_2;




-- PORTS:
	
	
	-- This is the port of a device as it appears in a net segment:
	type type_device_port is record
		device_name	: type_device_name; -- IC4
		unit_name	: pac_unit_name.bounded_string; -- A
		port_name	: pac_port_name.bounded_string; -- IN1
	end record;

	function "<" (left, right : in type_device_port) return boolean;
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


	
	type type_ports is record
		devices		: pac_device_ports.set;
		submodules	: pac_submodule_ports.set;
		netchangers	: et_netlists.pac_netchanger_ports.set;
	end record;


	
	-- Returns true if the given record of ports is completely emtpty.
	function no_ports (
		ports : in type_ports) 
		return boolean;


	

-- JUNCTIONS:
	
	procedure junction_in_sloping_segment (
		point : in type_object_position);
	
	
	-- A net junction is where segments and ports meet each other.	
	type type_junctions is record
		A	: boolean := false;
		B	: boolean := false;
	end record;

	
	-- GUI relevant only: In the schematic editor, the junction is drawn as follows:
	junction_radius : constant type_distance_positive := 0.5;
	

	
	junction_symbol : type_circle := type_circle (to_circle (
			center	=> origin,
			radius 	=> junction_radius));





-- SEGMENTS:
	
	type type_net_segment is new type_line with record		
		labels		: pac_net_labels.list;
		tag_labels	: type_tag_labels;
		junctions	: type_junctions;
		ports		: type_ports;
	end record;


	-- Creates a bare net segment without labels, 
	-- junctions and ports:
	function to_net_segment (
		A, B : in type_vector_model)
		return type_net_segment;


	-- Reset status flags of segment, junctions and labels:
	overriding procedure reset_status (
		segment : in out type_net_segment);
	
	
	-- Moves the net labels of a segment.
	-- CS: Currently moves only the tag labels
	-- procedure move_net_labels (
	-- 	segment_before	: in type_net_segment;		-- the segment before the move
	-- 	segment_after	: in out type_net_segment;	-- the segment after the move
	-- 	zone			: in type_line_zone);		-- the zone being moved


	


	
-- CONNECT STATUS OF TWO SEGMENTS:

	type type_connect_status is (CON_STS_A, CON_STS_B, CON_STS_NONE);

	-- Tests whether the given primary segment is 
	-- connected with the given secondary segment.
	-- Starting at the primary segment and its end (A/B)
	-- the ends of the secondary segment are tested:
	function get_connect_status (
		primary 	: in type_net_segment;
		AB_end		: in type_start_end_point;
		secondary	: in type_net_segment)
		return type_connect_status;





	
	
	
	package pac_net_segments is new doubly_linked_lists (type_net_segment);
	use pac_net_segments;
	

	function is_selected (
		segment : in pac_net_segments.cursor)
		return boolean;

	
	
	function get_A (
		segment : in pac_net_segments.cursor)
		return type_vector_model;


	function get_B (
		segment : in pac_net_segments.cursor)
		return type_vector_model;


	function is_moving (
		segment : in pac_net_segments.cursor)
		return boolean;

	
	-- Returns true if the start point of the 
	-- given net segment is set as "moving":
	function is_A_moving (
		segment	: in pac_net_segments.cursor)
		return boolean;


	-- Returns true if the end point of the 
	-- given net segment is set as "moving":
	function is_B_moving (
		segment	: in pac_net_segments.cursor)
		return boolean;

	
	
	-- Iterates the net segments. 
	-- Aborts the process when the proceed-flag goes false:
	procedure iterate (
		segments	: in pac_net_segments.list;
		process		: not null access procedure (position : in pac_net_segments.cursor);
		proceed		: not null access boolean);


	
	-- Returns a string that tells about start 
	-- and end coordinates of the net segment.
	function to_string (
		segment : in pac_net_segments.cursor) 
		return string;
		


	
	-- Returns true if given point sits between 
	-- start and end point (A/B) of the given segment.
	-- The catch zone is a means of reducing the accuracy. 
	-- The greater the zone
	-- the greater can be the distance to the segment:
	function between_A_and_B (
		catch_zone	: in type_catch_zone;
		segment		: in pac_net_segments.cursor)
		return boolean;


		
	
	-- Returns the orientation of a net segment.
	function get_segment_orientation (
		segment : in pac_net_segments.cursor) 
		return type_line_orientation;



	-- Returns true if given center of a zone in on the given segment.
	-- The catch zone is a means of reducing the accuracy. The greater the zone
	-- the greater can be the distance to the segment:
	function on_segment (
		catch_zone	: in type_catch_zone;
		segment 	: in pac_net_segments.cursor)
		return boolean;


	-- Returns true if the given point sits on 
	-- the given segment:
	function on_segment (
		point		: in type_vector_model;
		segment 	: in pac_net_segments.cursor)
		return boolean;



	-- Similar to function get_connect_status (see above), 
	-- but takes cursors to the primary and secondary segment:
	function get_connect_status (
		primary 	: in pac_net_segments.cursor;
		AB_end		: in type_start_end_point;
		secondary	: in pac_net_segments.cursor)
		return type_connect_status;
	


	
	net_line_width : constant type_distance_positive := 0.2;
	-- CS rename to net_linewidth ?



	
	
end et_net_segment;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

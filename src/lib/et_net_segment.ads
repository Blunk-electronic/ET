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

with et_module_instance;		use et_module_instance;
with et_schematic_geometry;		use et_schematic_geometry;
with et_schematic_coordinates;	use et_schematic_coordinates;
with et_logging;				use et_logging;
with et_net_names;				use et_net_names;
with et_net_labels;				use et_net_labels;
with et_net_connectors;			use et_net_connectors;
with et_net_ports;				use et_net_ports;
with et_net_junction;			use et_net_junction;
with et_device_name;			use et_device_name;
with et_netlists;
with et_directions;				use et_directions;


package et_net_segment is

	use pac_geometry_2;

	
	type type_net_segment is new type_line with record -- CS make private
		labels		: pac_net_labels.list;
		connectors	: type_net_connectors;
		junctions	: type_junctions;
		ports		: type_ports_AB; -- CS
		--ports		: type_ports;
	end record;


	-- Resets all components of a segment:
	procedure reset_net_segment (
		segment	: in out type_net_segment);

	
	type type_segment_array is array (natural range <>) of type_net_segment;

	-- Creates a bare net segment without labels, 
	-- junctions and ports:
	function to_net_segment (
		A, B : in type_vector_model)
		return type_net_segment;


	-- Renames the name of a device on
	-- the given end of the given segment. If the
	-- device_old does not exist then nothing happens:
	procedure rename_device_port (
		segment		: in out type_net_segment;
		AB_end		: in type_start_end_point;
		device_old	: in type_device_name;
		device_new	: in type_device_name);
	
	-- CS: rename_netchanger_port, rename_submodule_port


	

	-- Adds a net label to the given segment.
	-- The label is rejected if it is already
	-- in the list of net labels:
	procedure add_label (
		segment	: in out type_net_segment;
		label	: in type_net_label);					
	

	-- Returns true if the given net segment
	-- has a junction on the A or B end:
	function has_junctions (
		segment	: in type_net_segment)
		return boolean;

	
	-- Activates a junction at the given end:
	procedure set_junction (
		segment	: in out type_net_segment;
		AB_end	: in type_start_end_point);

	
	-- Removes a junction at the given end:	
	procedure clear_junction (
		segment	: in out type_net_segment;
		AB_end	: in type_start_end_point);


	-- Returns the status of a junction at the specified
	-- end of the segment:
	function get_junction_status (
		segment	: in type_net_segment;
		AB_end	: in type_start_end_point)
		return boolean;
	

	-- Returns the status of a junction at the specified
	-- NSWE end of a segment:
	function get_junction_status (
		segment		: in type_net_segment;
		NSWE_end	: in type_direction_NSWE)
		return boolean;



	-- Returns the status of a connector at the specified
	-- end of a segment:
	function get_connector_status (
		segment	: in type_net_segment;
		AB_end	: in type_start_end_point)
		return boolean;


	-- Returns true if the given net segment
	-- has a net connector on the A or B end:
	function has_connectors (
		segment	: in type_net_segment)
		return boolean;

	
	-- Sets a connector on the given end of
	-- the given segment:
	procedure set_connector (
		segment	: in out type_net_segment;
		AB_end	: in type_start_end_point);


	-- Deletes a connector on the given end of
	-- the given segment:
	procedure delete_connector (
		segment	: in out type_net_segment;
		AB_end	: in type_start_end_point);

	
	
	-- Returns the status of a connector at the specified
	-- NSWE end of a segment:
	function get_connector_status (
		segment		: in type_net_segment;
		NSWE_end	: in type_direction_NSWE)
		return boolean;


	-- Returns the connector at the specified
	-- end of a segment:
	function get_connector (
		segment	: in type_net_segment;
		AB_end	: in type_start_end_point)
		return type_net_connector;


	-- Returns the connector at the specified
	-- NSWE end of a segment:
	function get_connector (
		segment		: in type_net_segment;
		NSWE_end	: in type_direction_NSWE)
		return type_net_connector;

	
	
	-- Returns true if the given netchanger port
	-- is connected with the given segment:
	function is_connected (
		segment	: in type_net_segment;
		port	: in et_netlists.type_port_netchanger)
		return boolean;


	-- Inserts in the given segment at the given
	-- end the given device port.
	-- If the port is alrady there then nothing happens:
	procedure insert_device_port (
		segment	: in out type_net_segment;
		AB_end	: in type_start_end_point;
		port	: in type_device_port);


	-- Deletes in the given segment at the given
	-- end the given device port.
	-- If the port has already been deleted then nothing happens:
	procedure delete_device_port (
		segment	: in out type_net_segment;
		AB_end	: in type_start_end_point;
		port	: in type_device_port);
	
	
	-- Inserts in the given segment at the given
	-- end the given netchanger port.
	-- If the port is alrady there then nothing happens:
	procedure insert_netchanger_port (
		segment	: in out type_net_segment;
		AB_end	: in type_start_end_point;
		port	: in et_netlists.type_port_netchanger);


	
	
	-- Inserts in the given segment at the given
	-- end the given submodule port.
	-- If the port is alrady there then nothing happens:
	procedure insert_submodule_port (
		segment	: in out type_net_segment;
		AB_end	: in type_start_end_point;
		port	: in type_submodule_port);


	procedure delete_netchanger_port (
		segment	: in out type_net_segment;
		port	: in et_netlists.type_port_netchanger;
		deleted : out boolean);

	
	procedure delete_submodule_port (
		segment	: in out type_net_segment;
		port	: in type_submodule_port;
		deleted : out boolean);


	-- Deletes all ports belonging to a
	-- given submodule:
	procedure delete_submodule_ports (
		segment	: in out type_net_segment;
		module	: in pac_module_instance_name.bounded_string);
	
	
	-- Returns true if the net segment
	-- has any ports (devices, submodules, netchangers)
	-- on the given end (A or B):
	function has_ports (
		segment : in type_net_segment;
		AB_end	: in type_start_end_point)				   
		return boolean;

	
	-- Returns true if the net segment
	-- has any ports (devices, submodules, netchangers):
	function has_ports (
		segment : in type_net_segment)
		return boolean;


	-- Returns the ports that are connected with
	-- the given end of a segment:
	function get_ports (
		segment : in type_net_segment;
		AB_end	: in type_start_end_point)				   
		return type_ports;


	-- Returns the ports that are connected with
	-- the given NSWE end of a segment:
	function get_ports (
		segment 	: in type_net_segment;
		NSWE_end	: in type_direction_NSWE)				   
		return type_ports;

	

	-- Returns the number of ports connected with
	-- the given end of a segment:
	function get_port_count (
		segment : in type_net_segment;
		AB_end	: in type_start_end_point)   
		return natural;

								
	-- Appends to a segment the given ports at the given
	-- end (A/B):
	procedure append_ports (
		segment : in out type_net_segment;
		ports	: in type_ports;						   
		AB_end	: in type_start_end_point);

	

	-- When a segment is to be split in two
	-- segments, then this type should be used for
	-- the output of a split operation.
	-- It is a parameterized type because the split
	-- operation may result in a single segment in case
	-- the split point is the same as the A or B end
	-- of the given segment:
	type type_split_segment (count : positive) is record
		segments : type_segment_array (1 .. count);
	end record;


	-- This function splits a segment at the given point.
	-- The result is two new segments which join each 
	-- other at the given point.
	-- Since the given point is not required to be on 
	-- the given segment, the resulting segments may run in 
	-- to different directions.
	-- If either A or B of the given segment is the same
	-- as the given split point, then there is nothing
	-- to do. The result is a single segment, namely the 
	-- given segment without any change:	
	function split_segment (
		segment	: in type_net_segment;
		point	: in type_vector_model)
		return type_split_segment;


	
	-- Merges secondary net segment with primary segment.
	-- 1. Assumes that both segments run into the same direction.
	-- 2. Assumes that both segments join each other at a common end point (A or B).
	-- 3. If the joint is connected with any ports, the a constraint error
	--    is raised.
	-- 4. The ports connected with the open ends of the two segments
	--    are kept.
	-- 5. Net labels of both segments are kept.
	-- 6. The status of the junctions at the open ends of the two 
	--    segments are kept, whereas the junctions at the joint are removed.
	-- 7. Connectors at the open ends of the two segments are kept,
	--    whereas connectors at the joint are removed.
	procedure merge_segments (
		-- The primary segment and its end to be connected:
		primary			: in out type_net_segment;
		primary_end		: in type_start_end_point; 
		
		-- The secondary segment and its end to be connected:
		secondary		: in type_net_segment;
		secondary_end	: in type_start_end_point);


	
	
	-- Reset status flags of segment, junctions and labels:
	overriding procedure reset_status (
		segment : in out type_net_segment);
	
	



	
-- CONNECT STATUS OF TWO SEGMENTS:

	type type_connect_status is (CON_STS_A, CON_STS_B, CON_STS_NONE);

	-- Tests whether the given primary segment is 
	-- connected with the given secondary segment.
	-- Starting at the primary segment and its end (A/B),
	-- the ends of the secondary segment are tested.
	-- The reference point P to be tested is determined by the
	-- given AB_end of the primary segment.
	-- The return is as follows:
	-- 1. If the A end of the secondary segment is
	--    at P, then the result is CON_STS_A.
	-- 2. If the B end of the secondary segment is
	--    at P, then the result is CON_STS_B.
	-- 3. If neither condition 1 nor 2 is met, then
	--    the return is CON_STS_NONE:
	function get_connect_status (
		primary 	: in type_net_segment;
		AB_end		: in type_start_end_point;
		secondary	: in type_net_segment)
		return type_connect_status;




	-- Returns true if given secondary segment
	-- starts or ends between A and B of the primary
	-- segment.
	-- AB_end indicates which end of the secondary segment
	-- is to be tested:
	function between_A_and_B (
		secondary	: in type_net_segment;
		AB_end		: in type_start_end_point;
		primary		: in type_net_segment)
		return boolean;


	
	-- Merges two overlapping segments to a single one.
	-- "Overlapping" means that both have same orientation
	-- and do overlap in some way.
	-- 1. If any of the segments is a slope then an exception
	--    is raised.
	-- 2. If the orientations of the two segments differ then
	--    an exception is raised.
	-- 3. If their orientation is horizontal, then the
	--    resulting net segment has the A end in the WEST and
	--    the B end in the EAST.
	-- 4. If their orientation is vertical, then the
	--    resulting net segment has the A end in the SOUTH and
	--    the B end in the NORTH.
	function merge_overlapping_segments (
		primary, secondary : in type_net_segment)
		return type_net_segment;
	
	
	
	package pac_net_segments is new doubly_linked_lists (type_net_segment);
	use pac_net_segments;


	-- Returns the status of a junction at the specified
	-- end of the segment:
	function get_junction_status (
		segment	: in pac_net_segments.cursor;
		AB_end	: in type_start_end_point)
		return boolean;


	-- Returns the ports that are connected with
	-- the given end of a segment:
	function get_ports (
		segment : in pac_net_segments.cursor;
		AB_end	: in type_start_end_point)				   
		return type_ports;


	

	-- Returns true if the net segment
	-- has any ports (devices, submodules, netchangers):
	function has_ports (
		segment : in pac_net_segments.cursor)
		return boolean;


	function has_ports (
		segment : in pac_net_segments.cursor;
		AB_end	: in type_start_end_point)				   
		return boolean;
	
	
	function is_selected (
		segment : in pac_net_segments.cursor)
		return boolean;

	
	
	function get_A (
		segment : in pac_net_segments.cursor)
		return type_vector_model;


	function get_B (
		segment : in pac_net_segments.cursor)
		return type_vector_model;


	function get_end_point (
		segment : in pac_net_segments.cursor;
		AB_end	: in type_start_end_point)				   
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


	-- Returns true if the given two net segments
	-- overlap each other.
	-- It is assumed that none of the given segments
	-- is a slope. If any of the segments is sloping, then
	-- the return is always false:
	-- If argument "test_touch" is true, then the start and end points
	-- of the segments are tested whether they meet each other:
	function segments_overlap (
		s1, s2		: in pac_net_segments.cursor;
		test_touch	: in boolean := false)
		return boolean;


	
	

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



	-- Splits a segment. See details in comments
	-- of function split_segment above:
	function split_segment (
		segment	: in pac_net_segments.cursor;
		point	: in type_vector_model)
		return type_split_segment;



	
	-- Similar to function get_connect_status (see above), 
	-- but takes cursors to the primary and secondary segment:
	function get_connect_status (
		primary 	: in pac_net_segments.cursor;
		AB_end		: in type_start_end_point;
		secondary	: in pac_net_segments.cursor)
		return type_connect_status;
	


	-- Returns all points where the given
	-- net segments start or end.
	-- With other words: Returns all points occupied
	-- by the start or end points of the given net segments:
	function get_end_points (
		segments	: in pac_net_segments.list)
		return pac_points.list;

	
	
	
	
	net_line_width : constant type_distance_positive := 0.2;
	-- CS rename to net_linewidth ?



	
	
end et_net_segment;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                    SCHEMATIC OPERATIONS ON NETS                          --
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
--   ToDo: 


with et_net_labels;					use et_net_labels;
with et_net_class;					use et_net_class;
with et_object_status;				use et_object_status;


package et_schematic_ops.nets is

	
	-- This function returns the total number
	-- of nets of the given module:
	function get_net_count (
		module		: in pac_generic_modules.cursor)
		return type_net_count;

	

-- SEGMENTS:
	

	-- Sets the start or end points of
	-- net segments which start or end
	-- at the given position to "moving".
	-- If a segment starts an the given position then A will be set as "moving".
	-- If a segment ends an the given position then B will be set as "moving".
	procedure set_segments_moving (
		module_cursor	: in pac_generic_modules.cursor;
		position		: in type_object_position;							  
		log_threshold	: in type_log_level);


	-- Clears the proposed-flag and the selected-flag of all net segments:
	procedure reset_proposed_segments ( -- CS rename to reset_segments
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level);


	
	-- This composite type is meant to identify a net segment
	-- and its parent net in the schematic:
	type type_object_segment is record
		net_cursor		: pac_nets.cursor;
		strand_cursor	: pac_strands.cursor;
		segment_cursor	: pac_net_segments.cursor;
	end record;


	-- Returns the net name and segment of the given object
	-- as string in the form like "GND segment start x/y end x/y":
	function to_string (
		object	: in type_object_segment)
		return string;


	-- Returns the sheet number of the given net segment:
	function get_sheet (
		object	: in type_object_segment)
		return type_sheet;

	
	package pac_object_segments is new doubly_linked_lists (type_object_segment);

	
	
	-- Modifies the status flag of a net segment:
	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		segment			: in type_object_segment;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level);

	
	-- Sets the proposed-flag of all segments which are in the
	-- given zone around the given place on the currently active sheet.
	-- Adds to count the number of segments that have been found:
	procedure propose_segments (
		module_cursor	: in pac_generic_modules.cursor;
		catch_zone		: in type_catch_zone;
		count			: in out natural;
		log_threshold	: in type_log_level);


	-- Returns the first net segment according to the given flag.
	-- If no segment has been found, then the return is no_element:
	function get_first_segment (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;
		log_threshold	: in type_log_level)
		return type_object_segment;



	-- Deletes a net segment:
	procedure delete_segment (
		module_cursor	: in pac_generic_modules.cursor;
		segment			: in type_object_segment;
		log_threshold	: in type_log_level);

	
	-- Deletes the first net segment found
	-- in the given zone:
	procedure delete_segment (
		module_cursor	: in pac_generic_modules.cursor;
		sheet			: in type_sheet;
		catch_zone		: in type_catch_zone;
		log_threshold	: in type_log_level);



	-- Tests whether the given segment is movable
	-- at the given point_of_attack.
	-- Whether the A or B end (or both) are affected
	-- is determined according to the given point of attack.
	-- If the related segment is connected with a port of 
	-- any device, netchanger or submodule, then it can not be moved.
	-- Returns true if movable, returns falso otherwise.
	function segment_is_movable (
		module_cursor	: in pac_generic_modules.cursor;
		segment			: in type_net_segment;
		point_of_attack	: in type_object_position;
		log_threshold	: in type_log_level) 
		return boolean;

	
	-- Tests whether the given segment is movable
	-- at the given end point (A or B).
	-- If the given end point is connected with a port of 
	-- any device, netchanger or submodule, then it can not be moved.
	-- Returns true if movable, returns falso otherwise.
	function segment_is_movable (
		module_cursor	: in pac_generic_modules.cursor;
		segment			: in type_object_segment;
		AB_end			: in type_start_end_point;
		log_threshold	: in type_log_level) 
		return boolean;


	-- Tests whether the given segment is movable
	-- at the given zone.
	-- If the given end point is connected with a port of 
	-- any device, netchanger or submodule, then it can not be moved.
	-- Returns true if movable, returns falso otherwise.
	function segment_is_movable (
		module_cursor	: in pac_generic_modules.cursor;
		segment			: in type_object_segment;
		zone			: in type_line_zone;
		log_threshold	: in type_log_level) 
		return boolean;


	-- This procedure takes a primary segment and
	-- searches for secondary segments that are attached to
	-- the A or B end of the primary segment and moves their
	-- ends by the given displacement.
	-- The given primary segment is a composite type
	-- that provides cursors to net, strand and segment AFTER
	-- dragging of the the primary segment. So the state of the
	-- primary segment BEFORE must also be known. For this reason
	-- we also pass the original primary segment also:
	procedure move_secondary_segments (
		module_cursor	: in pac_generic_modules.cursor;
		primary_segment	: in type_object_segment; -- new state of primary segment
		original_segment: in type_net_segment; -- original state of the primary segment
		AB_end			: in type_start_end_point; -- A/B of primary segment
		displacement	: in type_vector_model;
		log_threshold	: in type_log_level);

		
										  
	-- Returns true if at the given place 
	-- a net segment starts or ends:
	function net_segment_at_place (
		module_cursor	: in pac_generic_modules.cursor;
		place			: in type_object_position)
		return boolean;
	

	-- Returns a list of net segments which cross
	-- the given catch zone:
	function get_segments (
		module_cursor	: in pac_generic_modules.cursor;
		sheet			: in type_sheet;
		catch_zone		: in type_catch_zone;
		log_threshold	: in type_log_level)
		return pac_object_segments.list;


	
	-- This procedure moves a given primary net segment
	-- which is attacked by the point of attack (POA).
	-- It computes the zone that is being attcked
	-- and the displacement (useful for other segments
	-- connected with the primary segment).
	-- Outputs also the original old primary segment
	-- (also required to drag secondary segments along):
	procedure move_primary_segment (
		module_cursor	: in pac_generic_modules.cursor;
		primary_segment	: in type_object_segment;
		POA				: in type_vector_model;
		coordinates		: in type_coordinates; -- relative/absolute
		destination		: in type_vector_model; -- x/y, the new position 
		zone			: out type_line_zone;
		displacement	: out type_vector_model;
		segment_old		: in out type_net_segment;
		log_threshold	: in type_log_level);

	
	-- Drags a segment of a net. The segment to be modified
	-- is searched for in the given catch zone on the given sheet.
	-- If more than one segment has been found in the given zone,
	-- then the first of them will be selected.
	-- We call this segment "primary segment". Other segments which
	-- might be connected with it are called "secondary segments".
	-- The secondary segments will be dragged along with the primary segment.
	-- If the primary segment is dragged to a place where it meets a port
	-- of a device, netchanger or submodule, then the segment will be 
	-- connected with that port.
	-- NOTE: If the segment meets another net, then these 
	-- two nets will NOT be connected.
	-- CS: The resulting overlapping segments should be detected by the ERC
	-- or better the drag operation should be rejected.
	procedure drag_segment (
		module_cursor	: in pac_generic_modules.cursor;
		sheet			: in type_sheet;
		catch_zone		: in type_catch_zone;
		coordinates		: in type_coordinates; -- relative/absolute
		destination		: in type_vector_model; -- x/y
		log_threshold	: in type_log_level);


	-- Drags a segment of a net.
	-- If the segment meets a port, then the port will be connected with the net.
	-- NOTE: If the segment meets another net, then these two nets will NOT be connected.
	--       CS: The resulting overlapping segments should be detected by the ERC.
	-- CS: This procedure is currently not used:
	procedure drag_segment (
		module_cursor	: in pac_generic_modules.cursor;
		primary_segment	: in type_object_segment;
		POA				: in type_vector_model;
		destination		: in type_vector_model; -- x/y
		log_threshold	: in type_log_level);


	
	
-- STRANDS:
	

	-- This composite type is meant to identify a strand
	-- and its parent net in the schematic:
	type type_object_strand is record
		net_cursor		: pac_nets.cursor;
		strand_cursor	: pac_strands.cursor;
	end record;

	
	-- Returns the net name and strand position of the given object
	-- as string in the form like "GND strand sheet / start x/y end x/y":
	function to_string (
		object	: in type_object_strand)
		return string;



	
	package pac_object_strands is new doubly_linked_lists (type_object_strand);


	-- Returns for a given net a list of strands
	-- that cross the given place:
	function get_strands (
		module_cursor	: in pac_generic_modules.cursor;
		net_cursor		: in pac_nets.cursor;
		place			: in type_object_position;
		log_threshold	: in type_log_level)
		return pac_strand_cursors.list;


	
	-- Returns for a given primary net segment a list of strands
	-- having a segment that ends between A and B of the given segment:
	function get_strands (
		module_cursor	: in pac_generic_modules.cursor;
		net_cursor		: in pac_nets.cursor;
		primary			: in type_net_segment;
		sheet			: in type_sheet;
		log_threshold	: in type_log_level)
		return pac_strand_segment_cursors.list;

	
	
	-- Returns a list of strands which cross
	-- the given catch zone:
	function get_strands (
		module_cursor	: in pac_generic_modules.cursor;
		sheet			: in type_sheet;
		catch_zone		: in type_catch_zone;
		log_threshold	: in type_log_level)
		return pac_object_strands.list;
	
	

	-- Clears the proposed-flag and the selected-flag of all strands:
	procedure reset_strands (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level);
	
	
	-- Sets the proposed-flag of all strands which are in the
	-- given zone around the given place on the currently active sheet.
	-- Adds to count the number of strands that have been found:
	procedure propose_strands (
		module_cursor	: in pac_generic_modules.cursor;
		catch_zone		: in type_catch_zone;
		count			: in out natural;
		log_threshold	: in type_log_level);


	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		strand			: in type_object_strand;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level);


	-- Returns the first strand according to the given flag.
	-- If no strand has been found, then the return is no_element:
	function get_first_strand (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;
		log_threshold	: in type_log_level)
		return type_object_strand;


	-- Deletes a strand.
	-- If the affected net has no strands anymore,
	-- then the whole net will be deleted:
	procedure delete_strand (
		module_cursor	: in pac_generic_modules.cursor;
		strand			: in type_object_strand;
		log_threshold	: in type_log_level);

	
	-- Deletes the first strand found in the given zone.
	-- If the affected net has no strands anymore,
	-- then the whole net will be deleted:
	procedure delete_strand (
		module_cursor	: in pac_generic_modules.cursor;
		sheet			: in type_sheet;
		catch_zone		: in type_catch_zone;
		log_threshold	: in type_log_level);

	

	-- After moving, dragging, deleting, adding of nets
	-- or net segments, the positions of strands must be updated.
	-- This procedure should be called for this purpose:
	procedure update_strand_positions (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level);


	

-- NETS:
	
	-- This composite type is meant to identify a net
	-- in the schematic:
	type type_object_net is record
		net_cursor		: pac_nets.cursor;
	end record;


	-- Returns the net name of the given object
	-- as string in the form like "GND":
	function to_string (
		object	: in type_object_net)
		return string;



	-- Clears the proposed-flag and the selected-flag of all nets:
	procedure reset_proposed_nets ( -- CS rename to reset_nets
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level);

	

	
	-- Modifies the status flag of a complete net:
	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		net				: in type_object_net;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level);

	

	-- Returns the first net according to the given flag.
	-- If no net has been found, then the return is no_element:
	function get_first_net (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;
		log_threshold	: in type_log_level)
		return type_object_net;

	
	
	-- Sets the proposed-flag of all nets which have a segment in the
	-- given zone around the given place on the currently active sheet.
	-- Adds to count the number of nets that have been found:
	procedure propose_nets (
		module_cursor	: in pac_generic_modules.cursor;
		catch_zone		: in type_catch_zone;
		count			: in out natural;
		log_threshold	: in type_log_level);


	
		
	-- Returns a cursor to the requested net in the given module.
	-- If the net could not be found, returns no_element:
	function locate_net (
		module_cursor	: in pac_generic_modules.cursor;
		net_name		: in pac_net_name.bounded_string)
		return pac_nets.cursor;



	-- Creates a new net. If the net does not exist already,
	-- then it will be created and the flag "created" is set.
	-- If the net does exist already, then "created" is cleared
	-- and nothing else will be done.
	-- net_cursor will be set so that it points to the
	-- net in any case:
	procedure create_net (
		module_cursor	: in pac_generic_modules.cursor;
		net_name		: in pac_net_name.bounded_string;
		created			: out boolean;
		net_cursor		: out pac_nets.cursor;
		log_threshold	: in type_log_level);


	
	
	-- Searches the module for an anonymous net with the lowest index available.
	-- Example: If the module contains nets like N$2, N$4, N$5 and N$101 then
	-- the lowest available name would be N$3.
	function get_lowest_available_anonymous_net (
		module		: in pac_generic_modules.cursor)
		return pac_net_name.bounded_string; -- N$3



	-- This flag indicates whether an operation
	-- on a net affects the active sheet or all sheets:
	modify_net_on_all_sheets : boolean := false;
	
	
	
	-- Renames a net. The scope determines whether to rename a certain strand,
	-- all strands on a certain sheet or on all sheets.
	-- CS If a particular strand on a sheet is to be renamed, the argument "place"
	-- must provide sheet and x/y start position of strand. 
	-- CS: In the future x/y can be any point on any segment of the strand.
	procedure rename_net (
		module_cursor	: in pac_generic_modules.cursor;
		net_name_before	: in pac_net_name.bounded_string; -- RESET, MOTOR_ON_OFF
		net_name_after	: in pac_net_name.bounded_string; -- RESET_N, MOTOR_ON_OFF_N	
		scope			: in type_net_scope; -- strand, sheet, everywhere
		place			: in type_object_position; -- sheet/x/y
		log_threshold	: in type_log_level);



	-- Deletes a net on the given sheet. If all_sheets is
	-- true, then all nets on all sheets are deleted:
	procedure delete_net (
		module_cursor	: in pac_generic_modules.cursor;
		net				: in type_object_net;
		sheet			: in type_sheet;
		all_sheets		: in boolean := false;
		log_threshold	: in type_log_level);


	-- Deletes a net on the given sheet. If all_sheets is
	-- true, then all nets on all sheets are deleted:
	procedure delete_net (
		module_cursor	: in pac_generic_modules.cursor;
		net_name		: in pac_net_name.bounded_string;
		sheet			: in type_sheet;
		all_sheets		: in boolean := false;
		log_threshold	: in type_log_level);


	
	-- Deletes a net. The scope determines whether to delete a certain strand,
	-- all strands on a certain sheet or on all sheets.
	-- CS If a particular strand on a sheet is to be deleted, the argument "place"
	-- must provide sheet and x/y start position of strand. In the future x/y can be
	-- any point on any segment of the strand.
	-- CS: This procedure is not used anymore.
	procedure delete_net (
		module_cursor	: in pac_generic_modules.cursor;
		net_name		: in pac_net_name.bounded_string; -- RESET, MOTOR_ON_OFF
		scope			: in type_net_scope; -- strand, sheet, everywhere
		place			: in type_object_position; -- sheet/x/y
		log_threshold	: in type_log_level);


	
	-- Shows/highlights a complete net:
	-- Currently just sets the status of the
	-- whole net as "selected":
	procedure show_net (
		module_cursor	: in pac_generic_modules.cursor;
		net_cursor		: in pac_nets.cursor;
		log_threshold	: in type_log_level);


	

	-- Returns the name of the first net of the given module.
	-- Net names are sorted alphabetically:
	function get_first_net (
		module_cursor	: in pac_generic_modules.cursor)
		return pac_net_name.bounded_string;
	

	
	-- Returns the names of all nets of the given module
	-- sorted alphabetically:
	function get_nets (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
		return pac_net_names.list;


	
	-- Returns for the given net a unique index.
	-- If the given net has not been found, raises exception:
	function get_net_index (
		module_cursor	: in pac_generic_modules.cursor;
		net_name		: in pac_net_name.bounded_string;
		log_threshold	: in type_log_level)
		return type_net_index;

	
	
	-- Returns a lists of nets that cross the given place.
	function get_nets_at_place (
		module_cursor	: in pac_generic_modules.cursor;
		place			: in type_object_position;
		log_threshold	: in type_log_level)
		return pac_net_names.list; -- CS return a list of cursors ?


	-- Before inserting net segments into a net, it must
	-- be ensured, that none of them collides with a foreign net.
	-- This function returns true if any of the given segments of 
	-- the given net on the given sheet touches a foreign net.
	-- Only matching A and B ends of segments are tested.
	-- Segments that cross each other between their A or B ends
	-- are ignored:
	function segments_touch_foreign_net (
		module_cursor	: in pac_generic_modules.cursor;
		net_cursor		: in pac_nets.cursor; -- the target net
		sheet			: in type_sheet;
		segments		: in pac_net_segments.list; -- the segments to be inserted
		log_threshold	: in type_log_level)
		return boolean;
	

	-- Inserts a list of net segments in the given net.
	-- Connects the segments with strands and ports of
	-- devices, netchangers and submodules.
	-- If any of the given segments starts or ends
	-- on a foreign net, then ALL given segments are rejected:
	procedure insert_net_segments (
		module_cursor	: in pac_generic_modules.cursor;
		net_cursor		: in pac_nets.cursor;
		sheet			: in type_sheet;
		segments		: in pac_net_segments.list;
		log_threshold	: in type_log_level);
	

	
	-- Inserts the given net segment in the given net
	-- on the given sheet. If the segment runs across the
	-- ends of other segments (of the same strand) then
	-- it will be broken down into smaller segments:
	procedure insert_net_segment (
		module_cursor	: in pac_generic_modules.cursor;
		net_cursor		: in pac_nets.cursor;
		sheet			: in type_sheet;
		segment			: in type_net_segment;
		log_threshold	: in type_log_level);
	
	
	
	-- Inserts a net segment to a given net. If the given net does
	-- not exist, then the net will be created.
	-- Find more details in description of procedure insert_net_segment
	-- above.
	-- Updates strand positions and the ratsnest:
	procedure insert_net_segment (
		module_cursor	: in pac_generic_modules.cursor;
		net_name		: in pac_net_name.bounded_string; -- RESET, MOTOR_ON_OFF
		A				: in type_object_position; -- sheet/x/y
		B				: in type_vector_model; -- x/y
		log_threshold	: in type_log_level);

	

	-- Sets the net class of a net:
	procedure set_net_class (
		module_cursor	: in pac_generic_modules.cursor;
		net_name		: in pac_net_name.bounded_string; -- RESET, MOTOR_ON_OFF
		net_class		: in pac_net_class_name.bounded_string; -- pwr
		log_threshold	: in type_log_level);

	
	
	-- Sets the scope of a net.
	procedure set_scope (
		module_cursor	: in pac_generic_modules.cursor;
		net_name		: in pac_net_name.bounded_string; -- RESET, MOTOR_ON_OFF
		scope			: in et_netlists.type_net_scope; -- local/global
		log_threshold	: in type_log_level);

	
	-- Places a net junction at the given position.
	-- If the junction is to be placed between start and end point of a segment, then the segment 
	-- is split in two new segments with the junction between them.
	-- If there is no net segment at the given position, no junction is placed and warning issued.
	procedure place_junction (
		module_cursor	: in pac_generic_modules.cursor;
		place			: in type_object_position; -- sheet/x/y, rotation doesn't matter
		log_threshold	: in type_log_level);

-- CS
-- 	procedure place_junction (
-- 	-- Places a net junction at the given position.
-- 	-- If the junction is to be placed between start and end point of a segment, then the segment 
-- 	-- is split in two new segments with the junction between them.
-- 	-- If there is no net segment at the given position, no junction is placed and warning issued.
-- 		module_cursor	: in pac_generic_modules.cursor;
-- 		place			: in type_object_position; -- sheet/x/y, rotation doesn't matter
-- 		log_threshold	: in type_log_level);



-- LABELS:
	

	-- This composite type is meant to identify a 
	-- simple net label
	-- and its parent net in the schematic:
	type type_object_label is record -- CS rename to type_object_label_simple
		net_cursor		: pac_nets.cursor;
		strand_cursor	: pac_strands.cursor;
		segment_cursor	: pac_net_segments.cursor;
		label_cursor	: pac_net_labels.cursor;
	end record;

	
	-- Returns the net name and the position of the given object
	-- as string in the form like "GND simple label at x/y":
	function to_string (
		object	: in type_object_label)
		return string;


	type type_object_label_tag is record
		net_cursor		: pac_nets.cursor;
		strand_cursor	: pac_strands.cursor;
		segment_cursor	: pac_net_segments.cursor;
		start_end		: type_start_end_point := A;
	end record;

	
	-- Returns the net name and the position of the given object
	-- as string in the form like "GND tag label at x/y":
	function to_string (
		object	: in type_object_label_tag)
		return string;

	

	-- Resets the status flags of all net labels 
	-- (both simple and tag labels):
	procedure reset_labels (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level);



	-- Modifies the status flag of a simple net label:
	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		label			: in type_object_label;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level);


	-- Modifies the status flag of a tag net label:
	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		label			: in type_object_label_tag;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level);

	

	-- Sets the proposed-flag of all net labels which are in the
	-- given zone around the given place on the currently active sheet.
	-- Adds to count the number of labels that have been found:
	procedure propose_labels ( -- CS rename to propose_labels_simple
		module_cursor	: in pac_generic_modules.cursor;
		catch_zone		: in type_catch_zone;
		count			: in out natural;
		log_threshold	: in type_log_level);


	procedure propose_labels_tag (
		module_cursor	: in pac_generic_modules.cursor;
		catch_zone		: in type_catch_zone;
		count			: in out natural;
		log_threshold	: in type_log_level);

	

	-- Returns the first net label according to the given flag.
	-- If no label has been found, then the return is no_element:
	function get_first_label ( -- CS rename to get_first_label_simple
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;
		log_threshold	: in type_log_level)
		return type_object_label;


	function get_first_label_tag (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;
		log_threshold	: in type_log_level)
		return type_object_label_tag;

	
	
	
	-- Places a label next to a segment at position.
	procedure place_net_label_simple (
		module_cursor	: in pac_generic_modules.cursor;

		-- CS size ?
		
		-- The reference point at the segment:
		segment_position: in type_object_position; -- sheet/x/y

		-- The position of the label relative to segment_position:
		label_position	: in type_vector_model := origin; -- x/y

		-- The rotation is relevant for simple labels only. 
		-- The label will always be placed
		-- so that is is readable from the front or from the right.
		-- Whatever angle you provide here, the final roation will 
		-- be either 0 or 90 degrees.
		rotation		: in et_schematic_coordinates.type_rotation_model := zero_rotation;
		log_threshold	: in type_log_level);


	
	procedure place_net_label_tag (
		module_cursor	: in pac_generic_modules.cursor;
		-- CS size ?
		position		: in type_object_position; -- sheet/x/y
		direction		: in type_net_label_direction; -- INPUT, OUTPUT, PASSIVE, ...
		log_threshold	: in type_log_level);


	
	
	-- Deletes a label.
	procedure delete_net_label (
		module_cursor	: in pac_generic_modules.cursor;
		position		: in type_object_position; -- sheet/x/y
		log_threshold	: in type_log_level);

	-- CS procedure move_net_label

	
	-- Queries the position of the given net. If a stub is at the
	-- given position, then returns the direction of the stub.
	function query_stub (
		module_cursor	: in pac_generic_modules.cursor;
		net_name		: in pac_net_name.bounded_string; -- RESET, MOTOR_ON_OFF
		position		: in type_object_position; -- sheet/x/y
		log_threshold	: in type_log_level)
		return type_stub;



	-- Shows/highlights a simple label by setting its 
	-- status to "selected":
	procedure show_label_simple (
		module_cursor	: in pac_generic_modules.cursor;
		label			: in type_object_label;
		log_threshold	: in type_log_level);

	
	-- Shows/highlights a tag label by setting its 
	-- status to "selected":
	procedure show_label_tag (
		module_cursor	: in pac_generic_modules.cursor;
		label			: in type_object_label_tag;
		log_threshold	: in type_log_level);

	

------------------------------------------------------------------------------------------

-- OBJECTS:


	type type_object_category is (
		CAT_VOID,
		CAT_SEGMENT,
		CAT_STRAND,
		CAT_NET,
		CAT_LABEL, -- CS rename to CAT_LABEL_SIMPLE
		CAT_LABEL_TAG);



	-- This type wraps all kinds of objects into a single type:
	type type_object (cat : type_object_category) is record
		case cat is
			when CAT_VOID => null;
			
			when CAT_SEGMENT =>
				segment : type_object_segment;

			when CAT_STRAND =>
				strand : type_object_strand;
				
			when CAT_NET =>
				net : type_object_net;
				
			when CAT_LABEL => --  CS rename to CAT_LABEL_SIMPLE
				label : type_object_label; -- CS rename to label_simple

			when CAT_LABEL_TAG =>
				label_tag : type_object_label_tag;
		end case;
	end record;

	
	
	package pac_objects is new indefinite_doubly_linked_lists (type_object);
	use pac_objects;


	function to_string (
		object_cursor : in pac_objects.cursor)
		return string;

	

	-- Returns the number of items stored in the given list:
	function get_count (
		objects : in pac_objects.list)
		return natural;



	-- Returns the cursor to the net of the given
	-- object. The object must be of CAT_SEGMENT or CAT_NET:
	function get_net (
		object_cursor : in pac_objects.cursor)
		return pac_nets.cursor;

	
	-- Returns the cursor to the strand of the given
	-- object. The object must be of CAT_SEGMENT:
	function get_strand (
		object_cursor : in pac_objects.cursor)
		return pac_strands.cursor;


	-- Returns the cursor to the actual net segment of the given
	-- object. The object must be of CAT_SEGMENT:
	function get_segment (
		object_cursor : in pac_objects.cursor)
		return pac_net_segments.cursor;



	

	-- Returns the first object
	-- according to the given flag.
	-- If nothing found, then the return is a void object (CAT_VOID):
	function get_first_object (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;								 
		log_threshold	: in type_log_level)
		return type_object;

	

	-- Collects all objects 
	-- according to the given flag and returns them in a list:
	function get_objects (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;								 
		log_threshold	: in type_log_level)
		return pac_objects.list;



	-- Modifies the status flag of an object:
	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level);



	-- Modifies the status flag of an object indicated by a cursor:
	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		object_cursor	: in pac_objects.cursor;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level);



	-- This is a collective procedure that resets
	-- the proposed-flag and the selected-flag 
	-- of all objects:
	procedure reset_proposed_objects ( -- CS rename to reset_objects
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level);



	-- Sets the start or end points of net segments which are 
	-- connected with selected net segments to "moving":
	procedure set_segments_moving (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level);


	-- Sets the start or/and end point (A/B) of the given net segment
	-- as moving according to the given point of attack.
	-- Sets the global variable object_original_position.
	-- In case the primary segment is to be dragged, then 
	-- the object_original_position serves as a reference for other 
	-- net segments which are attached to the primary segment.
	-- - If a net segment is attacked at its start point (A), then
	--   the object_original_position assumes A.
	-- - If a net segment is attacked at its end point (B), then
	--   the object_original_position assumes B.
	-- - If a net segment is attacked at its center (between A and B), then
	--   the object_original_position assumes the given point_of_attack.
	-- If movable_test is true, then it tests whether the given
	-- net segment is connected with a port of any device, netchanger
	-- or submodule before setting the end point (A/B) to "moving".
	-- If moving is allowed then the flag "granted" is set.
	-- If the end can not be moved, then the flag "granted" is
	-- cleared and nothing else happens.
	procedure set_primary_segment_AB_moving (
		module_cursor	: in pac_generic_modules.cursor;
		object_cursor	: in pac_objects.cursor; -- must point to a net segment
		point_of_attack	: in type_vector_model;
		movable_test	: in boolean;
		granted			: in out boolean;
		log_threshold	: in type_log_level);


	-- Starting with a given primary segment (indicated by object_cursor),
	-- Start/end points (A/B) of secondary segments which are 
	-- connected with the primary segment are marked as "moving"
	-- via this procedure:	
	procedure set_secondary_segments_AB_moving (
		module_cursor	: in pac_generic_modules.cursor;
		object_cursor	: in pac_objects.cursor; -- must point to a primary net segment
		log_threshold	: in type_log_level);



	procedure move_object (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		point_of_attack	: in type_vector_model;
		destination		: in type_vector_model;
		log_threshold	: in type_log_level);

	
	
	procedure drag_object (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		point_of_attack	: in type_vector_model;
		destination		: in type_vector_model;
		log_threshold	: in type_log_level);



	procedure delete_object (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		log_threshold	: in type_log_level);


	procedure show_object (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		log_threshold	: in type_log_level);

	
	
end et_schematic_ops.nets;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

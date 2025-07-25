------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                             NET STRANDS                                  --
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

with et_schematic_coordinates;	use et_schematic_coordinates;
with et_logging;				use et_logging;
with et_sheets;					use et_sheets;
with et_net_ports;				use et_net_ports;
with et_net_segment;			use et_net_segment;
with et_object_status;			use et_object_status;


package et_net_strands is

	use pac_geometry_2;

	use pac_net_segments;


	-- A strand is a collection of net segments which belong to each other. 
	-- Segments belong to each other because their start/end points meet.
	-- A strand has coordinates. 
	-- x/y position are the lowest values within the strand.
	type type_strand is record -- CS make private ?
	-- NOTE: ET does not provide a name for a strand.
	-- As a strand is part of a net, there is no need for individual strand names.
		position	: type_object_position; -- sheet and lowest x/y, rotation doesn't matter -> always zero
		segments	: pac_net_segments.list;

		status : type_object_status; 
		-- IMPORTANT: status "moving" shall not be used. 
		-- CS: Use precondition when modifying the status ?
	end record;		

	
	
	-- Returns true if the given strand has segments:
	function has_segments (
		strand : in type_strand)
		return boolean;


	-- This function returns the number of ports
	-- of the given strand that exist at the given point.
	-- This implies that point lies somewhere on the strand,
	-- otherwise zero will be returned:
	function get_port_count (
		strand	: in type_strand;
		point	: in type_vector_model)
		return natural;


	-- Calculates and sets the lowest x/y position of the given strand.	
	-- Leaves the sheet number of the strand as it is.	
	procedure set_strand_position (
		strand : in out type_strand);


	-- Searches for net segments that run into the same direction
	-- and overlap each other. Replaces such segments by a single 
	-- segment:
	procedure optimize_strand (
		strand			: in out type_strand;
		log_threshold	: in type_log_level);

	
	-- If strands are to be joined (or merged) with each other
	-- then the connection can be:
	-- 1. At a single point. When a strand is moved so that it
	--    meets another strand at a certain place.
	-- 2. A single net segment that serves as a bridge between
	--    two strands.
	-- For this reason this controlled type specifies how the
	-- two strands are joined:
	type type_strand_joint (point : boolean) is record
		case point is
			when TRUE =>
				joint : type_vector_model;

			when FALSE =>
				segment : type_net_segment;
		end case;
	end record;
				
	
	-- Merges strand "source" in strand "target".
	-- The joint specifies how the two strands are connected.
	-- - If joint is a single point, then the strands will be
	--   connected at this place. CS: NOT IMPLEMENTED YET !
	-- - If joint is a segment, then its A end will be connected
	--   with the target and its B end will be connected with the source.
	--   As a result overlapping segments will occur. 
	--   An optimization procedure irons this overlapping segments out
	--   so that only a single segment remains.
	-- Updates the position of target.
	-- Leaves status of target unchanged:
	procedure merge_strands (
		target			: in out type_strand;						
		source			: in type_strand;
		joint			: in type_strand_joint;
		log_threshold	: in type_log_level);

	

	
	type type_connected_segment is record
		segment	: pac_net_segments.cursor;
		AB_end	: type_start_end_point;
	end record;


	-- Returns true if the given connected segment
	-- has any ports of devices, netchangers or submodules
	-- at its end:
	function has_ports (
		segment	: in type_connected_segment)
		return boolean;

	
	package pac_connected_segments is new 
		doubly_linked_lists (type_connected_segment);

		
	-- Returns the net segments which are connected
	-- with the given primary segment at the given end (A/B).
	-- NOTE; The primary segment must belong to the given strand.	
	function get_connected_segments (
		primary 	: in pac_net_segments.cursor;
		AB_end		: in type_start_end_point;
		strand		: in type_strand)
		return pac_connected_segments.list;




	

	-- Returns the first segment (among the given segments)
	-- on which the given point sits between A and B end:
	function get_segment_to_split (
		segments	: in pac_net_segments.list;
		point		: in type_vector_model)
		return pac_net_segments.cursor;


		
	-- Tests whether at the givne point exist other
	-- net segments except the one indicated by "except":
	function other_segments_exist (
		segments	: in pac_net_segments.list;
		except		: in pac_net_segments.cursor;
		point		: in type_vector_model)
		return boolean;
		

	-- This function returns the number of segments
	-- of the given strand that start or end at the
	-- given point:
	function get_segment_count (
		strand	: in type_strand;
		point	: in type_vector_model)
		return natural;

		
		
	type type_segment_to_extend is record -- CS make private
		cursor	: pac_net_segments.cursor;
		AB_end	: type_start_end_point;
	end record;
	
	
	-- Returns true if selector "cursor" of
	-- the given segment points to a segment:
	function has_element (
		segment : in type_segment_to_extend)
		return boolean;
		
		
	-- Returns the cursor to a net segment that is
	-- to be extended:
	function get_segment (
		segment	: in type_segment_to_extend)
		return pac_net_segments.cursor;

		
	-- Retruns the end point (A/B) of a net segment
	-- that is to be extended:
	function get_end (
		segment	: in type_segment_to_extend)
		return type_start_end_point;

		
		

	-- Returns the first segment (among the given segments)
	-- and its end (A/B) which can be extended by the given segment.
	-- AB_end indicates which end of the given segment is to 
	-- be connected. A segment can only be extended if all those
	-- criteria are met:
	-- 1. It runs in the same direction as the given segment.
	-- 2. No ports exist at the attach point.
	-- 3. No other segments start or end at the attach point.
	-- If no suitable segment has been found, then the return is no_element:
	function get_segment_to_extend (
		segments	: in pac_net_segments.list;
		segment		: in type_net_segment;
		AB_end		: in type_start_end_point)
		return type_segment_to_extend;

	
	
	-- Attaches the given segment to the given strand.
	-- AB_end indicates which end of the segment is 
	-- to be connected.
	-- In case a junction is required, then the junction
	-- will be set on the given segment only:
	procedure attach_segment (
		strand			: in out type_strand;
		segment			: in type_net_segment;
		AB_end			: in type_start_end_point;
		log_threshold	: in type_log_level);



	package pac_strands is new doubly_linked_lists (type_strand);
	use pac_strands;



	-- Deletes the given segment in a given strand.
	-- The result can be:
	-- 1. just the given strand modified
	-- 2. two new strands because the given strand has been
	--    fallen apart into two separate strands:
	function delete_segment (
		strand			: in type_strand;
		segment			: in pac_net_segments.cursor;
		log_threshold	: in type_log_level)
		return pac_strands.list;

	
								 
	procedure set_proposed (
		strand : in out type_strand);

	
	procedure clear_proposed (
		strand : in out type_strand);


	function is_proposed (
		strand : in type_strand)
		return boolean;

	


	procedure set_selected (
		strand : in out type_strand);

	
	procedure clear_selected (
		strand : in out type_strand);


	function is_selected (
		strand : in type_strand)
		return boolean;




	procedure modify_status (
		strand		: in out type_strand;
		operation	: in type_status_operation);
	

	procedure reset_status (
		strand		: in out type_strand);


	
	

	-- Returns the sheet number where the given strand is on:
	function get_sheet (
		strand	: in type_strand)
		return type_sheet;


	-- Returns the (sheet/x/y) position of the given strand:
	function get_position (
		strand : in type_strand)
		return type_object_position;
	

	-- Returns the (sheet/x/y) position of the given strand
	-- as string:
	function get_position (
		strand : in type_strand)
		return string;



	-- Returns true if the given primary segment is movable
	-- at the given end. Queries also attached secondary
	-- segments:
	function is_movable (
		strand	: in type_strand;
		segment	: in pac_net_segments.cursor;
		AB_end	: in type_start_end_point)
		return boolean;



	
	
	

	-- Returns true if the given strand has segments:
	function has_segments (
		strand : in pac_strands.cursor)
		return boolean;


	
	function is_proposed (
		strand : in pac_strands.cursor)
		return boolean;

	
	function is_selected (
		strand : in pac_strands.cursor)
		return boolean;

	

	
	-- Returns the (sheet/x/y) position of the given strand:
	function get_position (
		strand : in pac_strands.cursor)
		return string;


	
	-- Iterates the strands. 
	-- Aborts the process when the proceed-flag goes false:
	procedure iterate (
		strands	: in pac_strands.list;
		process	: not null access procedure (position : in pac_strands.cursor);
		proceed	: not null access boolean);


	


	
	
	-- Returns a cursor to the segment that is
	-- on the lowest x/y position of the given strand:
	function get_first_segment (
		strand_cursor	: in pac_strands.cursor)
		return pac_net_segments.cursor;


	-- Returns the sheet number of the given strand:
	function get_sheet (
		strand_cursor	: in pac_strands.cursor)
		return type_sheet;
	
	
	-- Returns true if the given point is on the given
	-- net segment:
	function on_segment (
		segment_cursor	: in pac_net_segments.cursor;
		point			: in type_vector_model)
		return boolean;

	
	-- Returns true if the given point is on the given strand:
	function on_strand (
		strand_cursor	: in pac_strands.cursor;
		place			: in type_object_position)
		return boolean;
	




	

	
	package pac_strand_cursors is new doubly_linked_lists (pac_strands.cursor);



	
	type type_strand_segment_cursor is record
		strand_cursor	: pac_strands.cursor;
		segment_cursor	: pac_net_segments.cursor;
		AB_end			: type_start_end_point;
	end record;

	package pac_strand_segment_cursors is new 
		doubly_linked_lists (type_strand_segment_cursor);




	-- Breaks down a given primary segment in
	-- shorter fragments. The nodes are
	-- the places where other segments start or end
	-- between A and B of the primary segment:
	function split_segment (
		primary			: in type_net_segment;
		nodes			: in pac_strand_segment_cursors.list;
		log_threshold	: in type_log_level)
		return pac_net_segments.list;
	




	-- A stub of a net is modelled this way:
	type type_stub_direction is (
		LEFT,	-- dead end points to the left
		RIGHT,	-- dead end points to the right
		UP,		-- dead end points up
		DOWN);	-- dead end points down

	
	type type_stub (is_stub : boolean) is record
		case is_stub is
			when TRUE => direction : type_stub_direction;
			when FALSE => null;
		end case;
	end record;

	
	-- Maps from stub direction to rotation:
	function to_label_rotation (direction : in type_stub_direction)
		return type_rotation_model;

	
	-- Detects whether the given segment is a stub and if so
	-- detects the direction of the stub relative to the given point.
	-- If the segment is neither horizontal or vertical then it is NOT a stub.
	-- Examples: 
	-- - If point is right of a horizontal segment then then it is a stub that points to the right.
	-- - If point is above of a vertical segment then then it is a stub that points up.
	function stub_direction (
		segment	: in pac_net_segments.cursor;
		point	: in type_vector_model)
		return type_stub;
		

	
end et_net_strands;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

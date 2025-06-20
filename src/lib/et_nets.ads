------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                                NETS                                      --
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
with ada.containers.ordered_maps;

with et_assembly_variants;		use et_assembly_variants;
with et_module_instance;		use et_module_instance;
with et_schematic_coordinates;	use et_schematic_coordinates;
with et_logging;				use et_logging;
with et_net_names;				use et_net_names;
with et_net_class;				use et_net_class;
with et_netlists;
with et_pcb;
with et_commit;
with et_sheets;					use et_sheets;
with et_net_segment;			use et_net_segment;
with et_object_status;			use et_object_status;


package et_nets is

	use pac_geometry_2;
	use pac_net_name;

	use pac_net_segments;


	-- This type should be used whenever nets are counted.
	-- CS: Currently the maximum is a temporarily value and should
	-- be adjusted if required:
	type type_net_count is new natural range 0 .. 100_000;

	
	function to_string (
		net_count : in type_net_count)
		return string;
	

	




-- NETS (basic stuff):
	
	type type_net_scope is (
		STRAND,
		SHEET,
		EVERYWHERE
		);
	
	

	type type_net_base is tagged record
		route	: et_pcb.type_route; -- routing information -> pcb related

		-- The net class of the net: default, High_Voltage, EM/SI-critical, ...
		class 	: pac_net_class_name.bounded_string := net_class_name_default;
	end record;




	



-- STRANDS:

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

	
	
	package pac_strands is new doubly_linked_lists (type_strand);
	use pac_strands;
	

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


	
	
	-- Calculates and sets the lowest x/y position of the given strand.	
	-- Leaves the sheet number of the strand as it is.	
	procedure set_strand_position (strand : in out type_strand);


	
	
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
	



	
-- NETS:
	
	type type_net is new type_net_base with record
		strands		: pac_strands.list;
		scope		: et_netlists.type_net_scope := et_netlists.LOCAL;

		status : type_object_status; 
		-- IMPORTANT: status "moving" shall not be used. 
		-- CS: Use precondition when modifying the status ?
	end record;






	procedure set_proposed (
		net : in out type_net);

	
	procedure clear_proposed (
		net : in out type_net);


	function is_proposed (
		net : in type_net)
		return boolean;

	


	procedure set_selected (
		net : in out type_net);

	
	procedure clear_selected (
		net : in out type_net);


	function is_selected (
		net : in type_net)
		return boolean;



	procedure modify_status (
		net			: in out type_net;
		operation	: in type_status_operation);
	

	procedure reset_status (
		net			: in out type_net);



	-- Creates a new strand. Inserts the given segment
	-- in the strand. So a new strand with a single segment
	-- is created in the given net:
	procedure create_strand (
		net			: in out type_net;
		segment		: in type_net_segment);
	

	-- Returns true if the given net has strands:
	function has_strands (
		net : in type_net)
		return boolean;
	

		
	
	-- Returns the cursor to the strand at the given place.
	-- If no strand found then the return is no_element:
	function get_strand (
		net		: in type_net;
		place	: in type_object_position)
		return pac_strands.cursor;
	

	-- Returns a list of strands of the given net on the 
	-- given sheet:
	function get_strands (
		net		: in type_net;
		sheet	: in type_sheet)
		return pac_strands.list;


	
	package pac_strand_cursors is new doubly_linked_lists (pac_strands.cursor);

	-- Returns a list of cursors to strands that
	-- exist at the given place. 
	-- Usually strands do not cross each other (in a correct design).
	-- So in the result there should be only one item:
	function get_strands (
		net		: in type_net;
		place	: in type_object_position)
		-- CS log_threshold	: in type_log_level)
		return pac_strand_cursors.list;



	
	type type_strand_segment_cursor is record
		strand_cursor	: pac_strands.cursor;
		segment_cursor	: pac_net_segments.cursor;
		AB_end			: type_start_end_point;
	end record;

	package pac_strand_segment_cursors is new 
		doubly_linked_lists (type_strand_segment_cursor);


	-- Returns for a given primary net segment a list of strands
	-- having a segment that ends between A and B of the given segment:
	function get_strands (
		net				: in type_net;
		primary			: in type_net_segment;
		sheet			: in type_sheet;
		log_threshold	: in type_log_level)
		return pac_strand_segment_cursors.list;


	-- Breaks down a given primary segment in
	-- shorter fragments. The nodes are
	-- the places where other segments start or end
	-- between A and B of the primary segment:
	function split_segment (
		primary			: in type_net_segment;
		nodes			: in pac_strand_segment_cursors.list;
		log_threshold	: in type_log_level)
		return pac_net_segments.list;
	

	-- Deletes the given list of strands in the given net:
	procedure delete_strands (
		net		: in out type_net;
		strands	: in pac_strands.list);

	
	-- Appends a strand to the strands of net:
	procedure merge_strand (
		net		: in out type_net;
		strand	: in type_strand);
	

	-- Merges strands into the strands of net
	-- NOTE: The given list of strands will be cleared:
	procedure merge_strands (
		net		: in out type_net;
		strands	: in out pac_strands.list);
	
	
	-- Merges net_2 into net_1.
	-- NOTE: All strands, tracks, vias, fill-zones and cutout-areas 
	-- of net_2 are deleted.
	procedure merge_nets (
		net_1	: in out type_net;
		net_2	: in out type_net);



	-- Many nets are to be collected in maps:
	package pac_nets is new ordered_maps (
		key_type		=> pac_net_name.bounded_string, -- RESET_N
		element_type	=> type_net);

	use pac_nets;

	

	function is_proposed (
		net : in pac_nets.cursor)
		return boolean;


	function is_selected (
		net : in pac_nets.cursor)
		return boolean;



	
	-- Returns the name of the given net:
	function get_net_name (
		net_cursor : in pac_nets.cursor)
		return pac_net_name.bounded_string;
	

	-- Returns the name of the given net:
	function get_net_name (
		net_cursor : in pac_nets.cursor)
		return string;



	function net_exists (
		net_cursor : in pac_nets.cursor) 
		return boolean;

	
	
	-- Iterates the nets. Aborts the process when the proceed-flag goes false:
	procedure iterate (
		nets	: in pac_nets.map;
		process	: not null access procedure (position : in pac_nets.cursor);
		proceed	: not null access boolean);



	-- Returns the ports of devices, submodules and netchangers in
	-- the given net. The given assembly variant determines whether certain
	-- devices should be excluded (because they may not be present in a particular
	-- assembly variant).
	-- NOTE: If no variant is given, then the default variant is assumend
	-- and ALL devices are returned.
	function get_ports (
		net		: in pac_nets.cursor;
		variant	: in pac_assembly_variants.cursor := pac_assembly_variants.no_element)
		return type_ports;

	

	-- Returns true if the given net has strands:
	function has_strands (
		net : in pac_nets.cursor)
		return boolean;


	

	-- Returns a cursor to the strand that is
	-- on the lowest sheet and lowest x/y position:
	function get_first_strand (
		net_cursor	: in pac_nets.cursor)
		return pac_strands.cursor;

	

	-- Returns a cursor to the strand that is
	-- on the given sheet and has the lowest x/y position.
	-- Returns no_element if the given sheet does not
	-- contain a strand of the given net.
	function get_first_strand_on_sheet (
		sheet		: in type_sheet;
		net_cursor	: in pac_nets.cursor)
		return pac_strands.cursor;





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
		


	
-- NET INDEX:
	
	-- Nets may require to be indexed (for example with combo boxes)
	-- in a form like:
	--
	--     net       |  index
	-- ------------------------
	-- AGND          |      0
	-- analog_input  |      1
	-- digital_out   |      2
	-- gnd           |      3
	-- zero_pressure |    109
	--
	-- NOTE: The numbering starts at zero.
	
	-- We define a range for the net index. CS extend upper limit if required.
	type type_net_index is new natural range 0 .. 10_000;


	


	
-- COMMITS (required for undo/redo operations via the GUI):
	use et_commit;
	
	package pac_net_commit is new pac_commit (pac_nets.map);
	use pac_net_commit;
	
	package pac_net_commits is new doubly_linked_lists (
		element_type	=> pac_net_commit.type_commit);

	type type_nets_undo_redo_stack is record
		dos		: pac_net_commits.list;
		redos	: pac_net_commits.list;
	end record;
	
	
end et_nets;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

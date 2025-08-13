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
with et_net_ports;				use et_net_ports;
with et_net_segment;			use et_net_segment;
with et_net_strands;			use et_net_strands;
with et_net_labels;				use et_net_labels;
with et_object_status;			use et_object_status;


package et_nets is

	use pac_geometry_2;
	use pac_net_name;

	use pac_net_segments;
	use pac_strands;


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



	-- Creates a new strand on the given sheet.
	-- Inserts the given segment
	-- in the strand. So a new strand with a single segment
	-- is created in the given net:
	procedure create_strand (
		net		: in out type_net;
		sheet	: in type_sheet;						
		segment	: in type_net_segment);
	

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



	-- Returns a list of cursors to strands that
	-- exist at the given place. 
	-- Usually strands do not cross each other (in a correct design).
	-- So in the result there should be only one item:
	function get_strands (
		net		: in type_net;
		place	: in type_object_position)
		-- CS log_threshold	: in type_log_level)
		return pac_strand_cursors.list;





	-- Returns for a given primary net segment a list of strands
	-- having a segment that ends between A and B of the given segment:
	function get_strands (
		net				: in type_net;
		primary			: in type_net_segment;
		sheet			: in type_sheet;
		log_threshold	: in type_log_level)
		return pac_strand_segment_cursors.list;


	

	-- Deletes the given list of strands in the given net:
	procedure delete_strands (
		net		: in out type_net;
		strands	: in pac_strands.list);

	
	-- Adds a strand to the strands of net:
	procedure add_strand (
		net		: in out type_net;
		strand	: in type_strand);
	

	-- Merges strands into the strands of net
	-- NOTE: The given list of strands will be cleared:
	procedure add_strands (
		net		: in out type_net;
		strands	: in out pac_strands.list);



	-- Merges in the given net strand "source" in
	-- strand "target".
	-- See comments on procedure merge_strands above.
	-- Deletes the strand indicated by "source"
	-- so that "source" points to no_element afterward:
	procedure merge_strands (
		net				: in out type_net;
		target			: in pac_strands.cursor;
		source			: in out pac_strands.cursor;
		joint			: in type_strand_joint;
		log_threshold	: in type_log_level);


	
	
	-- Merges net source into net target.
	-- NOTE: All strands, tracks, vias, fill-zones and cutout-areas 
	-- of source are deleted.
	procedure merge_nets (
		target	: in out type_net;
		source	: in out type_net);



	-- Returns true if the given net has a segment that
	-- starts or ends at the given place:
	function has_end_point (
		net		: in type_net;
		place	: in type_object_position)
		return boolean;

	

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




	

-- OBJECT SEGMENT:


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





	


	
-- OBJECT STRAND:
	
	-- This composite type is meant to identify a strand
	-- and its parent net in the schematic:
	type type_object_strand is record
		net_cursor		: pac_nets.cursor;
		strand_cursor	: pac_strands.cursor;
	end record;


	-- Returns true if the cursors of the given
	-- strand object are empty (no_element):
	function is_empty (
		strand	: in type_object_strand)
		return boolean;

	

	function get_net_name (
		strand	: in type_object_strand)
		return pac_net_name.bounded_string;

	

	-- Returns the net name and strand position of the given object
	-- as string in the form like "GND strand sheet / start x/y end x/y":
	function to_string (
		object	: in type_object_strand)
		return string;



	-- Returns the actual strand as given by the cursor
	-- contained in argument "strand":
	function get_strand (
		strand : in type_object_strand)
		return type_strand;


	package pac_object_strands is new doubly_linked_lists (type_object_strand);


	-- Returns the net name of the given object strand:
	function get_net_name (
		strand	: in pac_object_strands.cursor)
		return pac_net_name.bounded_string;

	

	-- Returns the first strand among the given strands
	-- that belongs to the given net (indicated by net_cursor).
	-- If no suitable strand found, then the return is
	-- a type_object_strand with empty cursors:
	function get_strand (
		strands			: in pac_object_strands.list;
		net_cursor		: in pac_nets.cursor)
		return type_object_strand;

	



-- OBJECT NET:

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



	

-- OBJECT LABEL:

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

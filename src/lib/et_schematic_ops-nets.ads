------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                    SCHEMATIC OPERATIONS ON NETS                          --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2024                                                --
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
with et_pcb;


package et_schematic_ops.nets is


	-- Searches the module for an anonymous net with the lowest index available.
	-- Example: If the module contains nets like N$2, N$4, N$5 and N$101 then
	-- the lowest available name would be N$3.
	function lowest_available_anonymous_net (
		module		: in pac_generic_modules.cursor)
		return pac_net_name.bounded_string; -- N$3


	function net_exists (
		net_cursor : in pac_nets.cursor) 
		return boolean;

	
	type type_net_scope is (
		STRAND,
		SHEET,
		EVERYWHERE
		);

	-- Returns true if given point sits between start and end point of given segment.
	-- The zone is a means of reducing the accuracy. The greater the zone
	-- the greater can be the distance of point from the segment.
	function between_start_and_end_point (
		point 	: in type_vector_model;
		segment : in pac_net_segments.cursor;
		zone	: in type_accuracy := type_accuracy'first)
		return boolean;

	
	-- Returns true if given point sits on the given segment.
	-- The zone is a means of reducing the accuracy. The greater the zone
	-- the greater can be the distance of point from the segment.
	function on_segment (
		point 	: in type_vector_model;
		segment : in pac_net_segments.cursor;
		zone	: in type_accuracy := type_accuracy'first)
		return boolean;

	
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
		place			: in et_coordinates_2.type_position; -- sheet/x/y
		log_threshold	: in type_log_level);

	
	-- Deletes a net. The scope determines whether to delete a certain strand,
	-- all strands on a certain sheet or on all sheets.
	-- CS If a particular strand on a sheet is to be deleted, the argument "place"
	-- must provide sheet and x/y start position of strand. In the future x/y can be
	-- any point on any segment of the strand.
	procedure delete_net (
		module_cursor	: in pac_generic_modules.cursor;
		net_name		: in pac_net_name.bounded_string; -- RESET, MOTOR_ON_OFF
		scope			: in type_net_scope; -- strand, sheet, everywhere
		place			: in et_coordinates_2.type_position; -- sheet/x/y
		log_threshold	: in type_log_level);

	
	-- Deletes a segment of a net.
	procedure delete_segment (
		module_cursor	: in pac_generic_modules.cursor;
		net_name		: in pac_net_name.bounded_string; -- RESET, MOTOR_ON_OFF
		place			: in et_coordinates_2.type_position; -- sheet/x/y
		log_threshold	: in type_log_level);

	
	-- Returns true if the given record of ports is completely emtpty.
	function no_ports (ports : in type_ports) return boolean;

	
	-- Tests whether the zone of a net segment is movable.
	-- Returns true if movable, returns falso otherwise.
	function movable (
		module_cursor	: in pac_generic_modules.cursor;
		segment			: in type_net_segment;
		zone			: in type_line_zone;
		point_of_attack	: in et_coordinates_2.type_position;
		log_threshold	: in type_log_level) 
		return boolean;

	
	-- Moves the net labels of a segment.
	-- CS: Currently moves only the tag labels
	procedure move_net_labels (
		segment_before	: in type_net_segment;		-- the segment before the move
		segment_after	: in out type_net_segment;	-- the segment after the move
		zone			: in type_line_zone);		-- the zone being moved

	
	-- Drags a segment of a net.
	-- If the segment meets a port, then the port will be connected with the net.
	-- NOTE: If the segment meets another net, then these two nets will NOT be connected.
	--       CS: The resulting overlapping segments should be detected by the ERC.
	procedure drag_segment (
		module_cursor	: in pac_generic_modules.cursor;
		net_name		: in pac_net_name.bounded_string; -- RESET, MOTOR_ON_OFF
		point_of_attack	: in et_coordinates_2.type_position; -- sheet/x/y
		coordinates		: in type_coordinates; -- relative/absolute
		destination		: in type_vector_model; -- x/y
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
	subtype type_net_index is natural range 0 .. 10_000;
	
	-- Returns for the given net a unique index.
	-- If the given net has not been found, raises exception:
	function get_net_index (
		module_cursor	: in pac_generic_modules.cursor;
		net_name		: in pac_net_name.bounded_string;
		log_threshold	: in type_log_level)
		return type_net_index;
	
	
	-- Returns lists of nets that cross the given place.
	function get_nets_at_place (
		module_cursor	: in pac_generic_modules.cursor;
		place			: in et_coordinates_2.type_position;
		log_threshold	: in type_log_level)
		return pac_net_names.list;


	
	-- Inserts a net segment in the module.
	-- 1. If the start or end point of the new segment
	--    meets a port then the port will be connected with the segment.
	-- 2. If the segment_new collides with a foreign net, an error is raised.
	-- 3. If the net_name is a name of an already existing net, then the
	--    given net segment_new will be added to the existing net.
	--    net_cursor must point to the existing net.
	--    A junction will automatically be placed where the new segment 
	--    meets the existing net.
	-- 4. If net_cursor equals no_element then a new net named after 
	--    net_name will be created.
	-- 5. After this procedure net_cursor points to the net that has just
	--    been created or extended by segment_new.
	procedure insert_segment (
		module_cursor	: in pac_generic_modules.cursor;
		net_cursor		: in out pac_nets.cursor;
		sheet			: in type_sheet;
		net_name		: in pac_net_name.bounded_string;
		segment_new		: in type_net_segment;
		log_threshold	: in type_log_level);

	
	-- See description for procedure insert_segment.
	procedure insert_net (
		module_cursor	: in pac_generic_modules.cursor;
		net_name		: in pac_net_name.bounded_string; -- RESET, MOTOR_ON_OFF
		start_point		: in et_coordinates_2.type_position; -- sheet/x/y
		end_point		: in type_vector_model; -- x/y
		log_threshold	: in type_log_level);


	-- Sets the net class of a net:
	procedure set_net_class (
		module_cursor	: in pac_generic_modules.cursor;
		net_name		: in pac_net_name.bounded_string; -- RESET, MOTOR_ON_OFF
		net_class		: in et_pcb.pac_net_class_name.bounded_string; -- pwr
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
		place			: in et_coordinates_2.type_position; -- sheet/x/y, rotation doesn't matter
		log_threshold	: in type_log_level);

-- CS
-- 	procedure place_junction (
-- 	-- Places a net junction at the given position.
-- 	-- If the junction is to be placed between start and end point of a segment, then the segment 
-- 	-- is split in two new segments with the junction between them.
-- 	-- If there is no net segment at the given position, no junction is placed and warning issued.
-- 		module_cursor	: in pac_generic_modules.cursor;
-- 		place			: in et_coordinates_2.type_position; -- sheet/x/y, rotation doesn't matter
-- 		log_threshold	: in type_log_level);

	
	-- Places a label next to a segment at position.
	procedure place_net_label (
		module_cursor	: in pac_generic_modules.cursor;

		-- The reference point at the segment:
		segment_position: in et_coordinates_2.type_position; -- sheet/x/y

		-- The position of the label relative to segment_position:
		label_position	: in type_vector_model := origin; -- x/y

		-- The rotation is relevant for simple labels only. The label will always be placed
		-- so that is is readable from the front or from the right.
		-- Whatever angle you provide here, the final roation will be either 0 or 90 degrees.
		-- If the labe is a tag then this parameter has no meaning. The rotation of a tag
		-- label will be automatically determined by the direction of the stub it will be attached to.
		rotation		: in et_coordinates_2.type_rotation_model := zero_rotation;
		
		appearance 		: in type_net_label_appearance; -- simple/tag label		

		-- The direction is relevant for tag labels only:
		direction		: in type_net_label_direction; -- INPUT, OUTPUT, PASSIVE, ...
		log_threshold	: in type_log_level);

	
	-- Deletes a label.
	procedure delete_net_label (
		module_cursor	: in pac_generic_modules.cursor;
		position		: in et_coordinates_2.type_position; -- sheet/x/y
		log_threshold	: in type_log_level);

	-- CS procedure move_net_label

	
	-- Queries the position of the given net. If a stub is at the
	-- given position returns the direction of the stub.
	function query_stub (
		module_cursor	: in pac_generic_modules.cursor;
		net_name		: in pac_net_name.bounded_string; -- RESET, MOTOR_ON_OFF
		position		: in et_coordinates_2.type_position; -- sheet/x/y
		log_threshold	: in type_log_level)
		return type_stub;

	
end et_schematic_ops.nets;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

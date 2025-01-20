------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                BOARD OPERATIONS / ASSEMBLY DOCUMENTATION                 --
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

with ada.containers; 			use ada.containers;
with ada.containers.indefinite_doubly_linked_lists;


with et_text;
with et_assy_doc;					use et_assy_doc;
with et_assy_doc.boards;			use et_assy_doc.boards;

package et_board_ops.assy_doc is


	-- CS rework procedures so that a module cursor
	-- is used instead the module_name.
	
	use et_board_shapes_and_text;
	use pac_text_board;


	-- Draws a line in the assembly documentation.
	procedure draw_line (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		line			: in type_doc_line;
		log_threshold	: in type_log_level);


	-- Returns all lines in the vicinity of the given point:
	function get_lines (
		module_cursor	: in pac_generic_modules.cursor;
		face			: in type_face;
		point			: in type_vector_model;
		zone			: in type_accuracy; -- the circular area around the place
		log_threshold	: in type_log_level)
		return pac_doc_lines.list;
						   



	

	-- This composite type is required to distinguish
	-- between top and bottom lines when lines are searched for:
	type type_object_line is record
		face	: type_face := TOP;
		cursor	: pac_doc_lines.cursor := pac_doc_lines.no_element;
	end record;

	-- CS same for arcs and circles




	
	
	-- Modifies the status flag of a line (see package et_object_status):
	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		line			: in type_object_line;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level);


	
	-- Sets the proposed-flag of all lines which are
	-- in the given zone around the given place.
	procedure propose_lines (
		module_cursor	: in pac_generic_modules.cursor;
		point			: in type_vector_model; -- x/y
		face			: in type_face;
		zone			: in type_accuracy; -- the circular area around the place
		count			: in out natural; -- the number of affected lines
		log_threshold	: in type_log_level);


	
	-- Clears the proposed-flag and the selected-flag of all lines:
	procedure reset_proposed_lines (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level);

	
	
	-- Returns the first line according to the given flag.
	-- If no line has been found, then the return is 
	-- TOP and no_element:
	function get_first_line (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;								 
		log_threshold	: in type_log_level)
		return type_object_line;


	-- Advances to the next proposed line, starting at
	-- the given line. Traverses through the lines
	-- in a circular manner. If there are no
	-- proposed lines, then line assumes default value (no_element).
	-- If there is only one proposed line, then line is unchanged.
	-- CS last_item indicates that the last line has been reached.
	-- CS Currently this procedure is not used.
	procedure next_proposed_line (
		module_cursor	: in pac_generic_modules.cursor;
		line			: in out type_object_line;
		-- CS last_item		: in out boolean;
		log_threshold	: in type_log_level);

	

	
	procedure move_line (
		module_cursor	: in pac_generic_modules.cursor;
		face			: in type_face;
		line			: in type_doc_line;
		point_of_attack	: in type_vector_model;
		-- coordinates		: in type_coordinates; -- relative/absolute
		destination		: in type_vector_model;
		log_threshold	: in type_log_level);


	
	-- Draws an arc in the assembly documentation.
	procedure draw_arc (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		arc				: in type_doc_arc;
		log_threshold	: in type_log_level);

	
	-- Draws a circle in the assembly documentation.
	procedure draw_circle (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;	
		circle			: in type_doc_circle;
		log_threshold	: in type_log_level);


	-- Draws a zone in the assembly documentation layer.
	-- The given zone can consist of a single segment or a
	-- fragment of a zone contour.
	-- 1. If the given zone is a single segment or a fragment
	--    then the procedure serches for already existing zones
	--    which are incomplete (or open) and tries to append or prepend
	--    the given zone to one of the existing open zones.
	-- 2. If this attempt fails, then the given zone is regarded as 
	--    a new zone.
	-- 3. If all existing zones are already closed, then the given zone
	--    is regarded a a new zone and added to the existing zones.
	procedure draw_zone (
		module_cursor	: in pac_generic_modules.cursor;
		zone			: in type_doc_contour;
		face			: in type_face;
		log_threshold	: in type_log_level);




	
	-- This composite type helps to identify a
	-- contour segment by its zone and face:
	type type_object_segment is record
		face	: type_face := TOP;
		zone	: pac_doc_contours.cursor;
		segment	: pac_contours.pac_segments.cursor;
	end record;


	
	-- Modifies the status flag of a zone segment (see package et_object_status):
	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		segment			: in type_object_segment;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level);
	

	-- Sets the proposed-flag of all line and arc segments 
	-- of a zone which are
	-- in the given zone around the given place.
	procedure propose_segments (
		module_cursor	: in pac_generic_modules.cursor;
		point			: in type_vector_model; -- x/y
		zone			: in type_accuracy; -- the circular area around the place
		face			: in type_face;
		count			: in out natural; -- the number of affected segments
		log_threshold	: in type_log_level);


	-- Clears the proposed-flag and the selected-flag 
	-- of all line and arc segments:
	procedure reset_proposed_segments (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level);



	
	
	-- Returns the first line or arc segment according to the given flag.
	-- If no segment has been found, then the return is no_element:
	function get_first_segment (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;								 
		log_threshold	: in type_log_level)
		return type_object_segment;


	

	-- Advances to the next proposed line or arc segment, starting at
	-- the given segment. Traverses through the segments
	-- in a circular manner. If there are no
	-- proposed segments, then segment assumes default value (no_element).
	-- If there is only one proposed segment, then segment is unchanged.
	-- CS last_item indicates that the last segment has been reached.
	-- CS Currently this procedure is not used. 
	procedure next_proposed_segment (
		module_cursor	: in pac_generic_modules.cursor;
		segment			: in out type_object_segment;
		-- CS last_item		: in out boolean;
		log_threshold	: in type_log_level);


	
	-- Moves a line or arc segment of a zone:
	-- CS currently it moves only a single segment.
	-- CS provide parameter for move mode (move attached segments, move whole contour)
	procedure move_segment (
		module_cursor	: in pac_generic_modules.cursor;
		segment			: in type_object_segment;
		point_of_attack	: in type_vector_model;
		-- coordinates		: in type_coordinates; -- relative/absolute
		destination		: in type_vector_model;
		log_threshold	: in type_log_level);




	

	-- When objects are handled then we need these
	-- categories in order to store them in indefinite_doubly_linked_lists:
	type type_object_category is (CAT_VOID, CAT_LINE, CAT_ZONE_SEGMENT);
	-- CS CAT_ARC, CAT_CIRCLE

	-- This type wraps segments of zones, lines, arcs and circles
	-- into a single type:
	type type_object (cat : type_object_category) is record
		case cat is
			when CAT_VOID			=> null;
			when CAT_ZONE_SEGMENT	=> segment	: type_object_segment;
			when CAT_LINE 			=> line 	: type_object_line;
		end case;
	end record;

	package pac_objects is new indefinite_doubly_linked_lists (type_object);




	

	-- Returns the first object (line, arc, circle, zone segment)
	-- according to the given flag.
	-- If nothing found, then the return is a void object (CAT_VOID):
	function get_first_object (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;								 
		log_threshold	: in type_log_level)
		return type_object;


	-- Collects all objects (lines, arcs, circles, zone segments)
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



	procedure move_object (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		point_of_attack	: in type_vector_model;
		-- coordinates		: in type_coordinates; -- relative/absolute
		destination		: in type_vector_model;
		log_threshold	: in type_log_level);



	

	
	
	-- Deletes the segment of the assembly documentation that crosses the given point.
	-- CS currently deletes the first segment found. Leaves other segments untouched.
	-- CS a parameter like "all" to delete all segments in the vicinity of point.
	procedure delete (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		point			: in type_vector_model; -- x/y
		accuracy		: in type_accuracy;
		log_threshold	: in type_log_level);


	-- Deletes the given line in the given module:
	procedure delete (
		module_cursor	: in pac_generic_modules.cursor;
		face			: in type_face;
		line			: in type_doc_line;
		log_threshold	: in type_log_level);




	
	
	-- Returns all texts in the vicinity of the given point:
	function get_texts (
		module_cursor	: in pac_generic_modules.cursor;
		face			: in type_face;
		point			: in type_vector_model;		
		zone			: in type_accuracy; -- the circular area around the place
		log_threshold	: in type_log_level)
		return pac_doc_texts.list;


	-- Moves a text:
	procedure move_text (
		module_cursor	: in pac_generic_modules.cursor;
		face			: in type_face;
		text			: in type_doc_text;
		coordinates		: in type_coordinates; -- relative/absolute
		point			: in type_vector_model;
		log_threshold	: in type_log_level);
	
end et_board_ops.assy_doc;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

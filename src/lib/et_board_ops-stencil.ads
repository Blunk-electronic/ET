------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                   BOARD OPERATIONS / STENCIL                             --
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

with ada.containers; 			use ada.containers;
with ada.containers.indefinite_doubly_linked_lists;


with et_stencil;				use et_stencil;

package et_board_ops.stencil is

	-- CS rework procedures so that a module cursor
	-- is used instead the module_name.

	use et_board_shapes_and_text;
	

-- LINES:
	
	-- Draws a line:
	procedure add_line (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		line			: in type_stencil_line;
		log_threshold	: in type_log_level);



	-- This composite type is required to distinguish
	-- between top and bottom lines when lines are searched for:
	type type_object_line is record
		face	: type_face := TOP;
		cursor	: pac_stencil_lines.cursor := pac_stencil_lines.no_element;
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
	-- Adds to count the number of lines that have been found:
	procedure propose_lines (
		module_cursor	: in pac_generic_modules.cursor;
		face			: in type_face;
		catch_zone		: in type_catch_zone;
		count			: in out natural;
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

	
	procedure move_line (
		module_cursor	: in pac_generic_modules.cursor;
		face			: in type_face;
		line			: in type_stencil_line;
		point_of_attack	: in type_vector_model;
		-- coordinates		: in type_coordinates; -- relative/absolute
		destination		: in type_vector_model;
		log_threshold	: in type_log_level);


	-- Deletes the given line in the given module:
	procedure delete_line (
		module_cursor	: in pac_generic_modules.cursor;
		face			: in type_face;
		line			: in type_stencil_line;
		log_threshold	: in type_log_level);


	
	
	
-- ARCS:
	
	-- Draws an arc:
	procedure add_arc (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		arc				: in type_stencil_arc;		
		log_threshold	: in type_log_level);


-- CIRCLES:
	
	-- Draws an circle;
	procedure add_circle (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		circle			: in type_stencil_circle;
		log_threshold	: in type_log_level);


	
-- ZONES:
	
	-- Draws a stencil zone.
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
	procedure draw_zone ( -- CS rename to add_zone
		module_cursor	: in pac_generic_modules.cursor;
		zone			: in type_stencil_zone;
		face			: in type_face;
		log_threshold	: in type_log_level);


	-- This composite type helps to identify a
	-- segment of a zone by its zone and face:
	type type_object_segment is record
		face	: type_face := TOP;
		zone	: pac_stencil_zones.cursor;
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
	-- Adds to count the number of segments that have been found:
	procedure propose_segments (
		module_cursor	: in pac_generic_modules.cursor;
		catch_zone		: in type_catch_zone;
		face			: in type_face;
		count			: in out natural;
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



	-- Deletes a line or arc segment of a zone:
	procedure delete_segment (
		module_cursor	: in pac_generic_modules.cursor;
		segment			: in type_object_segment;
		log_threshold	: in type_log_level);






-- OBJECTS:
	

	-- When objects are handled then we need these
	-- categories in order to store them in indefinite_doubly_linked_lists:
	type type_object_category is (
		CAT_VOID,
		CAT_LINE, 
		CAT_ZONE_SEGMENT
		);
	-- CS CAT_ARC, CAT_CIRCLE

	
	-- This type wraps segments of zones, lines, arcs, circles, 
	-- into a single type:
	type type_object (cat : type_object_category) is record
		case cat is
			when CAT_VOID => null;
			
			when CAT_ZONE_SEGMENT =>
				segment		: type_object_segment;
				
			when CAT_LINE => 
				line 		: type_object_line;
				
		end case;
	end record;

	package pac_objects is new indefinite_doubly_linked_lists (type_object);


	


	-- Returns the number of items stored in the given list:
	function get_count (
		objects : in pac_objects.list)
		return natural;
	

	

	-- Returns the first object (line, arc, circle, zone segment, text,
	-- placeholder) according to the given flag.
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



	-- This is a collective procedure that resets
	-- the proposed-flag and the selected-flag 
	-- of texts, lines, arcs, circles and zone segments:
	procedure reset_proposed_objects (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level);




	procedure delete_object (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		log_threshold	: in type_log_level);

	
	
	
	-- Deletes the object that crosses the given point.
	-- CS currently deletes the first item found. Leaves other objects untouched.
	-- CS a parameter like "all" to delete all objects in the vicinity of point.
	procedure delete_object (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		catch_zone		: in type_catch_zone;
		log_threshold	: in type_log_level);

	
	
											
end et_board_ops.stencil;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

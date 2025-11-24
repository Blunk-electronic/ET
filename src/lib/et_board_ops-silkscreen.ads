------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                BOARD OPERATIONS / SILKSCREEN                             --
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


with et_text;
with et_silkscreen;					use et_silkscreen;
with et_pcb_placeholders;			use et_pcb_placeholders;


package et_board_ops.silkscreen is
	
	-- CS rework procedures so that a module cursor
	-- is used instead the module_name.
	
	use et_board_text;
	use pac_text_board;


-- LINES:

	-- Draws a line:
	procedure add_line (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		line			: in type_silk_line;
		log_threshold	: in type_log_level);


	-- Returns all lines in the vicinity of the given point:
	function get_lines (
		module_cursor	: in pac_generic_modules.cursor;
		face			: in type_face;
		catch_zone		: in type_catch_zone;
		log_threshold	: in type_log_level)
		return pac_silk_lines.list;



	-- This composite type is required to distinguish
	-- between top and bottom lines when lines are searched for:
	type type_object_line is record
		face	: type_face := TOP;
		cursor	: pac_silk_lines.cursor := pac_silk_lines.no_element;
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
		line			: in type_silk_line;
		point_of_attack	: in type_vector_model;
		-- coordinates		: in type_coordinates; -- relative/absolute
		destination		: in type_vector_model;
		log_threshold	: in type_log_level);


	-- Deletes the given line in the given module:
	procedure delete_line (
		module_cursor	: in pac_generic_modules.cursor;
		face			: in type_face;
		line			: in type_silk_line;
		log_threshold	: in type_log_level);



-- ARCS:
	
	
	-- Adds an arc to the silkscreen:
	procedure add_arc (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		arc				: in type_silk_arc;
		log_threshold	: in type_log_level);


	-- This composite type is required to distinguish
	-- between top and bottom arcs when arcs are searched for:
	type type_object_arc is record
		face	: type_face := TOP;
		cursor	: pac_silk_arcs.cursor := pac_silk_arcs.no_element;
	end record;


	
	-- Modifies the status flag of an arc (see package et_object_status):
	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		arc				: in type_object_arc;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level);


	
	-- Sets the proposed-flag of all arcs which are
	-- in the given zone around the given place.
	-- Adds to count the number of arcs that have been found:
	procedure propose_arcs (
		module_cursor	: in pac_generic_modules.cursor;
		face			: in type_face;
		catch_zone		: in type_catch_zone;
		count			: in out natural;
		log_threshold	: in type_log_level);


	-- Clears the proposed-flag and the selected-flag of all arcs:
	procedure reset_proposed_arcs (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level);



	-- Returns the first arc according to the given flag.
	-- If no arc has been found, then the return is 
	-- TOP and no_element:
	function get_first_arc (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;								 
		log_threshold	: in type_log_level)
		return type_object_arc;

	

	procedure move_arc (
		module_cursor	: in pac_generic_modules.cursor;
		face			: in type_face;
		arc				: in type_silk_arc;
		point_of_attack	: in type_vector_model;
		-- coordinates		: in type_coordinates; -- relative/absolute
		destination		: in type_vector_model;
		log_threshold	: in type_log_level);



	-- Deletes the given arc in the given module:
	procedure delete_arc (
		module_cursor	: in pac_generic_modules.cursor;
		face			: in type_face;
		arc				: in type_silk_arc;
		log_threshold	: in type_log_level);

	
	
-- CIRCLES:
	
	-- Adds a circle to the silkscreen:
	procedure add_circle (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;	
		circle			: in type_silk_circle;
		log_threshold	: in type_log_level);



-- ZONES:
	
	-- Adds a zone to the silkscreen layer.
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
	procedure add_zone (
		module_cursor	: in pac_generic_modules.cursor;
		zone			: in type_silk_zone;
		face			: in type_face;
		log_threshold	: in type_log_level);



	-- This composite type helps to identify a
	-- segment of a zone by its zone and face:
	type type_object_segment is record
		face	: type_face := TOP;
		zone	: pac_silk_zones.cursor;
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


		

	



-- TEXTS:

	-- Adds a text.
	-- The caller must take care for mirroring the text
	-- in case its at the bottom of the board:
	procedure add_text (
		module_cursor	: in pac_generic_modules.cursor;
		face			: in type_face; -- top/bottom
		text			: in type_text_fab_with_content;
		log_threshold	: in type_log_level);


	
	-- Returns all texts in the vicinity of the given point:
	function get_texts (
		module_cursor	: in pac_generic_modules.cursor;
		face			: in type_face;
		catch_zone		: in type_catch_zone;
		log_threshold	: in type_log_level)
		return pac_silk_texts.list;


	-- Moves a text:
	procedure move_text (
		module_cursor	: in pac_generic_modules.cursor;
		face			: in type_face;
		text			: in type_silk_text;
		coordinates		: in type_coordinates; -- relative/absolute
		point			: in type_vector_model;
		log_threshold	: in type_log_level);




	-- This composite type helps to identify a
	-- text by its face:
	type type_object_text is record
		face	: type_face := TOP;
		cursor	: pac_silk_texts.cursor := pac_silk_texts.no_element;
	end record;


	-- This procedure sets the status flag of the
	-- given text object:
	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		text			: in type_object_text;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level);


	-- Sets the proposed-flag of all texts which have their
	-- origin (or anchor point) in the given zone around the given place.
	-- Adds to count the number of texts that have been found:
	procedure propose_texts (
		module_cursor	: in pac_generic_modules.cursor;
		face			: in type_face;
		catch_zone		: in type_catch_zone;
		count			: in out natural;
		log_threshold	: in type_log_level);
	

	procedure move_text (
		module_cursor	: in pac_generic_modules.cursor;
		text			: in type_object_text;
		destination		: in type_vector_model;
		log_threshold	: in type_log_level);

	
	procedure delete_text (
		module_cursor	: in pac_generic_modules.cursor;
		text			: in type_object_text;
		log_threshold	: in type_log_level);


	function get_first_text (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;								 
		log_threshold	: in type_log_level)
		return type_object_text;


	-- Clears the proposed-flag and the selected-flag 
	-- of all texts:
	procedure reset_proposed_texts (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level);

	

	
	
-- TEXT PLACEHOLDERS:

	-- CS remove the following stuff. NO longer required ?
	
	-- Places a text placeholder.
	-- The caller must take care for mirroring the placeholder
	-- in case its at the bottom of the board:
	procedure add_placeholder (
		module_cursor	: in pac_generic_modules.cursor;
		placeholder		: in type_text_placeholder;
		face			: in type_face;
		log_threshold	: in type_log_level);

	
	-- CS
	-- move_placeholder via commandline

	-- This composite type helps to identify a
	-- placeholder by its face:
	type type_object_placeholder is record
		face	: type_face := TOP;
		cursor	: pac_text_placeholders.cursor := pac_text_placeholders.no_element;
	end record;

	
	-- This procedure sets the status flag of the
	-- given placeholder object:
	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		placeholder		: in type_object_placeholder;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level);

	
	-- Sets the proposed-flag of all placeholders which have their
	-- origin (or anchor point) in the given zone around the given place.
	-- Adds to count the number of placeholders that have been found:
	procedure propose_placeholders (
		module_cursor	: in pac_generic_modules.cursor;
		face			: in type_face;
		catch_zone		: in type_catch_zone;
		count			: in out natural;
		log_threshold	: in type_log_level);
	

	procedure move_placeholder (
		module_cursor	: in pac_generic_modules.cursor;
		placeholder		: in type_object_placeholder;
		destination		: in type_vector_model;
		log_threshold	: in type_log_level);

	
	procedure delete_placeholder (
		module_cursor	: in pac_generic_modules.cursor;
		placeholder		: in type_object_placeholder;
		log_threshold	: in type_log_level);

	
	function get_first_placeholder (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;								 
		log_threshold	: in type_log_level)
		return type_object_placeholder;
 

	-- Clears the proposed-flag and the selected-flag 
	-- of all placeholders:
	procedure reset_proposed_placeholders (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level);





-- OBJECTS:
	

	-- When objects are handled then we need these
	-- categories in order to store them in indefinite_doubly_linked_lists:
	type type_object_category is (
		CAT_VOID,
		CAT_LINE,
		CAT_ARC,
		CAT_ZONE_SEGMENT,
		CAT_TEXT,
		CAT_PLACEHOLDER
		);
	-- CS CAT_CIRCLE

	
	-- This type wraps segments of zones, lines, arcs, circles, 
	-- texts, placeholders into a single type:
	type type_object (cat : type_object_category) is record
		case cat is
			when CAT_VOID => null;
			
			when CAT_ZONE_SEGMENT =>
				segment		: type_object_segment;
				
			when CAT_LINE => 
				line 		: type_object_line;

			when CAT_ARC => 
				arc 		: type_object_arc;
				
			when CAT_TEXT =>
				text		: type_object_text;
				
			when CAT_PLACEHOLDER =>
				placeholder	: type_object_placeholder;
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
	procedure reset_proposed_objects ( -- CS rename to reset_status_objects
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level);

	
	
	procedure delete_object (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		log_threshold	: in type_log_level);
	
	

	-- Deletes the object in the zone around the given point.
	-- CS currently deletes the first segment found. Leaves other segments untouched.
	-- CS a parameter like "all" to delete all segments in the vicinity of point.
	procedure delete_object (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		catch_zone		: in type_catch_zone;
		log_threshold	: in type_log_level);


	
end et_board_ops.silkscreen;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

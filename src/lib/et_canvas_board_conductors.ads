------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      CANVAS BOARD / CONDUCTORS                           --
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
-- DESCRIPTION:
-- 

with ada.containers;   			       	use ada.containers;
with ada.containers.indefinite_doubly_linked_lists;

with gtk.box;							use gtk.box;

with et_primitive_objects;				use et_primitive_objects;
with et_pcb_sides;						use et_pcb_sides;
with et_pcb_coordinates_2;				use et_pcb_coordinates_2;
use et_pcb_coordinates_2.pac_geometry_2;

with et_canvas_tool;					use et_canvas_tool;
with et_canvas_messages;				use et_canvas_messages;

with et_board_shapes_and_text;			use et_board_shapes_and_text;

with et_pcb_stack;						use et_pcb_stack;
with et_conductor_segment.boards;		use et_conductor_segment.boards;
with et_canvas_board_lines;				use et_canvas_board_lines;


package et_canvas_board_conductors is


	-- This procedure is required in order to clarify
	-- which object among the proposed objects is meant.
	-- On every call of this procedure we advance from one
	-- proposed segment to the next in a circular manner
	-- and set it as "selected":
	procedure clarify_object;


	-- Locates objects in the vicinity of the given point
	-- and sets their proposed-flag.
	-- Only displayed layers are taken into account.
	-- Depending on how many objects have been found, the behaviour is:
	-- - If only one object found, then it is selected automatically.
	-- - If more than one object found, then clarification is requested.
	--   The first object of them is selected.
	procedure find_objects (
		point : in type_vector_model);

	
	
-- PLACING:

	-- see package et_canvas_board_lines
	

	

-- MOVE:

	status_move_object : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to move conductor object." 
		& status_hint_for_abort;

	
	procedure move_object (
		tool	: in type_tool;
		point	: in type_vector_model);				   



-- DELETE:

	status_delete_object : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to delete conductor object." 
		& status_hint_for_abort;
 
	
	procedure delete_object (
		point	: in type_vector_model);				   

	
	
end et_canvas_board_conductors;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         CANVAS BOARD OUTLINE                             --
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
--                                                                          --
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
-- IMPORTANT:
-- This is about drawing zones in layers like assy doc, silkscreen, stopmask,
-- stencil, keepout.
-- This is NOT about zones in conductor layers. 
-- See package et_canvas_board_zone_conductor.

with glib;								use glib;

with gtk.box;							use gtk.box;
with gtk.label;							use gtk.label;
with gtk.combo_box;						use gtk.combo_box;
with gtk.combo_box_text;				use gtk.combo_box_text;

with et_canvas_tool;					use et_canvas_tool;
with et_canvas_messages;				use et_canvas_messages;
with et_canvas_board_2;
with et_pcb_sides;						use et_pcb_sides;
with et_pcb_coordinates_2;				use et_pcb_coordinates_2;
use et_pcb_coordinates_2.pac_geometry_2;
use et_pcb_coordinates_2.pac_path_and_bend;

with et_board_layer_category;			use et_board_layer_category;



package et_canvas_board_outline is

	use et_canvas_board_2.pac_canvas;
	



	-- to be output in the status bar:
	status_draw_outline : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to draw board outline." 
		& status_hint_for_abort;



	-- Builds the final path. This procedure requires to be called twice:
	-- first time for the start and the second time for the end point of the path.
	-- The current bend style in preliminary_object.path is taken into account.
	-- The path may be started and finished with different tools. For example start
	-- with MOUSE and finish with KEYBOARD or vice versa.
	procedure make_path (
		tool	: in type_tool;
		point	: in type_vector_model);
	


	-- Outputs the selected line or arc segment in the status bar:
	procedure show_selected_segment (
		selected		: in pac_contours.pac_segments.cursor;
		clarification	: in boolean := false);

	

	-- On every call of this procedure we advance from one
	-- proposed segment to the next in a circular manner.
	procedure select_segment;


	-- Locates objects in the vicinity of the given point
	-- and sets their proposed-flag.
	-- Depending on how many objects have been found, the behaviour is:
	-- - If only one object found, then it is selected and 
	--   the flag preliminary_object.ready will be set.
	-- - If more than one object found, then clarification is requested.
	--   The first object of them is selected.
	procedure find_objects (
		point : in type_vector_model);



	

-- MOVE:

	status_move_object : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to move object in board outline." 
		& status_hint_for_abort;

	
	procedure move_object (
		tool	: in type_tool;
		point	: in type_vector_model);				   



-- DELETE:

	status_delete_object : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to delete object in board outline." 
		& status_hint_for_abort;

	
	procedure delete_object (
		point	: in type_vector_model);				   


	
	
end et_canvas_board_outline;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
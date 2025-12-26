------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         CANVAS BOARD TEXTS                               --
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

with gtk.text_view;

with et_canvas;
with et_canvas_tool;					use et_canvas_tool;
with et_canvas_messages;				use et_canvas_messages;

with et_pcb_sides;						use et_pcb_sides;
with et_board_coordinates;				use et_board_coordinates;
with et_board_geometry;					use et_board_geometry;

with et_text_content;					use et_text_content;
with et_text;							use et_text;
with et_alignment;						use et_alignment;
with et_board_text;						use et_board_text;
use et_board_text.pac_text_board;
use et_board_text.pac_text_board_vectorized;

with et_board_layer_category;			use et_board_layer_category;
with et_pcb_signal_layers;				use et_pcb_signal_layers;

with et_silkscreen;						use et_silkscreen;
with et_assy_doc;						use et_assy_doc;
with et_stopmask;						use et_stopmask;
with et_conductor_text.boards;			use et_conductor_text.boards;
with et_mirroring;						use et_mirroring;
with et_object_status;


package et_canvas_board_texts is

	use pac_geometry_2;
	
	
	-- This procedure creates a set of categories:
	procedure make_affected_layer_categories;

	
	
	

	-- Before placing, moving, deleting or other operations we
	-- collect preliminary information using this type:
	type type_preliminary_text is record
		text : type_text_fab_with_content := (
			size		=> 10.0,
			line_width	=> 1.0,
			position	=> origin_zero_rotation,
			status		=> et_object_status.get_default_status,
			alignment	=> text_alignment_default,
			content		=> empty_text_content);
		
		-- NOTE: The content will be extracted from selector entry_content.
		entry_content	: gtk.text_view.gtk_text_view;
	end record;

	
	-- The place where preliminary information of
	-- a text is stored as the operator specifies them
	-- via the GUI (text properties box):
	preliminary_text : type_preliminary_text;


	-- Resets parts of the preliminary text.
	-- CS currently nothing happens here:
	procedure reset_preliminary_text;

	

	-- Builds the box for the text properties and
	-- inserts it below the console.
	-- If the box is already on display, nothing happens.
	procedure show_text_properties;



	
	
-- PLACING:

	-- to be output in the status bar:
	status_place_text : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to place text." 
		& status_hint_for_abort;


	-- Builds the final text-to-be-placed from the information
	-- provided by preliminary_text.
	-- Places the text at the given point.
	-- This procedure is called when the operator does a
	-- left mouse click or if he presses the space key
	-- when finishing the text-placing operation
	-- (see et_canvas_board_2-key_pressed and et_canvas_board_2.button_pressed):
	procedure place_text (
		point : in type_vector_model);

	
	
end et_canvas_board_texts;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

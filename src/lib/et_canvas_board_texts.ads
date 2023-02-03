------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         CANVAS BOARD TEXTS                               --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2021 Mario Blunk, Blunk electronic          --
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

with gtk.window;					use gtk.window;
with gtk.box;						use gtk.box;
with gtk.text_view;					--use gtk.text_view;

with et_general;					use et_general;
with et_canvas_general;				use et_canvas_general;
with et_canvas_schematic;

with et_pcb_coordinates;			use et_pcb_coordinates;
use et_pcb_coordinates.pac_geometry_2;

with et_text;						use et_text;

with et_board_shapes_and_text;		use et_board_shapes_and_text;
use et_board_shapes_and_text.pac_text_board;

with et_pcb_stack;					use et_pcb_stack;
with et_packages;					use et_packages;
with et_project.modules;			use et_project.modules;
with et_pcb;
with et_string_processing;			use et_string_processing;

package et_canvas_board_texts is


	-- The text properties bar:
	type type_box_properties is record
		box_main	: gtk_hbox;
		
		-- This flag indicates that the
		-- box is being displayed. 
		-- The purpose of this flag is
		-- to prevent the box from being drawn
		-- multiple times:
		displayed	: boolean := false;
	end record;

	box_properties : type_box_properties;


	

	-- Before placing, moving, deleting or other operations we
	-- collect preliminary information using this type:
	type type_preliminary_text is record
		ready		: boolean := false;
		
		category		: type_layer_category := type_layer_category'first;
		signal_layer	: type_signal_layer := signal_layer_default;
		face			: type_face := face_default;

		text			: type_text_fab_with_content := (
					size		=> 10.0,
					line_width	=> 1.0,
					position	=> origin_zero_rotation,
					alignment	=> text_alignment_default,
					content		=> empty_text_content);
		
		-- NOTE: The content will be extracted from selector entry_content.
		entry_content	: gtk.text_view.gtk_text_view;
	end record;

	-- The place where preliminary information of
	-- a text is stored:
	preliminary_text : type_preliminary_text;


	-- Clears preliminary_text.ready and box_properties.displayed.
	-- Removes the text properties bar.
	procedure reset_preliminary_text;

	
	-- Calls reset_preliminary_text if the verb is not VERB_PLACE.
	procedure remove_text_properties;
	

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
	-- Places the text at the given point:
	procedure place_text (
		point : in type_point);

	
	
	
end et_canvas_board_texts;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

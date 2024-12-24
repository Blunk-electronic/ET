------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         CANVAS BOARD LINES                               --
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
-- This is about drawing lines in assy doc, silkscreen, stop mask, stencil
-- and freetracks in conductor layers.
-- Conductor segments connected with a net are handled in et_canvas_board_tracks.

with glib;								use glib;

with gtk.box;							use gtk.box;
with gtk.label;							use gtk.label;
with gtk.combo_box;						use gtk.combo_box;
with gtk.combo_box_text;				use gtk.combo_box_text;
with gtk.button;						use gtk.button;

with et_canvas_tool;					use et_canvas_tool;
with et_canvas_messages;				use et_canvas_messages;
with et_canvas_board_2;
with et_pcb_sides;						use et_pcb_sides;
with et_pcb_coordinates_2;				use et_pcb_coordinates_2;
use et_pcb_coordinates_2.pac_geometry_2;
use et_pcb_coordinates_2.pac_path_and_bend;

with et_board_layer_category;			use et_board_layer_category;
with et_pcb_stack;						use et_pcb_stack;




package et_canvas_board_lines is

	use et_canvas_board_2.pac_canvas;


	-- Lines can be drawn in various layer categories.
	-- For the combo_box that offers the categories, the
	-- affected layers must be put together.
	-- This procedure creates a set of categories:
	procedure make_affected_layer_categories;

	

	-- The text properties bar:
	type type_box_properties is record
		
		-- This flag indicates that the
		-- box is being displayed. 
		-- The purpose of this flag is
		-- to prevent the box from being drawn
		-- multiple times:
		displayed	: boolean := false;
	end record;

	box_properties : type_box_properties;




	box_layer_category, box_face, 
	box_signal_layer, --box_button,
	box_line_width : gtk_vbox;
	
	label_layer_category, label_face, 
	label_signal_layer, label_line_width : gtk_label;
	
	cbox_category, cbox_face, cbox_signal_layer : gtk_combo_box;
	-- Operator can choose between fixed menu entries.
	
	cbox_line_width : gtk_combo_box_text;
	-- Operator may enter an additional value in the menu.
	
	-- button_apply : gtk_button;

	-- These constants define the minimum and maximum of
	-- characters that can be entered in the fields for 
	-- text size and line width:
	text_size_length_min : constant gint := 1;
	text_size_length_max : constant gint := 6; 
	-- CS: adjust if necessary. see parameters 
	-- of et_board_shapes_and_text.pac_text_fab.
	
	line_width_length_min : constant gint := 1;
	line_width_length_max : constant gint := 5;
	-- CS: adjust if necessary. see parameters
	-- of et_board_shapes_and_text.pac_text_fab.
	
	rotation_length_min : constant gint := 1;
	rotation_length_max : constant gint := 5;
	-- CS: adjust if necessary. see et_pcb_coordinates type_rotation.
	
	-- The spacing between the boxes:
	spacing : constant natural := 5;




	procedure remove_properties_bar;
	
	

	procedure show_line_properties;


	-- to be output in the status bar:
	status_draw_line : constant string := 
		status_click_left 
		& "or "
		& status_press_space
		& "to draw line." 
		& status_hint_for_abort;



	-- Builds the final path. This procedure requires to be called twice:
	-- first time for the start and the second time for the end point of the path.
	-- The current bend style in preliminary_line.path is taken into account.
	-- The path may be started and finished with different tools. For example start
	-- with MOUSE and finish with KEYBOARD or vice versa.
	procedure make_path (
		tool	: in type_tool;
		point	: in type_vector_model);
	

	
end et_canvas_board_lines;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

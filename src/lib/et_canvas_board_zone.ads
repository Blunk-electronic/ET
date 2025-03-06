------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         CANVAS BOARD ZONE                                --
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
with et_pcb_stack;						use et_pcb_stack;



package et_canvas_board_zone is

	use et_canvas_board_2.pac_canvas;
	

	-- Zones can be drawn in various layer categories.
	-- For the combo_box that offers the categories, the
	-- affected layers must be put together.
	-- This procedure creates a set of categories:
	procedure make_affected_layer_categories;





	box_layer_category, box_face, box_signal_layer : gtk_vbox;
	
	label_layer_category, label_face, label_signal_layer : gtk_label;
	
	cbox_category, cbox_face, cbox_signal_layer : gtk_combo_box;
	-- Operator can choose between fixed menu entries.
	

	
	-- The spacing between the boxes:
	spacing : constant natural := 5;



	

	procedure show_zone_properties;


end et_canvas_board_zone;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

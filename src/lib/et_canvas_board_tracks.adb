------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         CANVAS BOARD TRACKS                              --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2022 Mario Blunk, Blunk electronic          --
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

with ada.text_io;					use ada.text_io;
with ada.strings;					use ada.strings;
with ada.strings.fixed; 			use ada.strings.fixed;
--with ada.characters;				use ada.characters;
--with ada.characters.handling;		use ada.characters.handling;

with glib;
with glib.values;

with gdk.types;						use gdk.types;
with gdk.event;						use gdk.event;
with gdk.types.keysyms;				use gdk.types.keysyms;

with gtk.window;					use gtk.window;
with gtk.widget;					use gtk.widget;

with gtk.combo_box;					use gtk.combo_box;
with gtk.cell_renderer_text;		
with gtk.cell_layout;        		
with gtk.list_store;				
with gtk.tree_model;

with gtk.combo_box_text;			use gtk.combo_box_text;
with gtk.label;
with gtk.gentry;					use gtk.gentry;
with gtk.container;					use gtk.container;
with gtk.button;					use gtk.button;

with et_project.modules;				use et_project.modules;
with et_canvas_board;

with et_board_ops;						use et_board_ops;
with et_board_ops.conductors;
with et_modes.board;

with et_display;						use et_display;
with et_display.board;					use et_display.board;

with et_logging;						use et_logging;
with et_string_processing;				use et_string_processing;
with et_exceptions;						use et_exceptions;


package body et_canvas_board_tracks is

	use et_canvas_board.pac_canvas;


	function to_string (
		mode	: in type_snap_mode)
		return string
	is begin
		return " " & type_snap_mode'image (mode);
	end to_string;


	
	procedure reset_preliminary_track is begin
		preliminary_track.ready := false;
		preliminary_track.tool := MOUSE;

		-- Remove the text properties bar from the window:
		if box_properties.displayed then
			remove (box_right, box_properties.box_main);
			box_properties.displayed := false;
		end if;
	end reset_preliminary_track;



	
	
	procedure signal_layer_changed (combo : access gtk_combo_box_record'class) is
		use glib;
		use gtk.tree_model;
		use gtk.list_store;

		-- Get the model and active iter from the combo box:
		model : constant gtk_tree_model := combo.get_model;
		iter : constant gtk_tree_iter := combo.get_active_iter;

		item_text : glib.values.gvalue;
	begin
		-- Get the actual text of the entry (column is 0):
		gtk.tree_model.get_value (model, iter, 0, item_text);

		preliminary_track.signal_layer := to_signal_layer (glib.values.get_string (item_text));
		--put_line ("signal layer " & to_string (preliminary_track.signal_layer));

		-- display the affected conductor layer:
		enable_conductor (preliminary_track.signal_layer);
		
		et_canvas_board.redraw_board;		
	end signal_layer_changed;

	


	
	
	procedure apply_line_width (text : in string) is
		width : type_distance_positive;
	begin
		width := to_distance (text);

		-- CS validate. output error in status bar
		preliminary_track.width := width;

		et_canvas_board.redraw_board;
	end apply_line_width;

	
	
	function line_width_key_pressed (
		combo_entry	: access gtk_widget_record'class;
		event		: gdk_event_key) 
		return boolean 
	is
		event_handled : boolean := false;
		
		use gdk.types;
		key : gdk_key_type := event.keyval;

		gentry : gtk_gentry := gtk_gentry (combo_entry);
		text : constant string := get_text (gentry);
	begin
		case key is
			when GDK_ESCAPE =>
				reset_preliminary_track;

			when GDK_TAB => 
				--put_line ("line width via tab " & text);
				apply_line_width (text);

			when others => nulL;
		end case;
		
		return event_handled;
	end line_width_key_pressed;

	
	procedure line_width_entered (combo_entry : access gtk_entry_record'class) is 
		text : constant string := get_text (combo_entry);
	begin
		--put_line ("line width " & text);
		apply_line_width (text);
	end line_width_entered;




	procedure show_track_properties is
		use glib;

		use gtk.window;
		use gtk.box;
		use gtk.label;
		use gtk.gentry;
		use gtk.cell_renderer_text;
		use gtk.cell_layout;
		use gtk.list_store;
		use gtk.tree_model;

		box_signal_layer, --box_button,
		box_line_width : gtk_vbox;
		
		label_signal_layer, label_line_width : gtk_label;
		
		cbox_signal_layer : gtk_combo_box;
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

		
		procedure make_combo_for_signal_layer is
			storage_model : gtk_list_store;

			-- An entry consists of just a single column:
			column_0 : constant := 0;

			-- The single column is to contain strings:
			entry_structure : glib.gtype_array := (column_0 => glib.gtype_string);

			iter : gtk_tree_iter;			
			render : gtk_cell_renderer_text;
		begin
			gtk_new_vbox (box_signal_layer, homogeneous => false);
			pack_start (box_properties.box_main, box_signal_layer, padding => guint (spacing));
			
			gtk_new (label_signal_layer, "SIGNAL LAYER");
			pack_start (box_signal_layer, label_signal_layer, padding => guint (spacing));

			
			-- Create the storage model:
			gtk_new (list_store => storage_model, types => (entry_structure));

			-- Insert the available signal layers in the storage model:
			for choice in 
				-- The top layer is always available:
				type_signal_layer'first .. 

				-- The deepest available layer depends on the stack configuration:
				deepest_conductor_layer (current_active_module) 
			loop
				storage_model.append (iter);
				gtk.list_store.set (storage_model, iter, column_0,
					type_signal_layer'image (choice));
			end loop;

			-- Create the combo box:
			gtk.combo_box.gtk_new_with_model (
				combo_box	=> cbox_signal_layer,
				model		=> +storage_model); -- ?

			-- Set the signal layer used last:
			cbox_signal_layer.set_active (gint (preliminary_track.signal_layer) - 1);
			-- NOTE: The entries are numbered from 0 .. N.


			pack_start (box_signal_layer, cbox_signal_layer, padding => guint (spacing));
			cbox_signal_layer.on_changed (signal_layer_changed'access);

			-- The purpose of this stuff is unclear, but it
			-- is required to make the entries visible:
			gtk_new (render);
			pack_start (cbox_signal_layer, render, expand => true);
			add_attribute (cbox_signal_layer, render, "markup", column_0);

		end make_combo_for_signal_layer;


		procedure make_combo_for_line_width is begin
			gtk_new_vbox (box_line_width, homogeneous => false);
			pack_start (box_properties.box_main, box_line_width, padding => guint (spacing));

			gtk_new (label_line_width, "TRACK WIDTH");
			pack_start (box_line_width, label_line_width, padding => guint (spacing));

			gtk_new_with_entry (cbox_line_width);
			pack_start (box_line_width, cbox_line_width, padding => guint (spacing));
			gtk_entry (cbox_line_width.get_child).set_max_length (line_width_length_max);
			gtk_entry (cbox_line_width.get_child).set_width_chars (line_width_length_min);

			-- Set the line width according to the value used last:
			gtk_entry (cbox_line_width.get_child).set_text (trim (to_string (preliminary_track.width), left));
			
			-- The width is to be accepted by either pressing TAB or by pressing ENTER:
			gtk_entry (cbox_line_width.get_child).on_key_press_event (line_width_key_pressed'access);
			gtk_entry (cbox_line_width.get_child).on_activate (line_width_entered'access);
		end make_combo_for_line_width;

		
	begin -- show_track_properties
		
		-- If the box is already shown, do nothing.
		-- Otherwise build it:
		if not box_properties.displayed then
			box_properties.displayed := true;
		
			gtk_new_hbox (box_properties.box_main);
			pack_start (et_canvas_board.pac_canvas.box_right, box_properties.box_main,
						expand	=> false);

			-- The properties bar is to be displayed in the right box
			-- below the console:
			reorder_child (box_right, box_properties.box_main, 1);

			-- build the elements of the properties bar:
			make_combo_for_signal_layer;
			make_combo_for_line_width;
			-- make_apply_button;

			-- Redraw the right box of the window:
			box_right.show_all;
		end if;
		
	end show_track_properties;



	procedure next_snap_mode is 
		PT : type_preliminary_track renames preliminary_track;

		i : constant natural := type_snap_mode'pos (PT.snap_mode);
		-- i points now to the current snap mode

		-- get the index of the last available snap mode:
		max : constant natural := type_snap_mode'pos (type_snap_mode'last);
	begin
		if i < max then
			-- jump to next snap mode
			PT.snap_mode := type_snap_mode'succ (type_snap_mode'val (i));
		else 
			-- After the last snap mode, jump back to the first snap mode:
			PT.snap_mode := type_snap_mode'first;
		end if;

		--put_line ("snap mode " & to_string (PT.snap_mode));
	end next_snap_mode;
	

	procedure make_path (
		tool	: in type_tool;
		point	: in type_point)
	is
		PT : type_preliminary_track renames preliminary_track;
		line : type_line;

		procedure add_by_category is
		begin
			null; -- CS
		end;
		
	begin -- make_path
		put_line ("make path"); --to_string (PT.snap_mode));
		
		-- Set the tool being used for this path so that procedure
		-- draw_track (in et_canvas_board-draw_conductors)
		-- knows where to get the end point from.
		PT.tool := tool;

		-- Initally the preliminary_track is NOT ready. Nothing will be drawn.
		-- Upon the first calling of this procedure the start point of the
		-- path will be set.
		
		if not PT.ready then
			-- set start point:
			PT.path.start_point := point;

			-- Allow drawing of the path:
			preliminary_track.ready := true;

			set_status (status_start_point & to_string (PT.path.start_point) & ". " &
				status_press_space & status_set_end_point & status_hint_for_abort);

		else -- preliminary_track IS ready

			-- Start a new path only if the given point differs from 
			-- the start point of the current path:
			if point /= PT.path.start_point then

				-- Complete the path by setting its end point.
				-- The the current bend point (if there is one) into account:
				
				if PT.path.bended = NO then
					PT.path.end_point := point;

					-- insert a single line:
					line.start_point := PT.path.start_point;
					line.end_point   := PT.path.end_point;
					add_by_category;
					
				else
					-- The path is bended. The bend point has been computed
					-- interactively while moving the mouse or the cursor.
					-- See for example procedure draw_path in et_canvas_board-draw_assy_doc.

					-- insert first line of the path:
					line.start_point := PT.path.start_point;
					line.end_point   := PT.path.bend_point;
					add_by_category;

					
					-- insert second line of the path:
					PT.path.end_point := point;
					line.start_point := PT.path.bend_point;
					line.end_point   := PT.path.end_point;
					add_by_category;
				end if;

				-- Set start point of path so that a new
				-- path can be drawn:
				PT.path.start_point := point;
				
			else
				reset_preliminary_track;
			end if;
		end if;			
	end make_path;
		

	
end et_canvas_board_tracks;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

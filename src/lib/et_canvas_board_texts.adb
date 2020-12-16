------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         CANVAS BOARD TEXTS                               --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2020 Mario Blunk, Blunk electronic          --
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

with glib;

with gdk.types;						use gdk.types;
with gdk.event;						use gdk.event;
with gdk.types.keysyms;				use gdk.types.keysyms;


with gtk.widget;					use gtk.widget;
with gtk.box;
with gtk.combo_box;					use gtk.combo_box;
with gtk.combo_box_text;			use gtk.combo_box_text;
with gtk.label;
with gtk.gentry;
with gtk.text_view;					use gtk.text_view;
with gtk.button;					use gtk.button;
--with gtk.menu;
--with gtk.menu_item;
--with gtk.menu_shell;


package body et_canvas_board_texts is

	procedure close_window_place_text is begin
		window_place_text.window.destroy;
		window_place_text.open := false;
	end close_window_place_text;	
	
	function window_place_text_key_event (
		self	: access gtk_widget_record'class;
		event	: in gdk_event_key) 
		return boolean 
	is
		key : gdk_key_type := event.keyval;

		-- This is required in order to propagate the key-pressed event further.
		result : boolean; -- to be returned. Indicates that the event has been handled.
	begin
		case key is
			when GDK_ESCAPE =>
				--put_line ("key A");

				-- Close the window if operator hits ESC:
				close_window_place_text;
				result := true;

			when others =>
				--put_line ("key B");
				result := false;
		end case;
		
		return result;
	end window_place_text_key_event;


	
	procedure close_window_place_text (
		self	: access gtk_widget_record'class) 
	is begin
		window_place_text.open := false;
	end close_window_place_text;

	procedure layer_category_changed (self : access gtk_combo_box_record'class) is
	begin
		put_line ("cat");
		null;
	end layer_category_changed;
	
	procedure place_text is
		use gtk.window;
		use gtk.box;
		use gtk.label;
		use gtk.gentry;

		box_main : gtk_vbox;
		
		box_layer_category, box_face, box_content, box_button,
		box_size, box_line_width : gtk_hbox;
		
		label_layer_category, label_face, label_content,
		label_size, label_line_width : gtk_label;
		
		cbox_category, cbox_face : gtk_combo_box;
		cbox_line_width, cbox_size : gtk_combo_box_text;
		
		entry_content : gtk_text_view;
		button_ok : gtk_button;
		
		use glib;
		spacing : constant natural := 10;

		procedure insert_categories is
		begin
			cbox_category.prepend_text ("test");
		end insert_categories;
		
	begin
		-- If it is already up, move it to the foreground.
		-- Otherwise build it:
		if window_place_text.open then
			window_place_text.window.present;
		else
			window_place_text.open := true;
		
			gtk_new (window_place_text.window);
			window_place_text.window.set_title ("Set text properties");
			window_place_text.window.on_destroy (close_window_place_text'access);
			window_place_text.window.on_key_press_event (window_place_text_key_event'access);

			gtk_new_vbox (box_main, homogeneous => false);
			window_place_text.window.add (box_main);

			-- LAYER CAT
			gtk_new_hbox (box_layer_category, homogeneous => false);
			pack_start (box_main, box_layer_category, padding => guint (spacing));
			insert_categories;

			gtk_new (label_layer_category, "LAYER CAT");
			pack_start (box_layer_category, label_layer_category, padding => guint (spacing));

			gtk_new (cbox_category);
			pack_start (box_layer_category, cbox_category, padding => guint (spacing));
			--cbox_category.on_changed (layer_category_changed'access);
			
			-- FACE
			gtk_new_hbox (box_face, homogeneous => false);
			pack_start (box_main, box_face, padding => guint (spacing));

			gtk_new (label_face, "FACE");
			pack_start (box_face, label_face, padding => guint (spacing));

			gtk_new (cbox_face);
			pack_start (box_face, cbox_face, padding => guint (spacing));

			-- SIZE
			gtk_new_hbox (box_size, homogeneous => false);
			pack_start (box_main, box_size, padding => guint (spacing));

			gtk_new (label_size, "SIZE");
			pack_start (box_size, label_size, padding => guint (spacing));

			gtk_new (cbox_size);
			pack_start (box_size, cbox_size, padding => guint (spacing));

			-- LINE WIDTH
			gtk_new_hbox (box_line_width, homogeneous => false);
			pack_start (box_main, box_line_width, padding => guint (spacing));

			gtk_new (label_line_width, "LINE WIDTH");
			pack_start (box_line_width, label_line_width, padding => guint (spacing));

			gtk_new (cbox_line_width);
			pack_start (box_line_width, cbox_line_width, padding => guint (spacing));
			
			-- CONTENT
			gtk_new_hbox (box_content, homogeneous => false);
			pack_start (box_main, box_content, padding => guint (spacing));

			gtk_new (label_content, "CONTENT");
			pack_start (box_content, label_content, padding => guint (spacing));

			gtk_new (entry_content);
			pack_start (box_content, entry_content, padding => guint (spacing));

			-- OK BUTTON
			gtk_new_hbox (box_button, homogeneous => false);
			pack_start (box_main, box_button, padding => guint (spacing));

			gtk_new (button_ok, "OK");
			pack_start (box_button, button_ok, padding => guint (spacing));
			
			window_place_text.window.show_all;
		null;
		end if;
		
	end place_text;
	
end et_canvas_board_texts;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

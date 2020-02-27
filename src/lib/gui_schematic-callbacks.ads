------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                       GUI SCHEMATIC CALLBACKS                            --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2020 Mario Blunk, Blunk electronic          --
--                                                                          --
--    This program is free software: you can redistribute it and/or modify  --
--    it under the terms of the GNU General Public License as published by  --
--    the Free Software Foundation, either version 3 of the License, or     --
--    (at your option) any later version.                                   --
--                                                                          --
--    This program is distributed in the hope that it will be useful,       --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of        --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         --
--    GNU General Public License for more details.                          --
--                                                                          --
--    You should have received a copy of the GNU General Public License     --
--    along with this program.  If not, see <http://www.gnu.org/licenses/>. --
------------------------------------------------------------------------------

--   For correct displaying set tab width in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with gdk;				use gdk;
with gdk.event;			use gdk.event;

with glib;				use glib;
with gtk.widget;  		use gtk.widget;
with gtk.button;     	--use gtk.button;
with glib.object;		--use glib.object;
with gtk.gentry;
with gtk.combo_box;			with gtk.combo_box;
with gtk.combo_box_text;	with gtk.combo_box_text;	
with gtkada.style;		use gtkada.style;


package gui_schematic.callbacks is


	-- Terminates the main window:
	procedure terminate_main (self : access gtk_widget_record'class);

	-- Scales the canvas so that the frame fits into.
	function window_resized (
		self  : access gtk_widget_record'class;
		event : gdk.event.gdk_event_configure) 
		return boolean;
	
	procedure zoom_to_fit (self : access glib.object.gobject_record'class);	
	procedure zoom_in (self : access glib.object.gobject_record'class);
	procedure zoom_out (self : access glib.object.gobject_record'class);


	console : gtk_combo_box_text;
	procedure echo_command_simple (self : access gtk.gentry.gtk_entry_record'class);

	procedure echo_command (self : access gtk.combo_box.gtk_combo_box_record'class);
	
	function on_key_event (
		self	: access gtk_widget_record'class;
		event	: in gdk_event_key) 
		return boolean;


	
end gui_schematic.callbacks;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

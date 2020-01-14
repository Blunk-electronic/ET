------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                               CANVAS                                     --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2020 Mario Blunk, Blunk electronic          --
--                                                                          --
--         Bases on the package gtkada.canvas_view written by               --
--         E. Briot, J. Brobecker and A. Charlet, AdaCore                   --
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

-- Rationale: Aims to help users understanding programming with gtkada,
-- especially creating a canvas with items displayed on it.
-- The code is reduced to a minimum so that the newcomer is not overtaxed
-- and is concerned with only the most relevant code.
-- For the sake for simplicity we do not use abstract types, interfaces
-- or private types.

with gtk.main;
with gtk.window; 			use gtk.window;
with gtk.widget;  			use gtk.widget;
with gtk.box;				use gtk.box;
with gtk.button;     		use gtk.button;
with gtk.handlers;			use gtk.handlers;
with gtk.toolbar; 			use gtk.toolbar;
with gtk.tool_button;		use gtk.tool_button;
with gtk.enums;				use gtk.enums;
with gtk.gentry;			use gtk.gentry;
with gtk.combo_box_text;	use gtk.combo_box_text;
with gtk.frame;				use gtk.frame;
with gtk.scrolled_window;	use gtk.scrolled_window;
with gtk.adjustment;		use gtk.adjustment;

with gdk;
with gdk.types;

with glib;					use glib;
with glib.object;			use glib.object;
with glib.values;			use glib.values;
with cairo;					use cairo;
with cairo.pattern;			use cairo.pattern;
with gtkada.style;     		use gtkada.style;

with pango.layout;			use pango.layout;

with ada.containers;		use ada.containers;
with ada.containers.doubly_linked_lists;

with et_coordinates;

package et_canvas is


	-- This signal is emitted by the model whenever items are added, moved, resized, ...
	signal_layout_changed : constant glib.signal_name := "layout_changed";
	

	signal_viewport_changed : constant glib.signal_name := "viewport_changed";
	-- This signal is emitted whenever the view is zoomed or scrolled.


	-- The view coordinates are the 
	-- coordinates of items on the screen and are expressed in pixels.
	-- They change when the operators zooms or scrolls.
	subtype type_view_coordinate is gdouble; -- gdouble is a real floating-point type (see glib.ads)
	subtype type_view_coordinate_positive is type_view_coordinate range 0.0 .. type_view_coordinate'last;

	-- The point inside the view.
	type type_view_point is record
		x, y : type_view_coordinate;
	end record;

	function to_string (p : in type_view_point) return string;
	
	-- A rectangular regions of the view:
	type type_view_rectangle is record
		x, y			: type_view_coordinate;
		width, height	: type_view_coordinate_positive;
	end record;

	--  The number of blank pixels on each sides of the view. This avoids having
	--  items displays exactly next to the border of the view.
	view_margin : constant type_view_coordinate := 20.0;

end et_canvas;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

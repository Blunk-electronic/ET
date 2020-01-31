------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           CANVAS FOR BOARD                               --
--                                                                          --
--                               S p e c                                    --
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

-- with gtk.main;
-- with gtk.window; 			use gtk.window;
-- with gtk.widget;  			use gtk.widget;
-- with gtk.box;				use gtk.box;
-- with gtk.button;     		use gtk.button;
-- with gtk.handlers;			use gtk.handlers;
-- with gtk.toolbar; 			use gtk.toolbar;
-- with gtk.tool_button;		use gtk.tool_button;
-- with gtk.enums;				use gtk.enums;
-- with gtk.gentry;			use gtk.gentry;
-- with gtk.combo_box_text;	use gtk.combo_box_text;
-- with gtk.frame;				use gtk.frame;
-- with gtk.scrolled_window;	use gtk.scrolled_window;
-- with gtk.adjustment;		use gtk.adjustment;
-- 
-- with gdk;
-- with gdk.types;
-- 
-- with glib;					use glib;
-- with glib.object;			use glib.object;
-- with glib.values;			use glib.values;

with ada.text_io;				use ada.text_io;
-- with cairo;					use cairo;
-- with cairo.pattern;			use cairo.pattern;
-- with gtkada.style;     		use gtkada.style;

-- with pango.layout;			use pango.layout;

-- with ada.containers;		use ada.containers;
-- with ada.containers.doubly_linked_lists;

-- with et_pcb_coordinates;	use et_pcb_coordinates;
-- with et_project;			--use et_project;
-- with et_frames;				--use et_frames;

-- with et_canvas;

package body et_canvas_draw is

	
package body pac_draw is

	function convert_x (x : in pac_shapes.geometry.type_distance) return type_view_coordinate is begin
		return type_view_coordinate (x);
	end;

	function shift_y (
		height	: in pac_shapes.geometry.type_distance;
		offset	: in pac_shapes.geometry.type_distance)
		return type_view_coordinate is
	begin
		return type_view_coordinate (offset - height);
	end;
	
	procedure draw_line (
		context	: in type_draw_context;
		line	: in type_line'class;
		height	: in pac_shapes.geometry.type_distance) is

		boundaries : type_boundaries := pac_shapes.boundaries (line);
	begin
		save (context.cr);
		
		-- start point
		cairo.move_to (
			context.cr,
			convert_x (line.start_point.x),
			shift_y (line.start_point.y, height)
			);

		-- end point
		cairo.line_to (
			context.cr,
			convert_x (line.end_point.x),
			shift_y (line.end_point.y, height)
			);

		restore (context.cr);
	end draw_line;

	
end pac_draw;

end et_canvas_draw;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

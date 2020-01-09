------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                               CANVAS                                     --
--                                                                          --
--                               B o d y                                    --
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


with ada.text_io;			use ada.text_io;

with interfaces.c.strings;	use interfaces.c.strings;

with gtk.main;
with gtk.window; 			use gtk.window;
with gtk.widget;  			use gtk.widget;
with gtk.box;				use gtk.box;
with gtk.button;     		use gtk.button;
with gtk.toolbar; 			use gtk.toolbar;
with gtk.tool_button;		use gtk.tool_button;
with gtk.enums;				use gtk.enums;
with gtk.gentry;			use gtk.gentry;
with gtk.combo_box_text;	use gtk.combo_box_text;
with gtk.frame;				use gtk.frame;
with gtk.handlers;			use gtk.handlers;
with gtk.scrolled_window;	use gtk.scrolled_window;
with gtk.adjustment;		use gtk.adjustment;
with gtk.bin;				use gtk.bin;
with gtk.scrollable;		use gtk.scrollable;
with gtk.style_context;		use gtk.style_context;

with glib.properties.creation;	use glib.properties.creation;
with cairo;					use cairo;
with gtkada.types;			use gtkada.types;
with gtkada.handlers;		use gtkada.handlers;
with gtkada.bindings;		use gtkada.bindings;
with gdk;					use gdk;
with gdk.window;			use gdk.window;
with gdk.window_attr;		use gdk.window_attr;
with gdk.event;				use gdk.event;

with gdk.rgba;
with pango.layout;					use pango.layout;
with system.storage_elements;		use system.storage_elements;

with ada.unchecked_deallocation;
with ada.containers;				use ada.containers;
with ada.containers.doubly_linked_lists;

with et_general;
with et_project;
with et_frames;
with et_coordinates;			use et_coordinates;
use et_coordinates.geometry;


separate (et_canvas.schematic)
procedure draw_frame (
	model	: not null access type_model;
	in_area	: in type_model_rectangle := no_rectangle;
	context : in type_draw_context) is

	use et_frames;
	use pac_lines;

	-- Draw the line of the title block. The line is offset by the position of the
	-- title block. The y-cooordinate is converted to the y-axis going downwards.
	procedure draw_line (cursor : in pac_lines.cursor) is begin

		-- start point
		cairo.move_to 
			(
			context.cr,

			-- x position
			type_view_coordinate (
				element (cursor).start_point.x 
				+ model.title_block_position.x), -- x position of title block

			-- y position
			type_view_coordinate (
				model.frame_bounding_box.height -- height of the drawing frame
				- type_model_coordinate (
					element (cursor).start_point.y 
					+ model.title_block_position.y)) -- y position of title block
			);

		-- end point
		cairo.line_to 
			(
			context.cr,

			-- x position	
			type_view_coordinate (
				element (cursor).end_point.x 
				+ model.title_block_position.x), -- x position of title block

			-- y position
			type_view_coordinate (
				model.frame_bounding_box.height  -- height of the drawing frame 
				- type_model_coordinate (
					element (cursor).end_point.y 
					+ model.title_block_position.y)) -- y position of title block
			);
	end;
	
	
begin
--		put_line ("draw frame ...");

	if (in_area = no_rectangle)
		or else intersects (in_area, model.frame_bounding_box) 
	then
		-- CS test size 
-- 			if not size_above_threshold (self, context.view) then
-- 				return;
-- 			end if;
		
		save (context.cr);

		-- Prepare the current transformation matrix (CTM) so that
		-- all following drawing is relative to the upper left frame corner.
		translate (
			context.cr,
			type_view_coordinate (model.frame_bounding_box.x),
			type_view_coordinate (model.frame_bounding_box.y));

		cairo.set_line_width (context.cr, 1.0);

		cairo.set_source_rgb (context.cr, gdouble (1), gdouble (0), gdouble (0)); -- red

		-- draw the outer frame
		cairo.rectangle (
			context.cr,
			type_view_coordinate (0.0),
			type_view_coordinate (0.0),
			type_view_coordinate (model.frame.size.x),
			type_view_coordinate (model.frame.size.y));

		-- draw the inner frame
		cairo.rectangle (
			context.cr,
			type_view_coordinate (model.frame.border_width),
			type_view_coordinate (model.frame.border_width),
			type_view_coordinate (model.frame.size.x - 2 * model.frame.border_width),
			type_view_coordinate (model.frame.size.y - 2 * model.frame.border_width));

		-- draw the title block

		
		-- lines
		iterate (model.frame.title_block_schematic.lines, draw_line'access);
		
		-- CS draw the sector delimiters

		-- CS draw the sector rows and columns
		
		cairo.stroke (context.cr);
		
		restore (context.cr);
	end if;
end;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

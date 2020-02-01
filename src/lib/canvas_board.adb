------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           CANVAS FOR BOARD                               --
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
with gtk.accel_group;

with glib.properties.creation;	use glib.properties.creation;
with cairo;					use cairo;
with gtkada.types;			use gtkada.types;
with gtkada.handlers;		use gtkada.handlers;
with gtkada.bindings;		use gtkada.bindings;
with gdk;					use gdk;
with gdk.window;			use gdk.window;
with gdk.window_attr;		use gdk.window_attr;
with gdk.event;				use gdk.event;
with gdk.types;				--use gdk.types;
with gdk.types.keysyms;

with gdk.rgba;
with pango.layout;					use pango.layout;
with system.storage_elements;		use system.storage_elements;

with ada.unchecked_deallocation;
with ada.containers;				use ada.containers;
with ada.containers.doubly_linked_lists;

with et_general;
with et_project;
with et_frames;
-- with et_packages;
with et_pcb_coordinates;	use et_pcb_coordinates;
use et_pcb_coordinates.geometry;

-- with et_canvas_draw;

package body canvas_board is

-- 	-- In order to draw objects of a package we instantiate this package:
-- 	package pac_draw_package is new et_canvas_draw.pac_draw (
-- 		pac_canvas	=> canvas_board.pac_canvas,
-- 		pac_shapes	=> et_packages.shapes);


	

	
	function model_to_drawing (
		self		: not null access type_view;
		model_point : in type_point)	
		return type_point is 
		use et_general;
		p : type_point; -- to be returned
	begin
		set (point	=> p,
			 axis	=> X, 
			 value	=> model_point.x - self.drawing.frame_bounding_box.x);
		
		set (point	=> p,
			 axis	=> Y,
			 value	=> type_distance (self.drawing.frame.frame.size.y) 
						- model_point.y 
						+ self.drawing.frame_bounding_box.y);

		return p;
	end;

	function bounding_box (self : not null access type_view)
		return type_rectangle is
	begin
		return self.drawing.paper_bounding_box; -- CS should include all items of the current sheet.
		-- means: also items outside the frame
	end;


	
	procedure gtk_new (
		self	: out type_view_ptr) is
	begin
		self := new type_view;
		init (self);
	end;


	procedure draw_grid (
		self    : not null access type_view;
		style   : gtkada.style.drawing_style;
		context : type_draw_context;
		area    : type_rectangle) is separate;

	procedure draw_frame (
		self    : not null access type_view;
		in_area	: in type_rectangle := no_rectangle;
		context : in type_draw_context) is separate;

	procedure draw_outline (
		self    : not null access type_view;
		in_area	: in type_rectangle := no_rectangle;
		context : in type_draw_context) is separate;


	procedure draw_internal (
		self    : not null access type_view;
		context : type_draw_context;
		area    : type_rectangle) 
	is
		-- prepare draing style so that white grid dots will be drawn.
		style : drawing_style := gtk_new (stroke => gdk.rgba.white_rgba);
	begin
-- 		put_line ("draw internal ...");
		
		-- draw a black background:
		set_source_rgb (context.cr, 0.0, 0.0, 0.0);
		paint (context.cr);

		-- draw white grid dots:
		set_grid_size (self, pac_canvas.grid_default);
		draw_grid (self, style, context, area);

		draw_frame (self, area, context); -- separate unit
		draw_outline (self, area, context); -- separate unit
-- 		draw_units (self, area, context); -- separate unit
		-- CS self.model.draw_texts (area, context);
		-- CS self.model.draw_submodules (area, context);
			
	end draw_internal;



	procedure init_drawing (
		view	: in type_view_ptr;							 
		module	: in et_project.type_modules.cursor)
	is
		use et_general;
		use et_frames;
		use et_project;

		type type_local_view_ptr is access all type_view;
		self : type_local_view_ptr := type_local_view_ptr (view);
	begin
		self.drawing.module := module;

		-- set some variables frequently used regarding frame and paper:
		self.drawing.frame := type_modules.element (module).board.frame;
		
		self.drawing.paper_height := type_distance_positive (paper_dimension (
							paper_size	=> self.drawing.frame.frame.paper,
							orientation	=> self.drawing.frame.frame.orientation,
							axis		=> Y));

		self.drawing.paper_width := type_distance_positive (paper_dimension (
							paper_size	=> self.drawing.frame.frame.paper,
							orientation	=> self.drawing.frame.frame.orientation,
							axis		=> X));

		-- The drawing frame has a bounding box:

		-- position (upper left corner):
		self.drawing.frame_bounding_box.x := (self.drawing.paper_width - type_distance_positive (self.drawing.frame.frame.size.x)) / 2.0;
		self.drawing.frame_bounding_box.y := (self.drawing.paper_height - type_distance_positive (self.drawing.frame.frame.size.y)) / 2.0;

		-- width and height
		self.drawing.frame_bounding_box.width := type_distance_positive (self.drawing.frame.frame.size.x);
		self.drawing.frame_bounding_box.height := type_distance_positive (self.drawing.frame.frame.size.y);

		-- The sheet has a drawing box:
		self.drawing.paper_bounding_box := (0.0, 0.0, self.drawing.paper_width, self.drawing.paper_height);

		-- Drawing of the title block items is relative to the title block position:
		self.drawing.title_block_position := self.drawing.frame.frame.title_block_pcb.position;

	end init_drawing;


	function convert_and_shift_y (
		self	: not null access type_view;
		y		: in type_distance_total)
		return type_view_coordinate is 
	begin
		return type_view_coordinate 
			(
			self.drawing.frame_bounding_box.height 
			- y
			);
	end;
		
	function convert_and_shift_y (
		self	: not null access type_view;
		y		: in type_distance_total)
		return type_distance_total is 
	begin
		return (
			self.drawing.frame_bounding_box.height 
			- y
			);
	end;

	
end canvas_board;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

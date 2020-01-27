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
with et_coordinates;			use et_coordinates;
use et_coordinates.geometry;

package body et_canvas.schematic is
	
-- 	procedure gtk_new (self : out type_model_ptr_sch) is begin
-- 		self := new type_model_sch;
-- 		init (self);
-- 	end;	

-- 	procedure init (self : not null access type_model'class) is begin
-- 		if not self.is_created then
-- 			g_new (self, model_get_type);
-- 		end if;
-- 	end;

	
-- 	function get_scale (self : not null access type_view) return type_scale is
-- 	begin
-- 		return self.scale;
-- 	end get_scale;

	
-- 	procedure layout_changed (self : not null access type_model'class) is begin
-- 		object_callback.emit_by_name (self, signal_layout_changed);
-- 	end layout_changed;



	
-- CONVERSIONS BETWEEN COORDINATE SYSTEMS
	
-- 	function view_to_model (
-- 		self   : not null access type_view;
-- 		p      : in type_view_point) 
-- 		return type_model_point is
-- 	begin
-- 		return type_model_point (set (
-- 			x	=> type_model_coordinate (p.x / self.scale) + self.topleft.x,
-- 			y	=> type_model_coordinate (p.y / self.scale) + self.topleft.y
-- 			));
-- 	end view_to_model;
-- 
-- 	function view_to_model (
-- 		self   : not null access type_view;
-- 		rect   : in type_view_rectangle) -- position and size are in pixels
-- 		return type_model_rectangle is
-- 	begin
-- 		return (x      => type_model_coordinate (rect.x / self.scale) + self.topleft.x,
-- 				y      => type_model_coordinate (rect.y / self.scale) + self.topleft.y,
-- 				width  => type_model_coordinate (rect.width / self.scale),
-- 				height => type_model_coordinate (rect.height / self.scale));
-- 	end view_to_model;
-- 
-- 	
-- 	function model_to_view (
-- 		self   : not null access type_view;
-- 		p      : in type_model_point) 
-- 		return type_view_point is
-- 	begin
-- 		return (
-- 			x => type_view_coordinate (p.x - self.topleft.x) * self.scale,
-- 			y => type_view_coordinate (p.y - self.topleft.y) * self.scale
-- 			);
-- 	end model_to_view;
-- 
-- 	function model_to_view (
-- 		self   : not null access type_view;
-- 		rect   : in type_model_rectangle)
-- 		return type_view_rectangle is
-- 		result : type_view_rectangle;
-- 	begin
-- 		result := (
-- 			x      => type_view_coordinate (rect.x - self.topleft.x) * self.scale,
-- 			y      => type_view_coordinate (rect.y - self.topleft.y) * self.scale,
-- 			width  => type_view_coordinate (rect.width) * self.scale,
-- 			height => type_view_coordinate (rect.height) * self.scale
-- 			);
-- 		
-- 		return result;
-- 	end model_to_view;
-- 
	
	function model_to_drawing (
		accessories	: in type_accessories;
		model_point : in type_model_point)	
		return type_model_point is 
		use et_general;
		p : type_model_point; -- to be returned
	begin
-- 		set (point	=> p,
-- 			 axis	=> X, 
-- 			 value	=> model_point.x - model.frame_bounding_box.x);
		p.x := model_point.x - accessories.frame_bounding_box.x;
		
-- 		set (point	=> p,
-- 			 axis	=> Y,
-- 			 value	=> type_model_coordinate (model.frame.size.y) 
-- 						- model_point.y 
-- 						+ model.frame_bounding_box.y);

		p.y := type_model_coordinate (accessories.frame.size.y) 
						- model_point.y 
						+ accessories.frame_bounding_box.y;
		return p;
	end;

	function bounding_box (accessories : in type_accessories)
		return type_model_rectangle is
	begin
		--return self.paper_bounding_box;
		return accessories.paper_bounding_box; -- CS
	end;


	procedure gtk_new (
		self	: out type_view_ptr) is
-- 		model	: access type_model'class := null) is
	begin
		self := new type_view;
		--init (self, model);
		init (self);
	end;


	procedure draw_grid (
		self    : not null access type_view;
		style   : gtkada.style.drawing_style;
		context : type_draw_context;
		area    : type_model_rectangle)	is separate;

	procedure draw_frame (
		in_area	: in type_model_rectangle := no_rectangle;
		context : in type_draw_context) is separate;

	procedure draw_nets (
		in_area	: in type_model_rectangle := no_rectangle;
		context : in type_draw_context) is separate;

	procedure draw_units (
		in_area	: in type_model_rectangle := no_rectangle;
		context : in type_draw_context) is separate;

	procedure draw_internal (
		self    : not null access type_view;
		context : type_draw_context;
		area    : type_model_rectangle) 
	is
		-- prepare draing style so that white grid dots will be drawn.
		style : drawing_style := gtk_new (stroke => gdk.rgba.white_rgba);
	begin
		put_line ("draw internal ...");
		
-- 		if self.model /= null then

			-- draw a black background:
			set_source_rgb (context.cr, 0.0, 0.0, 0.0);
			paint (context.cr);

			-- draw white grid dots:
			set_grid_size (self, pac_canvas.grid_default);
			draw_grid (self, style, context, area);

			draw_frame (area, context); -- separate unit
-- 			self.model.draw_nets (area, context); -- separate unit
-- 			self.model.draw_units (area, context); -- separate unit
			-- CS self.model.draw_texts (area, context);
			-- CS self.model.draw_submodules (area, context);
			
-- 		end if;
	end draw_internal;



	procedure set_module (
		module		: in et_project.type_modules.cursor;
		sheet		: in et_coordinates.type_sheet := et_coordinates.type_sheet'first) -- the sheet to be opened
	is
		use et_general;
		use et_frames;
		use et_project;
		
	begin 
		accessories.module := module;

		-- set some variables frequently used regarding frame and paper:
		--accessories.frame := type_modules.element (model.module).frames.frame;
		accessories.frame := type_modules.element (module).frames.frame;
		
		accessories.paper_height := type_model_coordinate (paper_dimension (
							paper_size	=> accessories.frame.paper,
							orientation	=> accessories.frame.orientation,
							axis		=> Y));

		accessories.paper_width := type_model_coordinate (paper_dimension (
							paper_size	=> accessories.frame.paper,
							orientation	=> accessories.frame.orientation,
							axis		=> X));

		-- The drawing frame has a bounding box:

		-- position (upper left corner):
		accessories.frame_bounding_box.x := (accessories.paper_width - type_model_coordinate (accessories.frame.size.x)) / 2.0;
		accessories.frame_bounding_box.y := (accessories.paper_height - type_model_coordinate (accessories.frame.size.y)) / 2.0;

		-- width and height
		accessories.frame_bounding_box.width := type_model_coordinate (accessories.frame.size.x);
		accessories.frame_bounding_box.height := type_model_coordinate (accessories.frame.size.y);

		-- The sheet has a drawing box:
		accessories.paper_bounding_box := (0.0, 0.0, accessories.paper_width, accessories.paper_height);

		-- Drawing of the title block items is relative to the title block position:
		accessories.title_block_position := accessories.frame.title_block_schematic.position;

		-- set active sheet
		accessories.sheet := sheet;
	end set_module;

	
-- 	function convert_x (x : in et_coordinates.type_distance) return type_view_coordinate is begin
-- 		return type_view_coordinate (
-- 			type_model_coordinate (x)
-- 			);
-- 	end;
-- 
-- 	function convert_x (x : in et_coordinates.type_distance) return type_model_coordinate is begin
-- 		return type_model_coordinate (x);
-- 	end;

	function convert_and_shift_y (
		accessories	: in type_accessories;
		y			: in type_distance)
		return type_view_coordinate is 
	begin
		return type_view_coordinate 
			(
			accessories.frame_bounding_box.height 
			- type_model_coordinate (y)
			);
	end;
		
	function convert_and_shift_y (
		accessories	: in type_accessories;
		y			: in type_distance)
		return type_model_coordinate is 
	begin
		return (
			accessories.frame_bounding_box.height 
			- type_model_coordinate (y)
			);
	end;

	
end et_canvas.schematic;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

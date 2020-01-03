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

with et_coordinates;			use et_coordinates;
use et_coordinates.geometry;

package body et_canvas.schematic is

	procedure dummy is begin null; end;

	model_signals : constant gtkada.types.chars_ptr_array := (
		1 => new_string (string (signal_layout_changed))
		);

	
	model_class_record : glib.object.ada_gobject_class := glib.object.uninitialized_class;
	
	function model_get_type return glib.gtype is begin
		glib.object.initialize_class_record (
			ancestor     => gtype_object,
			signals      => model_signals,
			class_record => model_class_record,
			type_name    => "gtkada_model",
			parameters   => (
				1 => (1 => gtype_none)  	-- layout_changed
				)
			);  
		return model_class_record.the_type;
	end model_get_type;
	
	procedure init (self : not null access type_model'class) is begin
		if not self.is_created then
			g_new (self, model_get_type);
		end if;
	end;
	
	procedure gtk_new (self : out type_model_ptr) is begin
		self := new type_model;
		init (self);
	end;	

	
-- CONVERSIONS BETWEEN COORDINATE SYSTEMS
	
	function view_to_model (
		self   : not null access type_view;
		p      : in type_view_point) 
		return type_model_point is
	begin
		return type_model_point (set (
			x	=> type_model_coordinate (p.x / self.scale) + self.topleft_2.x,
			y	=> type_model_coordinate (p.y / self.scale) + self.topleft_2.y
			));
	end view_to_model;

	function view_to_model (
		self   : not null access type_view;
		rect   : in type_view_rectangle) -- position and size are in pixels
		return type_model_rectangle is
	begin
		return (x      => type_model_coordinate (rect.x / self.scale) + self.topleft_2.x,
				y      => type_model_coordinate (rect.y / self.scale) + self.topleft_2.y,
				width  => type_model_coordinate (rect.width / self.scale),
				height => type_model_coordinate (rect.height / self.scale));
	end view_to_model;

	
	function model_to_view (
		self   : not null access type_view;
		p      : in type_model_point) 
		return type_view_point is
	begin
		return (
			x => type_view_coordinate (p.x - self.topleft_2.x) * self.scale,
			y => type_view_coordinate (p.y - self.topleft_2.y) * self.scale
			);
	end model_to_view;

	function model_to_view (
		self   : not null access type_view;
		rect   : in type_model_rectangle)
		return type_view_rectangle is
		result : type_view_rectangle;
	begin
		result := (
			x      => type_view_coordinate (rect.x - self.topleft_2.x) * self.scale,
			y      => type_view_coordinate (rect.y - self.topleft_2.y) * self.scale,
			width  => type_view_coordinate (rect.width) * self.scale,
			height => type_view_coordinate (rect.height) * self.scale
			);
		
		return result;
	end model_to_view;


	
	procedure set_scale (
		self     : not null access type_view;
		scale    : gdouble := 1.0;
		preserve : type_model_point := geometry.origin)
	is
		-- backup the current scale
		old_scale : constant gdouble := self.scale;
		
		box : type_model_rectangle;
		p   : type_model_point;
	begin
		if preserve /= geometry.origin then
			-- set p at the point given by preserve
			p := preserve;
		else
			-- get the visible area
			box := self.get_visible_area;

			-- set p at the center of the visible area
			p := type_model_point (set (box.x + box.width / 2.0, box.y + box.height / 2.0));
		end if;

		self.scale := scale;

		-- calculate the new topleft corner of the visible area:
		self.topleft_2 := type_model_point (set (
			p.x - (p.x - self.topleft_2.x) * type_model_coordinate (old_scale / scale),
			p.y - (p.y - self.topleft_2.y) * type_model_coordinate (old_scale / scale)));

		self.scale_to_fit_requested := 0.0;
		self.set_adjustment_values;
		self.queue_draw;
	end set_scale;

	function get_visible_area (self : not null access type_view)
		return type_model_rectangle is
	begin
		return self.view_to_model (
			-- Assemble a type_view_rectangle which will be converted
			-- to a type_model_rectangle by function view_to_model.
			(
			-- The visible area of the view always starts at 0/0 (topleft corner):
			x		=> 0.0, 
			y		=> 0.0,

			-- The view size is adjusted by the operator. So it must be inquired
			-- by calling get_allocated_width and get_allocated_height.
			-- get_allocated_width and get_allocated_height return an integer type
			-- which corresponds to the number of pixels required by self in y and x
			-- direction. Since the model coordinates are gdouble (a float type),
			-- the number of pixels must be converted to a gdouble type:
			width	=> gdouble (self.get_allocated_width),
			height	=> gdouble (self.get_allocated_height)
			));
	end get_visible_area;

	procedure refresh_layout (
		self        : not null access type_model;
		send_signal : boolean := true) is
		
-- 		procedure do_size_request (item : not null access type_item'class) is begin
-- 			type_item'class (item.all).size_request;
-- 		end;

	begin
		-- Update the width and height of all items:
-- 		type_model'class (self.all).for_each_item (do_size_request'access);

		if send_signal then
			type_model'class (self.all).layout_changed;
		end if;
	end refresh_layout;


	procedure set_grid_size (
		self : not null access type_view'class;
		size : type_model_coordinate := 30.0) is
	begin
		self.grid_size := size;
	end set_grid_size;

	procedure draw_grid_dots (
		self    : not null access type_view'class;
		style   : gtkada.style.drawing_style;
		context : type_draw_context;
		area    : type_model_rectangle)
	is
		tmpx, tmpy  : type_view_coordinate;
	begin
		if style.get_fill /= null_pattern then
			set_source (context.cr, style.get_fill);
			paint (context.cr);
		end if;

		if self.grid_size /= 0.0 then
			new_path (context.cr);

			tmpx := type_view_coordinate (gint (area.x / self.grid_size)) * type_view_coordinate (self.grid_size);
			
			while tmpx < type_view_coordinate (area.x + area.width) loop
				tmpy := type_view_coordinate (gint (area.y / self.grid_size)) * type_view_coordinate (self.grid_size);
				
				while tmpy < type_view_coordinate (area.y + area.height) loop
					rectangle (context.cr, tmpx - 0.5, tmpy - 0.5, 1.0, 1.0);
					tmpy := tmpy + type_view_coordinate (self.grid_size);
				end loop;

				tmpx := tmpx + type_view_coordinate (self.grid_size);
			end loop;

			style.finish_path (context.cr);
		end if;
	end draw_grid_dots;

	procedure draw_internal (
		self    : not null access type_view;
		context : type_draw_context;
		area    : type_model_rectangle)
	is
		procedure draw_item (
			item : not null access type_item'class) is
		begin
			translate_and_draw_item (item, context);
		end;

		-- prepare draing style so that white grid dots will be drawn.
		style : drawing_style := gtk_new (stroke => gdk.rgba.white_rgba);
		
	begin
		put_line ("draw internal ...");
		
		if self.model /= null then

			-- draw a black background:
			set_source_rgb (context.cr, 0.0, 0.0, 0.0);
			paint (context.cr);

			-- draw white grid dots:
			set_grid_size (self, 100.0);
			draw_grid_dots (self, style, context, area);
			
-- 			self.model.for_each_item (draw_item'access, in_area => area);
			
		end if;
	end draw_internal;

	procedure scale_to_fit (
		self      : not null access type_view;
		rect      : type_model_rectangle := no_rectangle;
		min_scale : gdouble := 1.0 / 4.0;
		max_scale : gdouble := 4.0)
	is
		box     : type_model_rectangle;
		w, h, s : gdouble;
		alloc   : gtk_allocation;
		tl      : type_model_point;
		wmin, hmin : gdouble;
	begin
		put_line ("scale to fit ...");
		self.get_allocation (alloc);
		if alloc.width <= 1 then
			self.scale_to_fit_requested := max_scale;
			self.scale_to_fit_area := rect;

		elsif self.model /= null then
			self.scale_to_fit_requested := 0.0;
			
			if rect = no_rectangle then
				box := self.model.bounding_box;
			else
				box := rect;
			end if;

			if box.width /= 0.0 and then box.height /= 0.0 then
						  
				w := gdouble (alloc.width);
				h := gdouble (alloc.height);

				--  the "-1.0" below compensates for rounding errors, since
				--  otherwise we are still seeing the scrollbar along the axis
				--  used to compute the scale.
				wmin := (w - 2.0 * view_margin - 1.0) / type_view_coordinate (box.width);
				hmin := (h - 2.0 * view_margin - 1.0) / type_view_coordinate (box.height);
				wmin := gdouble'min (wmin, hmin);
				s := gdouble'min (max_scale, wmin);
				s := gdouble'max (min_scale, s);

				-- calculate the new topleft corner of the visible area:
				tl := type_model_point (set (
					x	=> box.x - (type_model_coordinate (w / s) - box.width) / 2.0,
					y	=> box.y - (type_model_coordinate (h / s) - box.height) / 2.0)
					);
				
				self.scale := s;
				self.topleft_2 := tl;
				self.set_adjustment_values;
				self.queue_draw;

			end if;
		end if;
	end scale_to_fit;

	
end et_canvas.schematic;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

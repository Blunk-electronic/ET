------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         CANVAS FOR SCHEMATIC                             --
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

with et_coordinates;		use et_coordinates;
with et_project;			--use et_project;
with et_frames;				--use et_frames;

with et_canvas_2;

package et_canvas.schematic is

	package pac_canvas is new et_canvas_2.pac_canvas (
		type_distance	=> et_coordinates.type_distance,
		geometry		=> et_coordinates.geometry);

	use pac_canvas;
	


-- MODEL
	-- 	type type_model_sch is new pac_canvas.type_model with record
	type type_accessories is new pac_canvas.type_accessories with record	
		module	: et_project.type_modules.cursor;

		-- These variables are frequently used. Procedure set_module
		-- sets them. Other operations are free to access
		-- them.
		frame				: et_frames.type_frame (et_frames.SCHEMATIC);
		frame_bounding_box	: type_model_rectangle;

		paper_bounding_box	: type_model_rectangle;
		paper_height		: type_model_coordinate;
		paper_width			: type_model_coordinate;

		title_block_position	: et_frames.type_position;

		-- CS grid_size 	: type_model_coordinate_positive := 20.0;
		
		-- the active sheet
		sheet	: et_coordinates.type_sheet := type_sheet'first;
	end record;

	-- Converts a model point to a drawing point. 
	-- NOTE: The model point is in a coordinate system with y-axis
	-- going downwards. The drawing point is in a system where y-axis
	-- goes upwards. The origin of the drawing coordinate system is the
	-- lower left corner of the drawing frame.
-- CS
-- 	overriding function model_to_drawing (
-- 		accessories	: in type_accessories;
-- 		model_point : in type_model_point)
-- 		return type_model_point;


	overriding function bounding_box (accessories : in type_accessories)
		return type_model_rectangle;


	-- This function converts a y-value from the drawing to a y-value in the view.
	-- The input y increases upwards. The output y increases downwards.
	overriding function convert_and_shift_y (
		accessories	: in type_accessories;
		y			: in type_distance)
		return type_view_coordinate;

	-- This function converts a y-value from the drawing to a y-value in the model.
	-- The input y increases upwards. The output y increases downwards.
	overriding function convert_and_shift_y (
		accessories	: in type_accessories;
		y			: in type_distance) 
		return type_model_coordinate;

		
	accessories : type_accessories;
	
-- 	type type_model_ptr is access all type_model'class;
-- 	type type_model_ptr_sch is access all type_model_sch;

-- 	model	: type_model_ptr_sch;
-- 	model	: type_model_ptr;


	
	-- Creates a new model (or a drawing sheet according to the example above):
-- 	procedure gtk_new (self : out type_model_ptr_sch);

-- 	-- Initializes the internal data so that the model can send signals:
-- 	procedure init (self : not null access type_model'class);


-- VIEW

-- 	-- scale
-- 	scale_min : constant gdouble := 0.1;
-- 	scale_max : constant gdouble := 10.0;
-- 	subtype type_scale is gdouble range scale_min .. scale_max;
-- 	scale_default : constant type_scale := 1.0;
-- 	scale_delta_on_zoom : constant type_scale := 0.1;
-- 	
-- 	
	-- The view (or canvas) displays a certain region of the model (or the sheet) 
	-- depending on scrolling or zoom.
	type type_view is new pac_canvas.type_view with record
		accessories	: type_accessories;
	end record;

	-- The pointer to the canvas/view:
	type type_view_ptr is access all type_view'class;

	canvas	: type_view_ptr;

	
	procedure gtk_new (
		self	: out type_view_ptr);

	
	
-- CONVERSIONS BETWEEN COORDINATE SYSTEMS


-- 
-- 	procedure gtk_new (
-- 		self	: out type_view_ptr;
-- 		model	: access type_model'class := null);
-- 	

	procedure draw_frame (
		in_area	: in type_model_rectangle := no_rectangle;
		context : in type_draw_context);

	procedure draw_nets (
		in_area	: in type_model_rectangle := no_rectangle;
		context : in type_draw_context);
	
	procedure draw_units (
		in_area	: in type_model_rectangle := no_rectangle;
		context : in type_draw_context);

	
	-- Redraw either the whole view, or a specific part of it only.
	-- The transformation matrix has already been set on the context.
	overriding procedure draw_internal (
		self    : not null access type_view;
		context : type_draw_context;
		area    : type_model_rectangle);

	-- Assign the module to be edited to the model:
	procedure set_module (
		module		: in et_project.type_modules.cursor;
		sheet		: in et_coordinates.type_sheet := et_coordinates.type_sheet'first); -- the sheet to be opened



	
end et_canvas.schematic;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

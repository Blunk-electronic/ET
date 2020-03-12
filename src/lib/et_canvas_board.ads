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
-- DESCRIPTION:
-- 
-- This package draws the pcb layout (board) via various child packages.
-- This package instantiates the generic canvas package (et_canvas_general.pac_canvas)
-- and extends the type_view by the type_drawing. The latter is the link
-- to the actual drawing. The type_drawing provides information on sheet size,
-- fame bounding box, paper size etc. This information is frequently used
-- by various draw operations.
--  Further-on the generic package for primitve draw operations (et_canvas_draw.pac_draw)
-- is instantiated here so that lots of draw operations can use pac_draw_package.

with gtk.window; 			use gtk.window;

with glib;					use glib;
with cairo;					use cairo;
with cairo.pattern;			use cairo.pattern;
with gtkada.style;

with et_general;
with et_pcb_coordinates;	use et_pcb_coordinates;
with et_packages;
with et_project;			--use et_project;
with et_frames;				--use et_frames;

with et_canvas_general;
with et_canvas_primitive_draw_ops;
with et_string_processing;			use et_string_processing;

package et_canvas_board is

	title : constant string := et_general.system_name & " BOARD ";

	procedure set_title_bar (
		-- CS project name								
		module		: in et_general.type_module_name.bounded_string);

	
	-- Instantiate the general canvas package:
	package pac_canvas is new et_canvas_general.pac_canvas (
		canvas_name		=> "board", -- CS provide domain name like scripting.type_domain
		geometry		=> et_pcb_coordinates.geometry);

	
	-- In order to draw objects of packages and board we instantiate this package:
	package pac_draw_package is new et_canvas_primitive_draw_ops.pac_draw (
		pac_canvas	=> pac_canvas,
		pac_shapes	=> et_packages.pac_shapes,
		pac_text	=> et_packages.pac_text);
	
	use pac_canvas;	

	

	use et_pcb_coordinates.geometry;


	
	-- Frequently used things to draw the board layout.
	type type_drawing is record	

		-- These variables are frequently used. Procedure init_drawing
		-- sets them. Other operations are free to access them.
		frame				: et_frames.type_frame_pcb;
		frame_bounding_box	: type_rectangle;

		paper_bounding_box	: type_rectangle;
		paper_height		: geometry.type_distance_positive;
		paper_width			: geometry.type_distance_positive;

		title_block_position	: et_frames.type_position;

		grid : et_pcb_coordinates.geometry.type_grid;

		-- The position of the board origin relative to the lower left
		-- corner of the drawing frame:
		board_origin : geometry.type_point;
	end record;
	

-- 	-- Initializes the internal data so that the model can send signals:
-- 	procedure init (self : not null access type_model'class);


-- VIEW OR CANVAS
	
	type type_view is new pac_canvas.type_view with record
		drawing	: type_drawing;
	end record;

	-- Returns the distance on the given axis rounded to the current grid.
	overriding function to_string (
		self	: not null access type_view;
		point	: in type_point;
		axis	: in et_general.type_axis_2d)
		return string;
	
	-- Returns the given point x/y rounded to the current grid.
	overriding function to_string (
		self	: not null access type_view;
		point	: in type_point)
		return string;
	
	-- Returns the bounding box of all items.
	overriding function bounding_box (self : not null access type_view)
		return type_rectangle;

	-- This function converts a y-value.
	-- The input y increases upwards. The output y increases downwards.
	overriding function convert_and_shift_y (
		self	: not null access type_view;
		y		: in type_distance_total) 
		return type_distance_total;
		
	-- This function converts a y-value.
	-- The input y increases upwards. The output y increases downwards.
	overriding function convert_and_shift_y (
		self	: not null access type_view;
		y		: in type_distance_total)
		return type_view_coordinate;

	overriding function model_to_drawing (
		self		: not null access type_view;
		model_point : in type_point)
		return type_point;

	overriding function drawing_to_model (
		self			: not null access type_view;
		drawing_point : in type_point)	
		return type_point;

		

	-- Creates a new board view:
	procedure gtk_new (
		self	: out type_view_ptr);
	
	-- Redraws either the whole board view, or a specific part of it only:
	overriding procedure draw_internal (
		self    : not null access type_view;
		context : type_draw_context;
		area    : type_rectangle);

	-- Sets the board drawing grid:
	procedure set_grid (view : in type_view_ptr);

	-- Redraws the board:
	procedure redraw (view : in type_view_ptr);

	-- Executes a canvas related command:
	overriding procedure execute_command (
		self    		: not null access type_view;
		cmd				: in type_fields_of_line;
		log_threshold	: in type_log_level);

	
	cursor_line_width : constant type_distance_positive := et_packages.line_width_min;
	cursor_half_size : constant type_distance_positive := 5.0;
	type type_cursor_line is new et_packages.pac_shapes.type_line with null record;

	overriding procedure draw_cursor (
		self		: not null access type_view;
		in_area		: in type_rectangle := no_rectangle;
		context 	: in type_draw_context;
		cursor		: in type_cursor);
	
end et_canvas_board;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

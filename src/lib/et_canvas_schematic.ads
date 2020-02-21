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
-- DESCRIPTION:
-- 
-- This package draws the schematic via various child packages.
-- This package instantiates the generic canvas package (et_canvas_general.pac_canvas)
-- and extends the type_view by the type_drawing. The latter is the link
-- to the actual drawing. The type_drawing provides information on sheet size,
-- fame bounding box, paper size etc. This information is frequently used
-- by various draw operations.
--  Further-on the generic package for primitve draw operations (et_canvas_draw.pac_draw)
-- is instantiated here so that lots of draw operations can use pac_draw_package.

with glib;					use glib;
with cairo;					use cairo;
with cairo.pattern;			use cairo.pattern;
with gtkada.style;     		use gtkada.style;

with et_coordinates;		use et_coordinates;
with et_project;
with et_symbols;
with et_schematic;
with et_frames;

with et_canvas_general;
with et_canvas_primitive_draw_ops;

package et_canvas_schematic is

	-- Instantiate the canvas package:
	package pac_canvas is new et_canvas_general.pac_canvas (
		canvas_name		=> "schematic", -- CS provide domain name like scripting.type_domain
		geometry		=> et_coordinates.geometry);

	use pac_canvas;
	
	-- Objects that neither belong to frames or symbols like:
	--  - net segments
	--  - lines, arcs, circles of documentation
	-- we instantiate this package:
	package pac_draw_misc is new et_canvas_primitive_draw_ops.pac_draw (
		pac_canvas	=> pac_canvas,
		pac_shapes	=> et_schematic.pac_shapes,
		pac_text	=> et_symbols.pac_text);
	
	

	use et_coordinates.geometry; -- CS
	
	-- This is the link to the actual drawing:
	type type_drawing is record	
		module	: et_project.type_modules.cursor; -- the currently active module

		-- These variables are frequently used. Procedure init_drawing
		-- sets them. Other operations are free to access them.
		frame				: et_frames.type_frame (et_frames.SCHEMATIC);
		frame_bounding_box	: type_rectangle;

		paper_bounding_box	: type_rectangle;
		paper_height		: et_coordinates.geometry.type_distance_positive; -- CS
		paper_width			: et_coordinates.geometry.type_distance_positive;

		title_block_position	: et_frames.type_position;

		-- CS grid_size 	: type_distance_positive_positive := 20.0;
		
		-- the active sheet
		sheet	: et_coordinates.type_sheet := type_sheet'first;
	end record;

	
-- 	-- Initializes the internal data so that the model can send signals:
-- 	procedure init (self : not null access type_model'class);


-- VIEW OR CANVAS

	type type_view is new pac_canvas.type_view with record
		drawing	: type_drawing;
	end record;

	-- Returns the name of the currently active module:
	function active_module (self : not null access type_view) return string;
	
	-- Returns the bounding box of all items of the current sheet.
	overriding function bounding_box (self : not null access type_view)
		return type_rectangle;

	-- This function converts a y-value.
	-- The input y increases upwards. The output y increases downwards.
	overriding function convert_and_shift_y (
		self	: not null access type_view;
		y		: in type_distance) 
		return type_distance;
		
	-- This function converts a y-value.
	-- The input y increases upwards. The output y increases downwards.
	overriding function convert_and_shift_y (
		self	: not null access type_view;
		y		: in type_distance)
		return type_view_coordinate;

	-- This function converts the y-value of a drawing point.
	-- The input y increases upwards. The output y increases downwards.
	overriding function model_to_drawing (
		self		: not null access type_view;
		model_point : in type_point)
		return type_point;

	-- Creates a new schematic view:
	procedure gtk_new (
		self	: out type_view_ptr);


	
	-- Redraw either the whole schematic sheet or a specific part of it only.
	overriding procedure draw_internal (
		self    : not null access type_view;
		context : type_draw_context;
		area    : type_rectangle);
	
	-- Init the drawing:
	procedure init_drawing (
		view	: in type_view_ptr;
		module	: in et_project.type_modules.cursor;
		sheet	: in et_coordinates.type_sheet := et_coordinates.type_sheet'first); -- the sheet to be opened
	
end et_canvas_schematic;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           CANVAS SCHEMATIC                               --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2022 Mario Blunk, Blunk electronic          --
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

with ada.strings;					use ada.strings;
--with ada.characters;				use ada.characters;
--with ada.characters.handling;		use ada.characters.handling;
with ada.strings.fixed; 			use ada.strings.fixed;

with gdk.types;						use gdk.types;
with gdk.types.keysyms;				use gdk.types.keysyms;

with gtk;
with gtk.window;
with gtk.gentry;					use gtk.gentry;
with gtk.box;						use gtk.box;
with gtk.label;						use gtk.label;
with gtk.combo_box_text;			use gtk.combo_box_text;

with glib;							use glib;
with cairo;							use cairo;

with et_nets;						--use et_nets;
with et_net_labels;					use et_net_labels;
with et_net_names;					use et_net_names;
with et_general;					use et_general;
with et_geometry;					use et_geometry;

with et_coordinates;				use et_coordinates;
use et_coordinates.pac_geometry_sch;
use et_coordinates.pac_geometry_2;

with et_project.modules;			use et_project.modules;
with et_symbols;
with et_schematic;
with et_schematic_ops;
with et_schematic_ops.nets;
with et_schematic_ops.units;
with et_frames;
with et_text;						use et_text;

with et_canvas_general;				use et_canvas_general;
with et_canvas_primitive_draw_ops;	
with et_string_processing;			use et_string_processing;
with et_logging;					use et_logging;

with et_schematic_ops;
with et_canvas_schematic_nets;		use et_canvas_schematic_nets;
with et_canvas_schematic_units;		use et_canvas_schematic_units;


package et_canvas_schematic is

	use et_symbols.pac_text;
	
	use et_project.modules.pac_generic_modules;
	
	use pac_net_name;
	
	title : constant string := system_name & " SCHEMATIC ";

	procedure set_title_bar (
		-- CS project name								
		module		: in pac_module_name.bounded_string);
	
	-- Instantiate the canvas package:
	package pac_canvas is new et_canvas_general.pac_canvas (
		canvas_name		=> "schematic", -- CS provide domain name like scripting.type_domain
		pac_geometry_2	=> et_coordinates.pac_geometry_2);

	use pac_canvas;



	
	box_sheet		: gtk_vbox;
	label_sheet		: gtk_label;
	cbox_sheet		: gtk_combo_box_text;

	procedure update_sheet_number_display;
	procedure build_sheet_number_display;


	
	-- For primitve draw operations:
	package pac_draw is new et_canvas_primitive_draw_ops.pac_draw (
		pac_canvas		=> pac_canvas,
		pac_polygons	=> pac_polygons,
		pac_contours	=> pac_contours,
		pac_text		=> et_symbols.pac_text);

	use pac_draw;
	


	
	-- The current active module is stored here. Whenever objects of the schematic
	-- or board are to be drawn, this variable must be read.
	current_active_module : et_project.modules.pac_generic_modules.cursor; -- the currently active module

	-- The current active sheet:
	current_active_sheet : et_coordinates.type_sheet := type_sheet'first;


	
	-- Frequently used things to draw the schematic:
	type type_drawing is null record;


	
-- 	-- Initializes the internal data so that the model can send signals:
-- 	procedure init (self : not null access type_model'class);



	
	-- Composes a console command like 
	-- "schematic motor_driver execute script my_script.scr"
	-- and sends it to procedure et_scripting.schematic_cmd
	-- to be executed.:
	procedure execute_script (script : in pac_script_name.bounded_string);	

	-- Executes a command as typed on the console by the operator
	-- like "rename device R1 R2".
	-- Calls et_scripting.schematic_cmd for the actual execution.
	procedure execute_command (self : access gtk_entry_record'class);


	
-- VIEW OR CANVAS

	type type_view is new pac_canvas.type_view with record
		drawing	: type_drawing;
	end record;


	
	label_console_text : constant string := 
		(8 * " ") & "switch module: F11 / F12";

	-- Appends the label_console_text to the existing text
	-- of label_console:
	procedure set_label_console;


	
	procedure redraw_board;
	procedure redraw_schematic;
	procedure redraw;


	
	-- Advances no previous generic module. If there is no
	-- previous module, selects the last module of 
	-- collection of generic modules.
	procedure next_module;

	-- Advances no next generic module. If there is no
	-- next module, selects the first module of 
	-- collection of generic modules.
	procedure previous_module;

	
	
	-- Returns the name of the currently active module:
	function active_module return pac_module_name.bounded_string;

	-- Returns the bounding box of all items of the current sheet.
	overriding function bounding_box (self : not null access type_view)
		return type_bounding_box;

	
	overriding function model_to_drawing (
		self		: not null access type_view;
		model_point : in type_model_point)
		return type_point;

	
	overriding function drawing_to_model (
		self			: not null access type_view;
		drawing_point : in type_point)	
		return type_model_point;

	
	-- Creates a new schematic view:
	procedure gtk_new (
		self	: out type_view_ptr);


	
	-- Redraw either the whole schematic sheet or a specific part of it only.
	overriding procedure draw_internal (
		self    : not null access type_view;
		context : type_draw_context;
		area    : type_bounding_box);


	-- Sets the active module to be displayed in the canvas.
	-- The module must exist inside the current project directory.
	procedure set_module (
		module	: in pac_module_name.bounded_string); -- motor_driver

	-- Sets the global variables "current_active_module" and "current_active_sheet".
	-- Sets the grid values to be displayed in the coordinates display.
	procedure init_drawing (
		module	: in et_project.modules.pac_generic_modules.cursor; -- the module to be drawn in schematic and layout
		sheet	: in et_coordinates.type_sheet := et_coordinates.type_sheet'first); -- the sheet to be drawn

	

	-- These procedures set the grid as entered in the grid box:
	
	procedure set_grid_x (self : access gtk_entry_record'class);
	-- Additionally sets the grid for y. Mostly grid of x and y axis are the same.
	
	procedure set_grid_y (self : access gtk_entry_record'class);


	-- Multiplier for grid densities:
	grid_density_multiplier_coarse	: constant type_distance_positive := 10.0;
	grid_density_multiplier_normal	: constant type_distance_positive := 1.0;
	grid_density_multiplier_fine	: constant type_distance_positive := 0.1;

	-- Resets the grid density to default and snaps the cursor
	-- to the nearest grid point.
	-- Updates the coordinates display.
	procedure reset_grid_and_cursor (
		self : not null access type_view);

	-- Sets the grid density and snaps the cursor
	-- to the nearest grid point.
	-- Updates the coordinates display.
	procedure set_grid (
		self	: not null access type_view;
		density	: in type_grid_density); -- COARSE, NORMAL, FINE

	
	overriding procedure move_cursor (
		self		: not null access type_view;
		coordinates	: in type_coordinates;  -- relative/absolute
		cursor		: in out type_cursor;
		position	: in type_point);

	overriding procedure move_cursor (
		self		: not null access type_view;
		direction	: in type_cursor_direction; -- right, left, up, down
		cursor		: in out type_cursor);

	cursor_line_width : constant type_distance_positive := 0.8;
	cursor_half_size : constant type_distance_positive := 50.0;
	type type_cursor_line is new type_line with null record;
	
	overriding procedure draw_cursor (
		self		: not null access type_view;
		in_area		: in type_bounding_box := no_area;
		context 	: in type_draw_context;
		cursor		: in type_cursor);

	overriding function get_grid (
		self : not null access type_view)
		return type_grid;

	overriding function get_frame (
		self : not null access type_view)
		return et_frames.type_frame;

	
	overriding function frame_height (
		self : not null access type_view)
		return type_float_internal_positive;

	
	overriding function frame_width (
		self : not null access type_view)
		return type_float_internal_positive;
	
	
	overriding function title_block_position (
		self : not null access type_view)
		return et_frames.type_position;


	overriding function get_verb (
		self	: not null access type_view)
		return string;

	overriding function get_noun (
		self	: not null access type_view)
		return string;

	-- Resets global variables required for selections, clarifications, ...
	-- Verb and noun remain as they are
	-- so that the mode is unchanged.
	-- Should be called when exception rises in order to clean up.
	-- Should also be called when the operator hits ESC.
	procedure reset_selections;

	-- Clears list of proposed objects such as net segments, units, ...
	procedure clear_proposed_objects;
	
	overriding procedure evaluate_key (
		self	: not null access type_view;
		key		: in gdk_key_type);

	overriding procedure evaluate_mouse_position (
		self	: not null access type_view;
		point	: in type_point);
	
	overriding procedure button_pressed (
		self	: not null access type_view;
		button	: in type_mouse_button;
		point	: in type_point);

	overriding procedure reset_properties_selection (
		self : not null access type_view);


	status_text_module_saved : constant string := "Module saved.";

	-- This procedure saves the current active module 
	-- in its file. This procedure is called from
	-- the overridden procedure save_drawing.
	procedure save_module;

	-- Saves the current module by calling save_module 
	-- (see above):
	overriding procedure save_drawing (
		self : not null access type_view);

	
end et_canvas_schematic;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                        CANVAS FOR SCHEMATIC                              --
--                                                                          --
--                               B o d y                                    --
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

with ada.text_io;			use ada.text_io;

with gdk.rgba; -- CS remove

with et_general;
with et_project;
with et_frames;
with et_coordinates;			use et_coordinates;
use et_coordinates.geometry;

package body et_canvas_schematic is

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
			 value	=> type_distance (self.drawing.frame.size.y) 
						- model_point.y 
						+ self.drawing.frame_bounding_box.y);

		return p;
	end;

	
	function active_module (self : not null access type_view) 
		return string is
		use et_general.type_module_name;
		use et_project.type_modules;
	begin
		return to_string (key (self.drawing.module)); -- motor_driver (without extension)
	end active_module;

	
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
		context : type_draw_context;
		area    : type_rectangle)	is separate;

	procedure draw_frame (
		self    : not null access type_view;
		in_area	: in type_rectangle := no_rectangle;
		context : in type_draw_context) is separate;

	procedure draw_nets (
		self    : not null access type_view;
		in_area	: in type_rectangle := no_rectangle;
		context : in type_draw_context) is separate;

	procedure draw_units (
		self	: not null access type_view;
		in_area	: in type_rectangle := no_rectangle;
		context : in type_draw_context) is separate;

	procedure draw_submodules (
		self	: not null access type_view;
		in_area	: in type_rectangle := no_rectangle;
		context : in type_draw_context) is separate;
	
	procedure draw_internal (
		self    : not null access type_view;
		context : type_draw_context;
		area    : type_rectangle) 
	is
		-- The given area must be shifted (left and up) by the position
		-- of the drawing frame. This is required for all objects in the 
		-- drawing frame.
		-- Take a copy of the given area:
		area_shifted : type_rectangle := area;

		-- Calculate the new position of area_shifted:
		area_shifted_new_position : type_point := type_point (set (
						x => - self.drawing.frame_bounding_box.x,
						y => - self.drawing.frame_bounding_box.y));
	begin
-- 		put_line ("draw internal ...");
		
		-- draw a black background:
		set_source_rgb (context.cr, 0.0, 0.0, 0.0);
		paint (context.cr);

		-- draw the grid
		draw_grid (self, context, area);
		
		draw_frame (self, area, context);

		-- move area_shifted
		move_by (area_shifted, area_shifted_new_position);

		-- draw objects inside the drawing frame:
		draw_units (self, area_shifted, context);
		draw_nets (self, area_shifted, context);
		-- CS self.model.draw_texts (area, context);
		draw_submodules (self, area_shifted, context);
			
	end draw_internal;



	procedure init_drawing (
		view	: in type_view_ptr;							 
		module	: in et_project.type_modules.cursor;
		sheet	: in et_coordinates.type_sheet := et_coordinates.type_sheet'first) -- the sheet to be opened
	is
		use et_general;
		use et_frames;
		use et_project;

		type type_local_view_ptr is access all type_view;
		self : type_local_view_ptr := type_local_view_ptr (view);
	begin
		self.drawing.module := module;

		-- set some variables frequently used regarding frame and paper:
		self.drawing.frame := type_modules.element (module).frames.frame;
		
		self.drawing.paper_height := type_distance_positive (paper_dimension (
							paper_size	=> self.drawing.frame.paper,
							orientation	=> self.drawing.frame.orientation,
							axis		=> Y));

		self.drawing.paper_width := type_distance_positive (paper_dimension (
							paper_size	=> self.drawing.frame.paper,
							orientation	=> self.drawing.frame.orientation,
							axis		=> X));

		-- The drawing frame has a bounding box:

		-- position (upper left corner):
		self.drawing.frame_bounding_box.x := (self.drawing.paper_width - type_distance_positive (self.drawing.frame.size.x)) / 2.0;
		self.drawing.frame_bounding_box.y := (self.drawing.paper_height - type_distance_positive (self.drawing.frame.size.y)) / 2.0;

		-- width and height
		self.drawing.frame_bounding_box.width := type_distance_positive (self.drawing.frame.size.x);
		self.drawing.frame_bounding_box.height := type_distance_positive (self.drawing.frame.size.y);

		-- The sheet has a drawing box:
		self.drawing.paper_bounding_box := (0.0, 0.0, self.drawing.paper_width, self.drawing.paper_height);

		-- Drawing of the title block items is relative to the title block position:
		self.drawing.title_block_position := self.drawing.frame.title_block_schematic.position;

		-- set active sheet
		self.drawing.sheet := sheet;
		
	end init_drawing;


	function convert_and_shift_y (
		self	: not null access type_view;
		y		: in type_distance)
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
		y		: in type_distance)
		return type_distance is 
	begin
		return (
			self.drawing.frame_bounding_box.height 
			- y
			);
	end;

	
end et_canvas_schematic;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

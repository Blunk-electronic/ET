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

with ada.text_io;				use ada.text_io;

with et_general;
with et_project;
with et_frames;
with et_coordinates;			use et_coordinates;
use et_coordinates.geometry;

package body et_canvas_schematic is

	procedure set_title_bar (
		-- CS project name
		module		: in et_general.type_module_name.bounded_string;
		sheet		: in type_sheet) is
		use et_general;
	begin
		window.set_title (title & to_string (module) &
			" sheet " & to_sheet (sheet));
	end set_title_bar;

	function to_string (
		self	: not null access type_view;
		point	: in type_point;
		axis	: in et_general.type_axis_2d)
		return string 
	is
		use et_general;
	begin
		case axis is
			when X => return to_string (round (x (point), self.drawing.grid.x));
			when Y => return to_string (round (y (point), self.drawing.grid.y));
		end case;
	end;
	
	function to_string (
		self	: not null access type_view;
		point	: in type_point)
		return string is
	begin
		return round (point, self.drawing.grid);
	end;
	
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

	function drawing_to_model (
		self			: not null access type_view;
		drawing_point	: in type_point)	
		return type_point is 
		use et_general;
		p : type_point; -- to be returned
	begin
		set (point	=> p,
			 axis	=> X, 
			 value	=> drawing_point.x + self.drawing.frame_bounding_box.x);
		
		set (point	=> p,
			 axis	=> Y,
			 value	=> type_distance (self.drawing.frame.size.y) 
						- drawing_point.y 
						+ self.drawing.frame_bounding_box.y);

		return p;
	end;
	
	function active_module return et_general.type_module_name.bounded_string is
		use et_general.type_module_name;
		use et_project.type_modules;
	begin
		return key (current_active_module); -- motor_driver (without extension)
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


		
		save (context.cr);
			
		-- Prepare the current transformation matrix (CTM) so that
		-- all following drawing is relative to the upper left frame corner.
		translate (
			context.cr,
			convert_x (self.drawing.frame_bounding_box.x),
			convert_y (self.drawing.frame_bounding_box.y));

		draw_cursor (self, area_shifted, context, cursor_main);
		draw_nets (self, area_shifted, context);
		-- CS self.model.draw_texts (area, context);
		draw_submodules (self, area_shifted, context);

		restore (context.cr);
		
	end draw_internal;

	procedure set_grid (view : in type_view_ptr) is
		use et_project;
		type type_local_view_ptr is access all type_view;
		self : type_local_view_ptr := type_local_view_ptr (view);
	begin
		self.drawing.grid := type_modules.element (current_active_module).grid;
	end set_grid;

	procedure set_sheet (
		self    : not null access type_view;
		sheet	: in et_coordinates.type_sheet) is
	begin
		self.drawing.sheet := sheet;
	end set_sheet;

	function get_sheet (
		self    : not null access type_view)
		return et_coordinates.type_sheet is
	begin
		return self.drawing.sheet;
	end get_sheet;
	
	procedure set_module (
		module	: in et_general.type_module_name.bounded_string)  -- motor_driver
	is
		use et_general;
		use et_project;
		use type_modules;
		cursor : et_project.type_modules.cursor := find (modules, module);
	begin
		if cursor /= type_modules.no_element then -- module exists in project
			current_active_module := cursor;
		else
			log (WARNING, "Generic module " & enclose_in_quotes (to_string (module)) 
				 & " does not exist !",
				 console => true);

			
			-- CS list available modules
		end if;
	end set_module;

	procedure init_frame (
		view	: in type_view_ptr)
	is
		use et_general;
		use et_frames;
		use et_project;

		type type_local_view_ptr is access all type_view;
		self : type_local_view_ptr := type_local_view_ptr (view);

	begin
		-- set some variables frequently used regarding frame and paper:
		self.drawing.frame := type_modules.element (current_active_module).frames.frame;
		
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

	end init_frame;
	
	procedure init_drawing (
		view	: in type_view_ptr;							 
		module	: in et_project.type_modules.cursor; -- the module to be drawn
		sheet	: in et_coordinates.type_sheet := et_coordinates.type_sheet'first) -- the sheet to be drawn
	is
		type type_local_view_ptr is access all type_view;
		self : type_local_view_ptr := type_local_view_ptr (view);
	begin
		-- set the active module:
		current_active_module := module;
		
		-- set active sheet:
		self.drawing.sheet := sheet;
		
	end init_drawing;

	procedure redraw (view : in type_view_ptr) is begin
		init_frame (view); -- set the frame stuff (paper, sheet size, title block, ...)
		set_grid (view);
		queue_draw (view);
	end;

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

	procedure execute_command (
		self			: not null access type_view;
		cmd				: in type_fields_of_line;
		log_threshold	: in type_log_level) is separate;


	procedure draw_cursor (
		self		: not null access type_view;
		in_area		: in type_rectangle := no_rectangle;
		context 	: in type_draw_context;
		cursor		: in type_cursor)
	is
-- 		use pac_draw_misc;
-- 		type line is new pac_shapes.type_line with null record;
-- 		line_horizontal, line_vertical : type_line;
	begin
		cairo.set_line_width (context.cr, type_view_coordinate (et_schematic.net_line_width));
		cairo.set_source_rgb (context.cr, gdouble (1), gdouble (1), gdouble (1)); -- white

-- 		pac_draw_misc.draw_line (
-- 			area		=> in_area,
-- 			context		=> context,
-- 			line		=> element (segment_cursor),
-- 			height		=> self.drawing.frame_bounding_box.height,
-- 			extend_boundaries	=> false,
-- 			boundaries_to_add	=> boundaries_default -- CS
-- 			);

		
		cairo.stroke (context.cr);		
	end draw_cursor;

	
end et_canvas_schematic;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

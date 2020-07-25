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

with ada.text_io;					use ada.text_io;
with et_display.schematic;
with et_colors.schematic;			use et_colors.schematic;
with et_modes.schematic;			use et_modes.schematic;

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
		use et_project.modules.pac_generic_modules;
	begin
		case axis is
			when X => return to_string (round (x (point), element (current_active_module).grid.x));
			when Y => return to_string (round (y (point), element (current_active_module).grid.y));
		end case;
	end;

	function to_string (
		self	: not null access type_view;
		point	: in type_point)
		return string is
		use et_project.modules.pac_generic_modules;
	begin
		return round_to_string (point, element (current_active_module).grid);
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
			 value	=> model_point.x - self.frame_bounding_box.x);
		
		set (point	=> p,
			 axis	=> Y,
			 value	=> type_distance (self.frame_height) 
						- model_point.y 
						+ self.frame_bounding_box.y);
	
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
			 value	=> drawing_point.x + self.frame_bounding_box.x);
		
		set (point	=> p,
			 axis	=> Y,
			 value	=> type_distance (self.frame_height) 
						- drawing_point.y 
						+ self.frame_bounding_box.y);

		return p;
	end;
	
	function active_module return et_general.type_module_name.bounded_string is
		use et_general.type_module_name;
		use et_project.modules.pac_generic_modules;
	begin
		return key (current_active_module); -- motor_driver (without extension)
	end active_module;


	
	
	function bounding_box (self : not null access type_view)
		return type_rectangle is
	begin
		return self.paper_bounding_box; -- CS should include all items of the current sheet.
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

	procedure draw_texts (
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
						x => - self.frame_bounding_box.x,
						y => - self.frame_bounding_box.y));
		
		use et_display.schematic;
	begin
-- 		put_line ("draw internal ...");
-- 		shift_area (self, area_shifted, cursor_main);
-- 		shift_area (self, area_shifted_new_position, cursor_main);
		
		set_color_background (context.cr);
		paint (context.cr);

		if grid_enabled then
			draw_grid (self, context, area);
		end if;
		
		-- move area_shifted
		move_by (area_shifted, area_shifted_new_position);

		-- draw objects inside the drawing frame:
		draw_units (self, area_shifted, context);
		
		save (context.cr);
			
		-- Prepare the current transformation matrix (CTM) so that
		-- all following drawing is relative to the upper left frame corner.
		translate (
			context.cr,
			convert_x (self.frame_bounding_box.x),
			convert_y (self.frame_bounding_box.y));

		draw_frame (self, area_shifted, context);

		-- CS: rework the order of drawing layers so that top layers
		-- always obscure layers underneath.
		
		-- Draw nets if layer is enabled:
		if nets_enabled then
			draw_nets (self, area_shifted, context);
		end if;

		-- Draw texts if layer is enabled:
		if texts_enabled then
			draw_texts (self, area_shifted, context);
		end if;
		
		draw_submodules (self, area_shifted, context);

		-- The cursor is drawn last so that is in the foreground:
		draw_cursor (self, area_shifted, context, cursor_main);
		
		restore (context.cr);
		
	end draw_internal;

	procedure set_module (
		module	: in et_general.type_module_name.bounded_string)  -- motor_driver
	is
		use et_general;
		use et_project.modules;
		use et_project.modules.pac_generic_modules;
		cursor : et_project.modules.pac_generic_modules.cursor := find (generic_modules, module);
	begin
		if cursor /= pac_generic_modules.no_element then -- module exists in project
			current_active_module := cursor;
		else
			log (WARNING, "Generic module " & enclose_in_quotes (to_string (module)) 
				 & " does not exist !",
				 console => true);

			
			-- CS list available modules
		end if;
	end set_module;

	procedure init_drawing (
		module	: in et_project.modules.pac_generic_modules.cursor; -- the module to be drawn
		sheet	: in et_coordinates.type_sheet := et_coordinates.type_sheet'first) -- the sheet to be drawn
	is begin
		-- set the active module:
		current_active_module := module;
		
		-- set active sheet:
		current_active_sheet := sheet;
	end init_drawing;

	function convert_and_shift_y (
		self	: not null access type_view;
		y		: in type_distance)
		return type_view_coordinate is 
	begin
		return type_view_coordinate 
			(
			self.frame_height 
			- y
			);
	end;
		
	function convert_and_shift_y (
		self	: not null access type_view;
		y		: in type_distance)
		return type_distance is 
	begin
		return (
			self.frame_height 
			- y
			);
	end;

	procedure move_cursor (
		self		: not null access type_view;
		coordinates	: in type_coordinates;
		cursor		: in out type_cursor;
		position	: in type_point) is
		use et_general;
		use et_project.modules.pac_generic_modules;
	begin
		case coordinates is
			when ABSOLUTE =>
				cursor.position := type_point (round (position, element (current_active_module).grid));
				
			when RELATIVE =>
				cursor.position := type_point (round (cursor.position + position, element (current_active_module).grid));
		end case;

		update_coordinates_display (self);
		self.shift_area (cursor);		
	end move_cursor;

	procedure move_cursor (
		self		: not null access type_view;
		direction	: in type_cursor_direction;
		cursor		: in out type_cursor) is

		-- Get the currently active grid:
		use et_project.modules.pac_generic_modules;
		grid : constant type_grid := element (current_active_module).grid;

		-- Find the grid point nearest available to the current cursor position:
		position_snapped : constant type_point := type_point (round (
							point	=> cursor.position,
							grid	=> grid));

	begin
		case direction is
			when RIGHT =>
				cursor.position := type_point (move (position_snapped, 0.0, grid.x));

			when LEFT =>
				cursor.position := type_point (move (position_snapped, 180.0, grid.x));

			when UP =>
				cursor.position := type_point (move (position_snapped, 90.0, grid.y));

			when DOWN =>
				cursor.position := type_point (move (position_snapped, -90.0, grid.y));
		end case;
		
		update_coordinates_display (self);
		self.shift_area (cursor);
	end move_cursor;

	
	procedure draw_cursor (
		self		: not null access type_view;
		in_area		: in type_rectangle := no_rectangle;
		context 	: in type_draw_context;
		cursor		: in type_cursor)
	is
		lh : type_cursor_line; -- the horizontal line
		lv : type_cursor_line; -- the vertical line

		size : type_distance_positive;
	begin
		size := cursor_half_size / type_distance_positive (self.scale);
		
		-- set start and end point of horizontal line
		lh.start_point := type_point (set (
			x	=> x (cursor.position) - size,
			y	=> y (cursor.position)));

		lh.end_point := type_point (set (
			x	=> x (cursor.position) + size,
			y	=> y (cursor.position)));

		-- set start and end point of vertical line
		lv.start_point := type_point (set (
			x	=> x (cursor.position),
			y	=> y (cursor.position) + size));

		lv.end_point := type_point (set (
			x	=> x (cursor.position),
			y	=> y (cursor.position) - size));


		-- The line width is inversely proportional to the scale:
		cairo.set_line_width (context.cr, type_view_coordinate (cursor_line_width) / self.scale);
		
		set_color_cursor (context.cr);

		pac_draw_misc.draw_line (
			area		=> in_area,
			context		=> context,
			line		=> lh,
			height		=> self.frame_height);

		pac_draw_misc.draw_line (
			area		=> in_area,
			context		=> context,
			line		=> lv,
			height		=> self.frame_height);
		
		cairo.stroke (context.cr);		
	end draw_cursor;

	function get_frame (
		self : not null access type_view)
		return et_frames.type_frame is

		use et_project.modules.pac_generic_modules;
	begin
		return element (current_active_module).frames.frame;
	end get_frame;

	function frame_height (
		self : not null access type_view)
		return type_distance_positive is 

		use et_project.modules.pac_generic_modules;
	begin
		return type_distance_positive (element (current_active_module).frames.frame.size.y);
	end frame_height;

	function frame_width (
		self : not null access type_view)
		return type_distance_positive is 

		use et_project.modules.pac_generic_modules;
	begin
		return type_distance_positive (element (current_active_module).frames.frame.size.x);
	end frame_width;
	
	function title_block_position (
		self : not null access type_view)
		return et_frames.type_position is
	begin
		return self.get_frame.title_block_schematic.position;
	end title_block_position;

	function get_drawing_mode (
		self	: not null access type_view)
		return string 
	is begin
		return to_string (op_mode);
	end get_drawing_mode;
	
	procedure evaluate_key (
		self	: not null access type_view;
		key		: in gdk_key_type) is
	begin
		null;
		put_line ("schematic: evaluating other key ...");

-- 		put_line ("key pressed");
-- 		new_line;
		put_line (gdk_key_type'image (key));

-- 		put_line (gdk_modifier_type'image (key_ctrl));

		self.update_drawing_mode_display;
	end evaluate_key;
	
end et_canvas_schematic;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

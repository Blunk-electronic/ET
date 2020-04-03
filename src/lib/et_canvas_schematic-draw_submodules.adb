------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           DRAW SUBMODULES                                --
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
with cairo;						use cairo;
with pango.layout;				use pango.layout;

with et_text;					use et_text;
with et_general;				use et_general;
with et_project;				use et_project;
with et_coordinates;			use et_coordinates;
use et_coordinates.geometry;

with et_pcb_coordinates;

with et_schematic;
with submodules;				use submodules;
use et_project.type_modules;

separate (et_canvas_schematic)

procedure draw_submodules (
	self	: not null access type_view;
	in_area	: in type_rectangle := no_rectangle;
	context : in type_draw_context) is

	use type_submodules;
	
	procedure query_submods (cursor : in type_submodules.cursor) is

		-- The lower left corner of the submodule box in the schematic:
		submod_position : constant type_point := type_point (element (cursor).position);
		
		procedure draw_box is begin
			cairo.set_line_width (context.cr, type_view_coordinate (submod_box_line_width));
			cairo.set_source_rgb (context.cr, gdouble (1), gdouble (0), gdouble (1)); -- mangenta

			pac_draw_misc.draw_rectangle (
				area			=> in_area,
				context			=> context,
				position		=> submod_position,
				width			=> element (cursor).size.x,
				height			=> element (cursor).size.y,
				frame_height	=> self.drawing.frame_bounding_box.height);
								
			cairo.stroke (context.cr);
		end draw_box;

		-- The position of the instance name is below the lower left corner of the box.
		-- Position and size are fixed and can not be changed by the operator:
		procedure draw_instance_name is
			position : type_point := submod_position;
			offset : constant type_point := type_point (set (
					x => zero,
					y => - text_spacing));
		begin
			move (position, offset);
			
			pac_draw_misc.draw_text (
				area		=> in_area,
				context		=> context,
				content		=> et_text.to_content ("instance: " & to_string (key (cursor))),
				size		=> instance_font_size,
				font		=> instance_font,
				position	=> position,
				origin		=> true,
				rotation	=> zero_rotation,
				alignment	=> (LEFT, TOP),
				height		=> self.drawing.frame_bounding_box.height);

		end draw_instance_name;
		
		-- The position of the file name is below the instance name.
		-- Position and size are fixed and can not be changed by the operator:
		procedure draw_file_name is
			position : type_point := submod_position;
			offset : constant type_point := type_point (set (
					x => zero,
					y => - (2.0 * text_spacing + instance_font_size)));
		begin
			move (position, offset);
			
			pac_draw_misc.draw_text (
				area		=> in_area,
				context		=> context,
				content		=> et_text.to_content ("file: " & to_string (element (cursor).file)),
				size		=> file_font_size,
				font		=> file_font,
				position	=> position,
				origin		=> true,
				rotation	=> zero_rotation,
				alignment	=> (LEFT, TOP),
				height		=> self.drawing.frame_bounding_box.height);

		end draw_file_name;

		-- The position of module in the board is below the file name.
		-- Position and size are fixed and can not be changed by the operator:
		procedure draw_position_in_board is
			position : type_point := submod_position;
			offset : constant type_point := type_point (set (
					x => zero,
					y => - (3.0 * text_spacing + instance_font_size + file_font_size)));

			use et_pcb_coordinates.geometry;
			pos_x : constant string := to_string (x (element (cursor).position_in_board));
			pos_y : constant string := to_string (y (element (cursor).position_in_board));
			rotation : constant string := to_string (rot (element (cursor).position_in_board));
			
			text : constant string := "board (x/y/rot.):" &
					pos_x & et_pcb_coordinates.geometry.axis_separator &
					pos_y & et_pcb_coordinates.geometry.axis_separator &
					rotation;
		begin
			move (position, offset);
			
			pac_draw_misc.draw_text (
				area		=> in_area,
				context		=> context,
				content		=> et_text.to_content (text),
				size		=> position_board_font_size,
				font		=> position_board_font,
				position	=> position,
				origin		=> true,
				rotation	=> et_coordinates.geometry.zero_rotation,
				alignment	=> (LEFT, TOP),
				height		=> self.drawing.frame_bounding_box.height);

		end draw_position_in_board;
		
		procedure draw_ports is 
			use type_submodule_ports;

			procedure draw_port (pc : in type_submodule_ports.cursor) is 

				-- First get the position of the submodule box:
				pos_port : type_point := submod_position;
			begin
				-- CS detect the edge where the port sits at. Depending on the edge
				-- the port must be drawn 0, 90, 180 or 270 degree.
				
				-- Move pos_port by the position of the port. 
				-- The port position is relative to the module (box) position:
				move (pos_port, element (pc).position);

				-- Move pos_port down so that the port sits excatly at
				-- the point where a net will be connected:
				move (pos_port, set (zero, - port_symbol_height / 2.0));
				
				pac_draw_misc.draw_rectangle (
					area			=> in_area,
					context			=> context,
					position		=> pos_port,
					width			=> port_symbol_width,
					height			=> port_symbol_height,
					frame_height	=> self.drawing.frame_bounding_box.height);

				-- CS draw something that indicates the direction (master/slave).
				-- element (pc).direction

				-- CS draw port name
				-- key (pc)
			end draw_port;
			
		begin -- draw_ports
			cairo.set_line_width (context.cr, type_view_coordinate (port_symbol_line_width));
			cairo.set_source_rgb (context.cr, gdouble (1), gdouble (1), gdouble (1)); -- white

			iterate (element (cursor).ports, draw_port'access);
		
			cairo.stroke (context.cr);
		end draw_ports;
			
	begin -- query_submods
		-- We want to draw only those submodules which are on the active sheet:
		if sheet (element (cursor).position) = self.drawing.sheet then

			draw_box;
			draw_ports;
			draw_file_name;
			draw_instance_name;
			draw_position_in_board;

			-- CS view mode ?

		end if;
	end query_submods;
		
begin
-- 	put_line ("draw submodules ...");

	iterate (element (current_active_module).submods, query_submods'access);
	
end draw_submodules;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

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

with et_pcb_coordinates;
with et_schematic;

with et_project.modules;		use et_project.modules;
use et_project.modules.type_modules;

with submodules;				use submodules;

with et_display.schematic;

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
			set_color_submodules (context.cr);

			pac_draw_misc.draw_rectangle (
				area			=> in_area,
				context			=> context,
				position		=> submod_position,
				width			=> element (cursor).size.x,
				height			=> element (cursor).size.y,
				frame_height	=> self.frame_height);
								
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
			move_by (position, offset);
			
			pac_draw_misc.draw_text (
				area		=> in_area,
				context		=> context,
				content		=> et_text.to_content ("instance: " & to_string (key (cursor))),
				size		=> instance_font_size,
				font		=> instance_font,
				position	=> position,
				origin		=> false,
				rotation	=> zero_rotation,
				alignment	=> (LEFT, TOP),
				height		=> self.frame_height);

		end draw_instance_name;
		
		-- The position of the file name is below the instance name.
		-- Position and size are fixed and can not be changed by the operator:
		procedure draw_file_name is
			position : type_point := submod_position;
			offset : constant type_point := type_point (set (
					x => zero,
					y => - (2.0 * text_spacing + instance_font_size)));
		begin
			move_by (position, offset);
			
			pac_draw_misc.draw_text (
				area		=> in_area,
				context		=> context,
				content		=> et_text.to_content ("file: " & to_string (element (cursor).file)),
				size		=> file_font_size,
				font		=> file_font,
				position	=> position,
				origin		=> false,
				rotation	=> zero_rotation,
				alignment	=> (LEFT, TOP),
				height		=> self.frame_height);

		end draw_file_name;

		-- The position of module in the board is below the file name.
		-- Position and size are fixed and can not be changed by the operator:
		procedure draw_position_in_board is
			position : type_point := submod_position;
			offset : constant type_point := type_point (set (
					x => zero,
					y => - (3.0 * text_spacing + instance_font_size + file_font_size)));

			use et_pcb_coordinates.pac_geometry_brd;
			pos_x : constant string := to_string (x (element (cursor).position_in_board));
			pos_y : constant string := to_string (y (element (cursor).position_in_board));
			rotation : constant string := to_string (rot (element (cursor).position_in_board));
			
			text : constant string := "board (x/y/rot.):" &
					pos_x & et_pcb_coordinates.pac_geometry_brd.axis_separator &
					pos_y & et_pcb_coordinates.pac_geometry_brd.axis_separator &
					rotation;
		begin
			move_by (position, offset);
			
			pac_draw_misc.draw_text (
				area		=> in_area,
				context		=> context,
				content		=> et_text.to_content (text),
				size		=> position_board_font_size,
				font		=> position_board_font,
				position	=> position,
				origin		=> false,
				rotation	=> pac_geometry_sch.zero_rotation,
				alignment	=> (LEFT, TOP),
				height		=> self.frame_height);

		end draw_position_in_board;
		
		procedure draw_ports is 
			use type_submodule_ports;

			procedure draw_port (pc : in type_submodule_ports.cursor) is 
				-- First get the position of the submodule box.
				pos : type_point := submod_position;

				procedure draw_horizontal is begin
					-- Draw the port horizontal:
					pac_draw_misc.draw_rectangle (
						area			=> in_area,
						context			=> context,
						position		=> pos,
						width			=> port_symbol_width,
						height			=> port_symbol_height,
						frame_height	=> self.frame_height);
					
				end draw_horizontal;

				procedure draw_vertical is begin
					-- Draw the port vertical:					
					pac_draw_misc.draw_rectangle (
						area			=> in_area,
						context			=> context,
						position		=> pos,
						width			=> port_symbol_height,
						height			=> port_symbol_width,
						frame_height	=> self.frame_height);

				end draw_vertical;
				
			begin -- draw_port
				-- Detect the edge where the port sits at. Depending on the edge
				-- the port must be drawn 0, 90, 180 or 270 degree.

				-- Move pos by the position of the port. 
				-- The port position is relative to the module (box) position:
				move_by (pos, element (pc).position);

				-- According to the edge where the port sits, pos will now be fine
				-- adjusted, because the port is a rectangle which position is at 
				-- its lower left corner.
				
				-- Does the port sit on the LEFT edge of the box ?
				if x (element (pc).position) + x (submod_position) = x (submod_position) then 

					-- Draw the port direction (the letter M or S) inside the port rectangle:
					pac_draw_misc.draw_text 
						(
						area		=> in_area,
						context		=> context,
						content		=> to_content (to_direction_abbrevation (element (pc).direction)),
						size		=> port_direction_font_size,
						font		=> port_direction_font,
						position	=> type_point (move (pos, 0.0, port_symbol_width / 2.0)),
						origin		=> false, -- no origin required
						rotation	=> zero_rotation,
						alignment	=> (CENTER, CENTER),
						height		=> self.frame_height);
					
					-- Draw the port name. The text is placed on the RIGHT of the port rectangle:
					pac_draw_misc.draw_text 
						(
						area		=> in_area,
						context		=> context,
						content		=> to_content (to_string (key (pc))),
						size		=> port_name_font_size,
						font		=> port_name_font,
						position	=> type_point (move (pos, 0.0, port_symbol_width + port_name_spacing)),
						origin		=> false, -- no origin required
						rotation	=> zero_rotation,
						alignment	=> (LEFT, CENTER),
						height		=> self.frame_height);
					
					-- Move pos down so that the port sits excatly at
					-- the point where a net will be connected:
					move_by (pos, set (x => zero, y => - port_symbol_height / 2.0));

					draw_horizontal;
					
				-- Does the port sit on the RIGHT edge of the box ?
				elsif x (element (pc).position) + x (submod_position) = x (submod_position) + element (cursor).size.x then 

					-- Draw the port direction (the letter M or S) inside the port rectangle:
					pac_draw_misc.draw_text 
						(
						area		=> in_area,
						context		=> context,
						content		=> to_content (to_direction_abbrevation (element (pc).direction)),
						size		=> port_direction_font_size,
						font		=> port_direction_font,
						position	=> type_point (move (pos, 180.0, port_symbol_width / 2.0)),
						origin		=> false, -- no origin required
						rotation	=> zero_rotation,
						alignment	=> (CENTER, CENTER),
						height		=> self.frame_height);
					
					-- Draw the port name. The text is placed on the LEFT of the port rectangle:
					pac_draw_misc.draw_text 
						(
						area		=> in_area,
						context		=> context,
						content		=> to_content (to_string (key (pc))),
						size		=> port_name_font_size,
						font		=> port_name_font,
						position	=> type_point (move (pos, 180.0, port_symbol_width + port_name_spacing)),
						origin		=> false, -- no origin required
						rotation	=> zero_rotation,
						alignment	=> (RIGHT, CENTER),
						height		=> self.frame_height);
					
					-- Move pos down and left so that the port sits excatly at
					-- the point where a net will be connected:
					move_by (pos, set (x => - port_symbol_width, y => - port_symbol_height / 2.0));

					draw_horizontal;

				-- Does the port sit on the LOWER edge of the box ?
				elsif y (element (pc).position) + y (submod_position) = y (submod_position) then

					-- Draw the port direction (the letter M or S) inside the port rectangle:
					pac_draw_misc.draw_text 
						(
						area		=> in_area,
						context		=> context,
						content		=> to_content (to_direction_abbrevation (element (pc).direction)),
						size		=> port_direction_font_size,
						font		=> port_direction_font,
						position	=> type_point (move (pos, 90.0, port_symbol_width / 2.0)),
						origin		=> false, -- no origin required
						rotation	=> zero_rotation,
						alignment	=> (CENTER, CENTER),
						height		=> self.frame_height);
					
					-- Draw the port name. The text is placed ABOVE the port rectangle:
					pac_draw_misc.draw_text 
						(
						area		=> in_area,
						context		=> context,
						content		=> to_content (to_string (key (pc))),
						size		=> port_name_font_size,
						font		=> port_name_font,
						position	=> type_point (move (pos, 90.0, port_symbol_width + port_name_spacing)),
						origin		=> false, -- no origin required
						rotation	=> 90.0,
						alignment	=> (LEFT, CENTER),
						height		=> self.frame_height);
					
					-- Move pos left so that the port sits excatly at
					-- the point where a net will be connected:
					move_by (pos, set (x => - port_symbol_height / 2.0, y => zero));

					draw_vertical;

				-- Does the port sit on the UPPER edge of the box ?
				elsif y (element (pc).position) + y (submod_position) = y (submod_position) + element (cursor).size.y then 

					-- Draw the port direction (the letter M or S) inside the port rectangle:
					pac_draw_misc.draw_text 
						(
						area		=> in_area,
						context		=> context,
						content		=> to_content (to_direction_abbrevation (element (pc).direction)),
						size		=> port_direction_font_size,
						font		=> port_direction_font,
						position	=> type_point (move (pos, 270.0, port_symbol_width / 2.0)),
						origin		=> false, -- no origin required
						rotation	=> zero_rotation,
						alignment	=> (CENTER, CENTER),
						height		=> self.frame_height);
					
					-- Draw the port name. The text is placed BELOW the port rectangle:
					pac_draw_misc.draw_text 
						(
						area		=> in_area,
						context		=> context,
						content		=> to_content (to_string (key (pc))),
						size		=> port_name_font_size,
						font		=> port_name_font,
						position	=> type_point (move (pos, 270.0, port_symbol_width + port_name_spacing)),
						origin		=> false, -- no origin required
						rotation	=> 90.0,
						alignment	=> (RIGHT, CENTER),
						height		=> self.frame_height);
					
					-- Move pos up and left so that the port sits excatly at
					-- the point where a net will be connected:
					move_by (pos, set (x => - port_symbol_height / 2.0, y => - port_symbol_width));

					draw_vertical;
					
				else
					-- port does not sit on any edge
					raise constraint_error; -- CS should never happen
				end if;

			end draw_port;
			
		begin -- draw_ports
			cairo.set_line_width (context.cr, type_view_coordinate (port_symbol_line_width));

			iterate (element (cursor).ports, draw_port'access);
		
			cairo.stroke (context.cr);
		end draw_ports;

		use et_display.schematic;
		
	begin -- query_submods
		-- We want to draw only those submodules which are on the active sheet:
		if sheet (element (cursor).position) = current_active_sheet then

			draw_box;
			draw_ports;

			-- Draw file and instance name and position in board if
			-- layer device_names is enabled:
			if device_names_enabled then

				set_color_placeholders (context.cr);
				
				draw_file_name;
				draw_instance_name;
				draw_position_in_board;
			end if;

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
